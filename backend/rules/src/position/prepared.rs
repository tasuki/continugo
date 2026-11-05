//! Immutable per-position groups and exact liberty witnesses. A surviving
//! witness proves a group's liberty without searching. Keep up to two separated
//! witnesses per group; search again only when both are blocked. Enemy captures
//! precede friendly suicide, both simultaneous.

use super::liberties::RowBlockers;
use super::{Color, LibertySpan, Position, Spot, Stone};
use std::{sync::OnceLock, time::Instant};

struct Group {
    color: Color,
    members: Vec<usize>,
    liberties: Vec<Spot>,
}

pub(crate) struct PreparedGroups {
    groups: Vec<Group>,
}

/// Candidate validation tied to one immutable position, for either color.
/// Dropping this view discards all cached analysis; no candidate mutates it.
pub struct PreparedPosition<'a> {
    pub(super) position: &'a Position,
    pub(crate) groups: PreparedGroups,
    pub(super) blockers: OnceLock<RowBlockers>,
}

/// Rule-derived facts about a legal placement. `liberties` is min(actual, cap)
/// for the resulting merged group, not the number of validation witnesses.
#[derive(Debug)]
pub struct MoveAnalysis {
    pub position: Position,
    pub connected_groups: usize,
    pub connected_stones: usize,
    pub captured_groups: usize,
    pub captured_stones: usize,
    pub group_size: usize,
    pub liberties: usize,
}

/// Geometry plus an exact one-placement capture witness for a complete group.
/// The attacker is the group's opposite color, regardless of the actual turn.
#[derive(Debug)]
pub struct GroupAnalysis {
    pub liberty_spans: Vec<LibertySpan>,
    pub capture_in_one: Option<Spot>,
}

/// Disjoint diagnostic durations; ordinary group analysis reads no clocks.
/// The first span measurement includes lazy blocker-row construction.
#[derive(Debug, Default)]
pub struct GroupAnalysisProfile {
    pub liberty_spans_seconds: f64,
    pub capturability_seconds: f64,
}

impl PreparedPosition<'_> {
    pub fn play(&self, stone: Stone) -> Option<Position> {
        self.groups.play(self.position, stone)
    }

    /// Borrow the groups already constructed for candidate validation.
    pub fn groups(&self, color: Color) -> impl Iterator<Item = &[usize]> {
        self.groups
            .groups
            .iter()
            .filter(move |group| group.color == color)
            .map(|group| group.members.as_slice())
    }

    /// Members must be a complete group from this position's or this view's `groups` output.
    /// This is unchanged-board capturability, not a forced capture after replies.
    pub fn analyze_group(&self, members: &[usize]) -> GroupAnalysis {
        self.analyze_group_inner::<false>(members).0
    }

    /// Same analysis, with separate span and validated capture-search clocks.
    pub fn analyze_group_profiled(
        &self,
        members: &[usize],
    ) -> (GroupAnalysis, GroupAnalysisProfile) {
        self.analyze_group_inner::<true>(members)
    }

    fn analyze_group_inner<const PROFILE: bool>(
        &self,
        members: &[usize],
    ) -> (GroupAnalysis, GroupAnalysisProfile) {
        let mut profile = GroupAnalysisProfile::default();
        let tick = PROFILE.then(Instant::now);
        let blockers = (!members.is_empty()).then(|| {
            self.blockers
                .get_or_init(|| RowBlockers::new(self.position))
        });
        let liberty_spans = self.position.liberty_spans_with_blockers(members, blockers);
        if let Some(tick) = tick {
            profile.liberty_spans_seconds = tick.elapsed().as_secs_f64();
        }
        let tick = PROFILE.then(Instant::now);
        let mut capture_in_one = None;
        if let Some(&first) = members.first() {
            let attacker = self.position.stones[first].color.other();
            self.position
                .visit_liberty_covers_with_blockers(&liberty_spans, blockers, |spot| {
                    if let Some(next) = self.play(Stone {
                        color: attacker,
                        spot,
                    }) && members
                        .iter()
                        .all(|&i| !next.stones.contains(&self.position.stones[i]))
                    {
                        capture_in_one = Some(spot);
                        return false;
                    }
                    true
                });
        }
        if let Some(tick) = tick {
            profile.capturability_seconds = tick.elapsed().as_secs_f64();
        }
        (
            GroupAnalysis {
                liberty_spans,
                capture_in_one,
            },
            profile,
        )
    }

    /// Uses the same validator as `play`; optional analysis never changes rules.
    pub fn analyze_move(&self, stone: Stone, liberty_cap: usize) -> Option<MoveAnalysis> {
        let next = self.play(stone)?;
        let source = self.position;
        let joined: Vec<_> = self
            .groups
            .groups
            .iter()
            .filter(|group| {
                group.color == stone.color
                    && group
                        .members
                        .iter()
                        .any(|&i| source.board.adjacent(source.stones[i].spot, stone.spot))
            })
            .collect();
        let connected_stones = joined.iter().map(|g| g.members.len()).sum::<usize>();
        let enemy_count = |position: &Position| {
            position
                .stones
                .iter()
                .filter(|s| s.color != stone.color)
                .count()
        };
        let captured_stones = enemy_count(source) - enemy_count(&next);
        let captured_groups = if captured_stones == 0 {
            0
        } else {
            self.groups
                .groups
                .iter()
                .filter(|g| {
                    g.color != stone.color && !next.stones.contains(&source.stones[g.members[0]])
                })
                .count()
        };
        let mut witnesses = Vec::new();
        for group in &joined {
            for &liberty in &group.liberties {
                if !source.board.overlaps(liberty, stone.spot) && !witnesses.contains(&liberty) {
                    witnesses.push(liberty);
                }
            }
        }
        let spots = std::iter::once(stone.spot).chain(
            joined
                .iter()
                .flat_map(|g| g.members.iter().map(|&i| source.stones[i].spot)),
        );
        let liberties = next.collect_liberties(spots, liberty_cap, witnesses).len();
        Some(MoveAnalysis {
            position: next,
            connected_groups: joined.len(),
            connected_stones,
            captured_groups,
            captured_stones,
            group_size: connected_stones + 1,
            liberties,
        })
    }
}

impl PreparedGroups {
    pub(crate) fn new(position: &Position) -> Self {
        let groups = [Color::Black, Color::White]
            .into_iter()
            .flat_map(|color| {
                position.groups(color).into_iter().map(move |members| {
                    let mut liberties: Vec<Spot> = Vec::with_capacity(2);
                    let mut separation = 0;
                    for &i in &members {
                        if let Some(liberty) = position.find_liberty(position.stones[i].spot) {
                            if let Some(&first) = liberties.first() {
                                let distance = first.distance_squared(liberty);
                                if distance > separation {
                                    liberties.truncate(1);
                                    liberties.push(liberty);
                                    separation = distance;
                                }
                                // One placement cannot cover witnesses this far apart.
                                if separation >= 4 * i64::from(position.board.diameter()).pow(2) {
                                    break;
                                }
                            } else {
                                liberties.push(liberty);
                            }
                        }
                    }
                    Group {
                        color,
                        members,
                        liberties,
                    }
                })
            })
            .collect();
        Self { groups }
    }

    pub(crate) fn play(&self, position: &Position, stone: Stone) -> Option<Position> {
        if !position.can_place(stone.spot) {
            return None;
        }
        let board = position.board;
        let witness_survives = |group: &Group| {
            group
                .liberties
                .iter()
                .any(|&liberty| !board.overlaps(liberty, stone.spot))
        };
        let has_liberty = |next: &Position, group: &Group| {
            witness_survives(group)
                || group
                    .members
                    .iter()
                    .any(|&i| next.has_liberty(position.stones[i].spot))
        };
        let mut next = position.clone();
        next.stones.push(stone);
        let mut dead = vec![false; next.stones.len()];
        for group in &self.groups {
            if group.color != stone.color && !has_liberty(&next, group) {
                for &i in &group.members {
                    dead[i] = true;
                }
            }
        }
        let mut index = 0;
        next.stones.retain(|_| {
            let keep = !dead[index];
            index += 1;
            keep
        });

        // The new stone joins exactly those old friendly groups adjacent to it.
        // Other friendly groups remain separate, even when enemies are removed.
        let joins = |group: &Group| {
            group.color == stone.color
                && group
                    .members
                    .iter()
                    .any(|&i| board.adjacent(position.stones[i].spot, stone.spot))
        };
        let joined_alive = self
            .groups
            .iter()
            .any(|group| joins(group) && witness_survives(group))
            || next.has_liberty(stone.spot)
            || self
                .groups
                .iter()
                .any(|group| joins(group) && has_liberty(&next, group));
        if !joined_alive {
            return None;
        }
        for group in &self.groups {
            if group.color == stone.color && !joins(group) && !has_liberty(&next, group) {
                for &i in &group.members {
                    dead[i] = true;
                }
            }
        }
        // Keep original stone order, just as the uncached validator does.
        let mut index = 0;
        next.stones = position
            .stones
            .iter()
            .filter_map(|&old| {
                let keep = !dead[index];
                index += 1;
                keep.then_some(old)
            })
            .chain([stone])
            .collect();
        Some(next)
    }
}
