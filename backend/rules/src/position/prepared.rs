//! Immutable per-position groups and exact liberty witnesses. A surviving
//! witness proves a group's liberty without searching. Keep up to two separated
//! witnesses per group; search again only when both are blocked. Enemy captures
//! precede friendly suicide, both simultaneous.

use super::{Color, Position, Spot, Stone};

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
}

impl PreparedPosition<'_> {
    pub fn play(&self, stone: Stone) -> Option<Position> {
        self.groups.play(self.position, stone)
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
