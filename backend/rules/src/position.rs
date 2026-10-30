//! Positions, stones, and exact placement rules, independent of turn order.
//! Placements return a new position or `None`, leaving the original unchanged.
//! Liberties use exact integer row intervals; `prepare` reuses group analysis
//! and liberty witnesses across candidate placements.

use crate::Board;
use std::time::Instant;

mod liberties;
mod prepared;
mod profile;
pub(crate) use prepared::PreparedGroups;
pub use prepared::PreparedPosition;
pub use profile::PlayProfile;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Spot {
    pub x: i32,
    pub y: i32,
}

impl Spot {
    pub(crate) fn distance_squared(self, other: Self) -> i64 {
        let dx = i64::from(self.x) - i64::from(other.x);
        let dy = i64::from(self.y) - i64::from(other.y);
        dx * dx + dy * dy
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn other(self) -> Self {
        match self {
            Self::Black => Self::White,
            Self::White => Self::Black,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Stone {
    pub color: Color,
    pub spot: Spot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Play {
    Place(Stone),
    Pass(Color),
}

impl Play {
    pub fn color(self) -> Color {
        match self {
            Self::Place(stone) => stone.color,
            Self::Pass(color) => color,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Position {
    board: Board,
    stones: Vec<Stone>,
}

impl Position {
    pub fn new(board: Board) -> Self {
        Self {
            board,
            stones: Vec::new(),
        }
    }

    pub fn board(&self) -> Board {
        self.board
    }

    pub fn stones(&self) -> &[Stone] {
        &self.stones
    }

    pub fn prepare(&self) -> PreparedPosition<'_> {
        PreparedPosition {
            position: self,
            groups: PreparedGroups::new(self),
        }
    }

    pub fn play(&self, stone: Stone) -> Option<Self> {
        self.play_inner::<false>(stone, &mut PlayProfile::default(), Self::has_liberty)
    }

    pub fn play_profiled(&self, stone: Stone, profile: &mut PlayProfile) -> Option<Self> {
        self.play_inner::<true>(stone, profile, Self::has_liberty)
    }

    /// Original exhaustive liberty search for differential tests and benchmarks.
    #[cfg(any(test, feature = "reference"))]
    pub fn play_reference(&self, stone: Stone) -> Option<Self> {
        self.play_inner::<false>(
            stone,
            &mut PlayProfile::default(),
            Self::has_liberty_reference,
        )
    }

    #[cfg(any(test, feature = "reference"))]
    pub fn play_reference_profiled(&self, stone: Stone, profile: &mut PlayProfile) -> Option<Self> {
        self.play_inner::<true>(stone, profile, Self::has_liberty_reference)
    }

    fn play_inner<const PROFILE: bool>(
        &self,
        stone: Stone,
        profile: &mut PlayProfile,
        has_liberty: impl Fn(&Self, Spot) -> bool + Copy,
    ) -> Option<Self> {
        let total_started = PROFILE.then(Instant::now);
        let started = PROFILE.then(Instant::now);
        let possible = self.can_place(stone.spot);
        if let Some(started) = started {
            profile.calls += 1;
            profile.geometry_seconds += started.elapsed().as_secs_f64();
        }
        let result = if possible {
            let started = PROFILE.then(Instant::now);
            let mut next = self.clone();
            next.stones.push(stone);
            if let Some(started) = started {
                profile.cloning_seconds += started.elapsed().as_secs_f64();
            }
            next.capture_groups_without_liberties::<PROFILE>(
                stone.color.other(),
                profile,
                has_liberty,
            );
            next.capture_groups_without_liberties::<PROFILE>(stone.color, profile, has_liberty);
            next.stones.contains(&stone).then_some(next)
        } else {
            None
        };
        if let Some(started) = total_started {
            profile.total_seconds += started.elapsed().as_secs_f64();
        }
        result
    }

    /// Check board bounds and overlap only, not captures or suicide.
    pub fn can_place(&self, spot: Spot) -> bool {
        self.board.contains(spot)
            && self
                .stones
                .iter()
                .all(|stone| !self.board.overlaps(spot, stone.spot))
    }

    /// Same-color connected groups, expressed as indices into `stones()`.
    pub fn groups(&self, color: Color) -> Vec<Vec<usize>> {
        let mut groups = Vec::new();
        let mut seen = vec![false; self.stones.len()];

        for start in 0..self.stones.len() {
            if seen[start] || self.stones[start].color != color {
                continue;
            }

            let mut group = vec![start];
            seen[start] = true;
            let mut cursor = 0;
            while cursor < group.len() {
                let spot = self.stones[group[cursor]].spot;
                for (index, stone) in self.stones.iter().enumerate() {
                    if !seen[index] && stone.color == color && self.board.adjacent(spot, stone.spot)
                    {
                        seen[index] = true;
                        group.push(index);
                    }
                }
                cursor += 1;
            }
            groups.push(group);
        }
        groups
    }

    fn capture_groups_without_liberties<const PROFILE: bool>(
        &mut self,
        color: Color,
        profile: &mut PlayProfile,
        has_liberty: impl Fn(&Self, Spot) -> bool,
    ) {
        let started = PROFILE.then(Instant::now);
        let groups = self.groups(color);
        if let Some(started) = started {
            profile.group_builds += 1;
            profile.groups_seconds += started.elapsed().as_secs_f64();
        }
        let started = PROFILE.then(Instant::now);
        let mut dead = Vec::new();
        for group in groups {
            if !group.iter().any(|&index| {
                if PROFILE {
                    profile.liberty_queries += 1;
                }
                has_liberty(self, self.stones[index].spot)
            }) {
                for index in group {
                    dead.push(self.stones[index]);
                }
            }
        }
        if let Some(started) = started {
            profile.liberties_seconds += started.elapsed().as_secs_f64();
        }
        let started = PROFILE.then(Instant::now);
        // Determine all dead groups before removing any: captures are simultaneous.
        self.stones.retain(|stone| !dead.contains(stone));
        if let Some(started) = started {
            profile.removal_seconds += started.elapsed().as_secs_f64();
        }
    }
}

#[cfg(test)]
mod prepared_tests;

#[cfg(test)]
mod tests {
    use super::*;

    fn stone(color: Color, x: i32, y: i32) -> Stone {
        Stone {
            color,
            spot: Spot { x, y },
        }
    }

    #[test]
    fn placements_leave_the_original_unchanged_and_reject_overlap() {
        let empty = Position::default();
        let first = empty.play(stone(Color::Black, 200, 200)).unwrap();
        assert!(empty.stones().is_empty());
        assert_eq!(first.stones(), &[stone(Color::Black, 200, 200)]);
        assert!(first.play(stone(Color::White, 200, 200)).is_none());
        assert!(first.play(stone(Color::White, 275, 200)).is_none());
        assert!(first.play(stone(Color::White, 276, 200)).is_some());
        assert!(first.play(stone(Color::White, 0, 200)).is_none());
    }

    #[test]
    fn groups_follow_only_same_color_adjacency() {
        let position = Position {
            stones: vec![
                stone(Color::Black, 200, 200),
                stone(Color::Black, 276, 200),
                stone(Color::White, 352, 200),
                stone(Color::Black, 500, 500),
            ],
            ..Position::default()
        };
        assert_eq!(position.groups(Color::Black), vec![vec![0, 1], vec![3]]);
        assert_eq!(position.groups(Color::White), vec![vec![2]]);
    }

    #[test]
    fn liberty_search_checks_beyond_the_four_quick_probes() {
        let position = Position {
            stones: vec![
                stone(Color::Black, 500, 500),
                stone(Color::White, 349, 500),
                stone(Color::White, 651, 500),
                stone(Color::White, 500, 349),
                stone(Color::White, 500, 651),
            ],
            ..Position::default()
        };
        assert!(position.has_liberty(Spot { x: 500, y: 500 }));
    }

    #[test]
    fn an_isolated_touching_spot_is_a_liberty() {
        let position = Position {
            stones: vec![
                stone(Color::Black, 38, 38),
                stone(Color::White, 190, 38),
                stone(Color::White, 38, 114),
                stone(Color::White, 114, 114),
            ],
            ..Position::default()
        };
        assert!(position.has_liberty(Spot { x: 38, y: 38 }));
    }
}
