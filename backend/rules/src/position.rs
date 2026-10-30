//! Positions, stones, and exact placement rules, independent of turn order.
//! Placements return a new position or `None`, leaving the original unchanged.
//! Groups and liberties are recomputed directly, without neighbour caches.

use crate::{Board, COORD_RANGE};

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

    pub fn play(&self, stone: Stone) -> Option<Self> {
        if !self.can_place(stone.spot) {
            return None;
        }

        let mut next = self.clone();
        next.stones.push(stone);
        next.capture_groups_without_liberties(stone.color.other());
        next.capture_groups_without_liberties(stone.color);

        if next.stones.contains(&stone) {
            Some(next)
        } else {
            None
        }
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

    fn capture_groups_without_liberties(&mut self, color: Color) {
        let mut dead = Vec::new();
        for group in self.groups(color) {
            if !group
                .iter()
                .any(|&index| self.has_liberty(self.stones[index].spot))
            {
                for index in group {
                    dead.push(self.stones[index]);
                }
            }
        }
        // Determine all dead groups before removing any: captures are simultaneous.
        self.stones.retain(|stone| !dead.contains(stone));
    }

    fn has_liberty(&self, spot: Spot) -> bool {
        let radius = self.board.stone_radius();
        let diameter = self.board.diameter();
        let diameter_squared = i64::from(diameter).pow(2);
        for (dx, dy) in [(-diameter, 0), (0, -diameter), (0, diameter), (diameter, 0)] {
            if self.can_place(Spot {
                x: spot.x + dx,
                y: spot.y + dy,
            }) {
                return true;
            }
        }

        // Search the integer adjacency ring exactly, including touching spots.
        let reach = 2 * diameter;
        let left = (spot.x - reach).max(radius);
        let right = (spot.x + reach).min(COORD_RANGE - radius);
        let top = (spot.y - reach).max(radius);
        let bottom = (spot.y + reach).min(COORD_RANGE - radius);
        for y in top..=bottom {
            for x in left..=right {
                let candidate = Spot { x, y };
                let distance = spot.distance_squared(candidate);
                if (diameter_squared..2 * diameter_squared).contains(&distance)
                    && self.can_place(candidate)
                {
                    return true;
                }
            }
        }
        false
    }
}

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
