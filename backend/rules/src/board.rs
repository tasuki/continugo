//! Board sizes share a 1000-unit coordinate space. Stone radius is derived by
//! integer division. Sizes must be positive and leave a radius of at least one;
//! the 13x13 equivalent is the default in both languages.

use crate::{COORD_RANGE, Spot};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Board {
    size: i32,
}

impl Default for Board {
    fn default() -> Self {
        Self { size: 13 }
    }
}

impl Board {
    pub fn new(size: i32) -> Result<Self, String> {
        if !(1..=COORD_RANGE / 2).contains(&size) {
            return Err(format!(
                "Board size must be between 1 and {}, got: {size}",
                COORD_RANGE / 2
            ));
        }
        Ok(Self { size })
    }

    pub const fn size(self) -> i32 {
        self.size
    }

    pub const fn stone_radius(self) -> i32 {
        COORD_RANGE / (2 * self.size)
    }

    pub const fn diameter(self) -> i32 {
        2 * self.stone_radius()
    }

    pub fn contains(self, spot: Spot) -> bool {
        let range = self.stone_radius()..=COORD_RANGE - self.stone_radius();
        range.contains(&spot.x) && range.contains(&spot.y)
    }

    pub fn overlaps(self, a: Spot, b: Spot) -> bool {
        a.distance_squared(b) < i64::from(self.diameter()).pow(2)
    }

    pub fn adjacent(self, a: Spot, b: Spot) -> bool {
        a.distance_squared(b) < 2 * i64::from(self.diameter()).pow(2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sizes_derive_radii_and_keep_geometry_boundaries_exact() {
        assert_eq!(Board::default().size(), 13);
        for (size, radius) in [
            (1, 500),
            (7, 71),
            (9, 55),
            (11, 45),
            (13, 38),
            (19, 26),
            (37, 13),
            (500, 1),
        ] {
            let board = Board::new(size).unwrap();
            assert_eq!(board.size(), size);
            assert_eq!(board.stone_radius(), radius);
            let d = board.diameter();
            let origin = Spot { x: 0, y: 0 };
            assert!(board.overlaps(origin, Spot { x: d - 1, y: 0 }));
            assert!(!board.overlaps(origin, Spot { x: d, y: 0 }));
            assert!(board.adjacent(origin, Spot { x: d - 1, y: d }));
            assert!(!board.adjacent(origin, Spot { x: d, y: d }));
            assert!(board.contains(Spot {
                x: radius,
                y: COORD_RANGE - radius
            }));
            assert!(!board.contains(Spot {
                x: radius - 1,
                y: radius
            }));
            assert!(!board.contains(Spot {
                x: radius,
                y: COORD_RANGE - radius + 1
            }));
        }
    }

    #[test]
    fn invalid_sizes_cannot_construct_boards() {
        for size in [i32::MIN, -1, 0, 501, i32::MAX] {
            assert!(Board::new(size).is_err(), "Accepted {size}");
        }
    }
}
