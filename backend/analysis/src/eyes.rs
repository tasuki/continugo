//! Small eyes are pockets of geometrically available integer stone centres.
//! We conservatively connect all eight neighbouring coordinates: a pocket is
//! one-stone-sized only if the proposed stone overlaps every point in it.
//! Board edges may bound an eye; its stone boundary must be one friendly group.
//! This is a geometric query, not a life-and-death solver or a legality rule.

use crate::GeometricAvailability;
use continugo_rules::{Board, COORD_RANGE, Color, Position, Spot};
use std::collections::{HashMap, HashSet};

#[cfg(any(test, feature = "reference"))]
mod reference;
#[cfg(any(test, feature = "reference"))]
pub use reference::fills_small_eye_reference;

/// Would a stone here completely fill a small eye of one friendly group?
/// Larger pockets, enemy boundaries and connections between groups return false.
/// Callers still check legality and may choose to allow captures unconditionally.
pub fn fills_small_eye(position: &Position, color: Color, spot: Spot) -> bool {
    SmallEyeAnalysis::new(position).fills_small_eye(color, spot)
}

/// Lazy pocket analysis for one immutable position; safe to query for both colors.
/// Only complete components are cached. An early escape from a proposed stone's
/// disc leaves no partial labels. Thus open space stays cheap, while rejected eye
/// fillings share their flood fill and boundary ownership with later candidates.
/// Drop this analysis after a placement, including captures.
pub struct SmallEyeAnalysis<'a> {
    position: &'a Position,
    availability: Option<&'a GeometricAvailability>,
    pocket_at: HashMap<i32, usize>,
    pockets: Vec<Pocket>,
    groups: Option<Vec<usize>>,
}

struct Row {
    y: i32,
    left: i32,
    right: i32,
}

struct Pocket {
    color: Color,
    owner: usize,
    rows: Vec<Row>,
}

impl Pocket {
    fn is_filled(&self, board: Board, spot: Spot) -> bool {
        // On each row, the farthest point is an endpoint, even for a nonconvex
        // component. Checking these actual extrema is exact, not a bounding-box
        // approximation; strict touching points must still count as unfilled.
        self.rows.iter().all(|row| {
            board.overlaps(
                spot,
                Spot {
                    x: row.left,
                    y: row.y,
                },
            ) && board.overlaps(
                spot,
                Spot {
                    x: row.right,
                    y: row.y,
                },
            )
        })
    }
}

impl<'a> SmallEyeAnalysis<'a> {
    pub fn new(position: &'a Position) -> Self {
        Self {
            position,
            availability: None,
            pocket_at: HashMap::new(),
            pockets: Vec::new(),
            groups: None,
        }
    }

    /// Reuse a geometric index synchronized to this position. A mismatch panics.
    pub fn with_availability(
        position: &'a Position,
        availability: &'a GeometricAvailability,
    ) -> Self {
        assert!(
            availability.matches(position),
            "availability must match the analyzed position"
        );
        Self {
            availability: Some(availability),
            ..Self::new(position)
        }
    }

    fn can_place(&self, spot: Spot) -> bool {
        self.availability.map_or_else(
            || self.position.can_place(spot),
            |index| index.is_available(spot),
        )
    }

    pub fn fills_small_eye(&mut self, color: Color, spot: Spot) -> bool {
        if !self.can_place(spot) {
            return false;
        }
        if let Some(&id) = self.pocket_at.get(&coordinate_id(spot)) {
            return self.cached_eye(id, color, spot);
        }
        let board = self.position.board();
        let stones = self.position.stones();
        if !stones
            .iter()
            .any(|stone| stone.color == color && board.adjacent(spot, stone.spot))
        {
            return false;
        }
        let mut seen = HashSet::from([coordinate_id(spot)]);
        let mut pending = vec![spot];
        let mut points = vec![spot];
        let mut boundary = vec![false; stones.len()];
        while let Some(current) = pending.pop() {
            for dy in -1..=1 {
                for dx in -1..=1 {
                    let next = Spot {
                        x: current.x + dx,
                        y: current.y + dy,
                    };
                    if !board.contains(next) || !seen.insert(coordinate_id(next)) {
                        continue;
                    }
                    if self.can_place(next) {
                        if !board.overlaps(spot, next) {
                            return false;
                        }
                        points.push(next);
                        pending.push(next);
                    } else {
                        for (i, stone) in stones.iter().enumerate() {
                            if board.overlaps(next, stone.spot) {
                                if stone.color != color {
                                    return false;
                                }
                                boundary[i] = true;
                            }
                        }
                    }
                }
            }
        }
        let Some(first) = boundary.iter().position(|&touches| touches) else {
            return false;
        };
        let groups = self.groups.get_or_insert_with(|| {
            let mut labels = vec![usize::MAX; stones.len()];
            let mut id = 0;
            for color in [Color::Black, Color::White] {
                for group in self.position.groups(color) {
                    for index in group {
                        labels[index] = id;
                    }
                    id += 1;
                }
            }
            labels
        });
        let owner = groups[first];
        if boundary
            .iter()
            .enumerate()
            .any(|(i, &touches)| touches && groups[i] != owner)
        {
            return false;
        }

        let top = points.iter().map(|p| p.y).min().unwrap();
        let bottom = points.iter().map(|p| p.y).max().unwrap();
        let mut rows: Vec<_> = (top..=bottom)
            .map(|y| Row {
                y,
                left: i32::MAX,
                right: i32::MIN,
            })
            .collect();
        let id = self.pockets.len();
        for point in points {
            self.pocket_at.insert(coordinate_id(point), id);
            let row = &mut rows[(point.y - top) as usize];
            row.left = row.left.min(point.x);
            row.right = row.right.max(point.x);
        }
        self.pockets.push(Pocket { color, owner, rows });
        self.cached_eye(id, color, spot)
    }

    fn cached_eye(&self, id: usize, color: Color, spot: Spot) -> bool {
        let pocket = &self.pockets[id];
        let board = self.position.board();
        if pocket.color != color || !pocket.is_filled(board, spot) {
            return false;
        }
        let groups = self.groups.as_ref().expect("cached boundary groups");
        let mut adjacent = false;
        for (i, stone) in self.position.stones().iter().enumerate() {
            if stone.color == color && board.adjacent(spot, stone.spot) {
                if groups[i] != pocket.owner {
                    return false;
                }
                adjacent = true;
            }
        }
        adjacent
    }
}

fn coordinate_id(spot: Spot) -> i32 {
    spot.y * COORD_RANGE + spot.x
}

#[cfg(test)]
mod tests {
    use super::*;
    use continugo_rules::{Board, Stone};

    fn position(size: i32, stones: impl IntoIterator<Item = Stone>) -> Position {
        let mut position = Position::new(Board::new(size).unwrap());
        for stone in stones {
            position = position.play(stone).unwrap();
        }
        position
    }

    fn corners(color: Color) -> Position {
        position(
            3,
            [
                (498, 166),
                (830, 166),
                (166, 498),
                (498, 498),
                (830, 498),
                (166, 830),
                (498, 830),
            ]
            .map(|(x, y)| Stone {
                color,
                spot: Spot { x, y },
            }),
        )
    }

    #[test]
    fn repeated_candidates_share_a_complete_pocket_but_not_its_color() {
        let position = corners(Color::Black);
        let availability = GeometricAvailability::new(&position);
        let mut eyes = SmallEyeAnalysis::with_availability(&position, &availability);
        for action in availability.coordinates() {
            let id = action.index() as i32;
            let spot = Spot {
                x: id % COORD_RANGE,
                y: id / COORD_RANGE,
            };
            for color in [Color::Black, Color::White] {
                assert_eq!(
                    eyes.fills_small_eye(color, spot),
                    fills_small_eye_reference(&position, color, spot)
                );
            }
        }
        assert_eq!(eyes.pockets.len(), 2);
        assert_eq!(eyes.pocket_at.len(), availability.coordinates().len());
        assert!(eyes.fills_small_eye(Color::Black, Spot { x: 834, y: 834 }));
        assert_eq!(eyes.pockets.len(), 2);
    }

    #[test]
    fn escaping_searches_leave_no_partial_labels_and_filling_depends_on_the_candidate() {
        let original = corners(Color::Black);
        let position = position(
            3,
            original
                .stones()
                .iter()
                .copied()
                .filter(|stone| stone.spot != Spot { x: 498, y: 166 }),
        );
        let availability = GeometricAvailability::new(&position);
        let mut eyes = SmallEyeAnalysis::with_availability(&position, &availability);
        let edge = Spot { x: 166, y: 166 };
        assert!(!eyes.fills_small_eye(Color::Black, edge));
        assert!(eyes.pocket_at.is_empty());
        assert!(eyes.fills_small_eye(Color::Black, Spot { x: 332, y: 166 }));
        assert_eq!(eyes.pockets.len(), 1);
        // The other endpoint is exactly one diameter away and stays available.
        assert!(!eyes.fills_small_eye(Color::Black, edge));
        for x in [166, 167, 200, 250, 332, 400, 497, 498, 1166] {
            let spot = Spot { x, y: 166 };
            for color in [Color::Black, Color::White] {
                assert_eq!(
                    eyes.fills_small_eye(color, spot),
                    fills_small_eye_reference(&position, color, spot),
                    "{color:?}, {spot:?}"
                );
                assert_eq!(
                    fills_small_eye(&position, color, spot),
                    fills_small_eye_reference(&position, color, spot)
                );
            }
        }
        assert_eq!(eyes.pockets.len(), 1);
    }

    #[test]
    #[should_panic(expected = "availability must match")]
    fn a_stale_geometric_index_is_rejected() {
        let position = corners(Color::Black);
        let stale = GeometricAvailability::new(&Position::new(position.board()));
        SmallEyeAnalysis::with_availability(&position, &stale);
    }

    #[test]
    fn filling_a_corner_eye_is_not_a_suicide_but_spends_an_eye() {
        for color in [Color::Black, Color::White] {
            let position = corners(color);
            for spot in [Spot { x: 166, y: 166 }, Spot { x: 830, y: 830 }] {
                assert!(position.play(Stone { color, spot }).is_some());
                assert!(fills_small_eye(&position, color, spot));
                assert!(!fills_small_eye(&position, color.other(), spot));
            }
        }
    }

    #[test]
    fn a_larger_pocket_is_not_filled_even_at_a_touching_coordinate() {
        let original = corners(Color::Black);
        let position = position(
            3,
            original
                .stones()
                .iter()
                .copied()
                .filter(|stone| stone.spot != Spot { x: 498, y: 166 }),
        );
        let spot = Spot { x: 166, y: 166 };
        assert!(position.can_place(Spot { x: 498, y: 166 }));
        assert!(!fills_small_eye(&position, Color::Black, spot));
    }

    #[test]
    fn internal_single_point_eye_and_a_connection_are_distinct() {
        let mut stones = Vec::new();
        for y in [100, 300, 500, 700, 900] {
            for x in [100, 300, 500, 700, 900] {
                if (x, y) != (500, 500) && (x, y) != (100, 100) {
                    stones.push(Stone {
                        color: Color::Black,
                        spot: Spot { x, y },
                    });
                }
            }
        }
        let original = position(5, stones);
        let spot = Spot { x: 500, y: 500 };
        assert!(fills_small_eye(&original, Color::Black, spot));
        let position = position(
            5,
            original.stones().iter().copied().filter(|stone| {
                [(300, 500), (500, 300), (700, 500), (500, 700)]
                    .contains(&(stone.spot.x, stone.spot.y))
            }),
        );
        assert!(
            position
                .play(Stone {
                    color: Color::Black,
                    spot
                })
                .is_some()
        );
        assert!(!fills_small_eye(&position, Color::Black, spot));
    }

    #[test]
    fn enemy_boundaries_occupied_spots_and_open_space_are_not_own_eyes() {
        let original = corners(Color::Black);
        let position = position(
            3,
            original
                .stones()
                .iter()
                .copied()
                .enumerate()
                .map(|(index, stone)| {
                    if index == 0 {
                        Stone {
                            color: Color::White,
                            ..stone
                        }
                    } else {
                        stone
                    }
                }),
        );
        assert_eq!(position.stones()[0].color, Color::White);
        assert!(!fills_small_eye(
            &position,
            Color::Black,
            Spot { x: 166, y: 166 }
        ));
        assert!(!fills_small_eye(
            &position,
            Color::Black,
            position.stones()[0].spot
        ));
        assert!(!fills_small_eye(
            &Position::default(),
            Color::Black,
            Spot { x: 500, y: 500 }
        ));
    }
}
