//! Small eyes are pockets of geometrically available integer stone centres.
//! We conservatively connect all eight neighbouring coordinates: a pocket is
//! one-stone-sized only if the proposed stone overlaps every point in it.
//! Board edges may bound an eye; its stone boundary must be one friendly group.
//! This is a geometric query, not a life-and-death solver or a legality rule.

use continugo_rules::{COORD_RANGE, Color, Position, Spot};
use std::collections::HashSet;

/// Would a stone here completely fill a small eye of one friendly group?
/// Larger pockets, enemy boundaries and connections between groups return false.
/// Callers still check legality and may choose to allow captures unconditionally.
pub fn fills_small_eye(position: &Position, color: Color, spot: Spot) -> bool {
    let board = position.board();
    let stones = position.stones();
    if !position.can_place(spot)
        || !stones
            .iter()
            .any(|stone| stone.color == color && board.adjacent(spot, stone.spot))
    {
        return false;
    }

    let index = |p: Spot| p.y * COORD_RANGE + p.x;
    let mut seen = HashSet::from([index(spot)]);
    let mut pending = vec![spot];
    let mut boundary = vec![false; stones.len()];
    while let Some(current) = pending.pop() {
        for dy in -1..=1 {
            for dx in -1..=1 {
                let next = Spot {
                    x: current.x + dx,
                    y: current.y + dy,
                };
                if !board.contains(next) || !seen.insert(index(next)) {
                    continue;
                }
                let mut blocked = false;
                for (i, stone) in stones.iter().enumerate() {
                    if board.overlaps(next, stone.spot) {
                        if stone.color != color {
                            return false;
                        }
                        blocked = true;
                        boundary[i] = true;
                    }
                }
                if !blocked {
                    // A touching centre remains available: strict overlap matters.
                    if !board.overlaps(spot, next) {
                        return false;
                    }
                    pending.push(next);
                }
            }
        }
    }

    let Some(first) = boundary.iter().position(|&touches| touches) else {
        return false;
    };
    let owner = position
        .groups(color)
        .into_iter()
        .find(|group| group.contains(&first))
        .expect("friendly boundary group");
    boundary
        .iter()
        .enumerate()
        .all(|(i, &touches)| !touches || owner.contains(&i))
        && stones.iter().enumerate().all(|(i, stone)| {
            stone.color != color || !board.adjacent(spot, stone.spot) || owner.contains(&i)
        })
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
