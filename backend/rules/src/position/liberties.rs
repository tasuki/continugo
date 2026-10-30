//! Exact integer scanlines for liberty existence, following the frontend's
//! Rules.Liberties algorithm. Strict discs use squared radii minus one; no
//! floating-point rounding or sampled liberties. Row geometry is shared by all
//! positions with the same stone radius.

use super::{Position, Spot};
use crate::{Board, COORD_RANGE};
use std::sync::OnceLock;

struct DiscRows {
    overlap: Vec<i32>,
    adjacency: Vec<i32>,
}

static ROWS: [OnceLock<DiscRows>; 501] = [const { OnceLock::new() }; 501];

fn disc_rows(squared_radius: i32) -> Vec<i32> {
    let reach = (squared_radius as u32).isqrt() as i32;
    let mut rows = Vec::with_capacity(reach as usize + 1);
    let mut x = reach;
    for y in 0..=reach {
        while x * x + y * y > squared_radius {
            x -= 1;
        }
        rows.push(x);
    }
    rows
}

fn rows_on(board: Board) -> &'static DiscRows {
    ROWS[board.stone_radius() as usize].get_or_init(|| {
        let d = board.diameter();
        DiscRows {
            overlap: disc_rows(d * d - 1),
            adjacency: disc_rows(2 * d * d - 1),
        }
    })
}

fn half_width(rows: &[i32], dy: i32) -> i32 {
    rows.get(dy.unsigned_abs() as usize).copied().unwrap_or(-1)
}

fn interval_has_gap(mut left: i32, right: i32, blocked: &[(i32, i32)]) -> bool {
    if left > right {
        return false;
    }
    for &(start, end) in blocked {
        if start > left {
            return true;
        }
        left = left.max(end + 1);
        if left > right {
            return false;
        }
    }
    true
}

impl Position {
    pub(super) fn has_liberty(&self, spot: Spot) -> bool {
        let d = self.board.diameter();
        for (dx, dy) in [(-d, 0), (0, -d), (0, d), (d, 0)] {
            if self.can_place(Spot {
                x: spot.x + dx,
                y: spot.y + dy,
            }) {
                return true;
            }
        }
        let rows = rows_on(self.board);
        let reach = rows.adjacency.len() as i32 - 1;
        let low = self.board.stone_radius();
        let high = COORD_RANGE - low;
        // Only these stones can block any part of the queried adjacency disc.
        let block_reach = reach + d - 1;
        let nearby: Vec<_> = self
            .stones
            .iter()
            .filter(|stone| {
                (stone.spot.x - spot.x).abs() <= block_reach
                    && (stone.spot.y - spot.y).abs() <= block_reach
            })
            .map(|stone| stone.spot)
            .collect();
        let mut blocked = Vec::with_capacity(nearby.len());
        'rows: for y in (spot.y - reach).max(low)..=(spot.y + reach).min(high) {
            let outer = half_width(&rows.adjacency, y - spot.y);
            let inner = half_width(&rows.overlap, y - spot.y) + 1;
            let left = (spot.x - outer).max(low);
            let right = (spot.x + outer).min(high);
            if left > right {
                continue;
            }
            blocked.clear();
            for other in &nearby {
                let width = half_width(&rows.overlap, y - other.y);
                if width < 0 {
                    continue;
                }
                let start = other.x - width;
                let end = other.x + width;
                if start <= left && end >= right {
                    continue 'rows;
                }
                if start <= right && end >= left {
                    blocked.push((start, end));
                }
            }
            blocked.sort_unstable_by_key(|&(start, _)| start);
            if interval_has_gap(left, (spot.x - inner).min(high), &blocked)
                || interval_has_gap((spot.x + inner).max(low), right, &blocked)
            {
                return true;
            }
        }
        false
    }

    // Slow original implementation retained only as a test/benchmark oracle.
    #[cfg(any(test, feature = "reference"))]
    pub(super) fn has_liberty_reference(&self, spot: Spot) -> bool {
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
    use crate::{Color, Stone};

    #[test]
    fn row_widths_preserve_strict_integer_disc_boundaries() {
        for size in [1, 2, 3, 7, 9, 13, 19, 37, 500] {
            let board = Board::new(size).unwrap();
            let rows = rows_on(board);
            let d = board.diameter();
            for (disc, squared) in [(&rows.overlap, d * d - 1), (&rows.adjacency, 2 * d * d - 1)] {
                for (dy, &width) in disc.iter().enumerate() {
                    let y = dy as i32;
                    assert!(width * width + y * y <= squared);
                    assert!((width + 1) * (width + 1) + y * y > squared);
                    assert_eq!(half_width(disc, -y), width);
                }
                assert_eq!(half_width(disc, disc.len() as i32), -1);
            }
        }
    }

    #[test]
    fn scanlines_match_exhaustive_search_for_enclosures_edges_and_tiny_gaps() {
        for size in [1, 2, 3, 9, 13, 19, 500] {
            let board = Board::new(size).unwrap();
            let r = board.stone_radius();
            let d = board.diameter();
            for spot in [
                Spot { x: 500, y: 500 },
                Spot { x: r, y: r },
                Spot {
                    x: COORD_RANGE - r,
                    y: COORD_RANGE - r,
                },
            ] {
                for gap in [0, 1, 2] {
                    let mut position = Position::new(board);
                    position.stones.push(Stone {
                        color: Color::Black,
                        spot,
                    });
                    for dy in [-d, 0, d] {
                        for dx in [-d, 0, d] {
                            if dx == 0 && dy == 0 {
                                continue;
                            }
                            let other = Spot {
                                x: spot.x + dx + gap,
                                y: spot.y + dy,
                            };
                            if board.contains(other) {
                                position.stones.push(Stone {
                                    color: Color::White,
                                    spot: other,
                                });
                            }
                        }
                    }
                    assert_eq!(
                        position.has_liberty(spot),
                        position.has_liberty_reference(spot),
                        "size={size}, spot={spot:?}, gap={gap}"
                    );
                }
            }
        }
    }

    #[test]
    fn multi_stone_capture_matches_the_original_surviving_stones() {
        let fixture = include_str!("../../../../fixtures/rules/capture-four.txt");
        let mut lines = fixture.lines();
        let record = lines.nth(1).unwrap().strip_prefix("record: ").unwrap();
        let capture =
            crate::sgf::decode_play(lines.next().unwrap().strip_prefix("move: ").unwrap()).unwrap();
        let mut position = Position::default();
        for play in crate::sgf::decode(record)
            .unwrap()
            .into_iter()
            .chain([capture])
        {
            if let crate::Play::Place(stone) = play {
                let next = position.play(stone);
                assert_eq!(next, position.play_reference(stone));
                position = next.unwrap();
            }
        }
    }

    #[test]
    fn placements_and_captures_match_the_original_rule_path() {
        let mut rng = 42u64;
        for size in [1, 2, 3, 9, 13, 19] {
            let mut position = Position::new(Board::new(size).unwrap());
            for ply in 0..80 {
                rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                let spot = Spot {
                    x: ((rng >> 32) % 1000) as i32,
                    y: ((rng >> 16) % 1000) as i32,
                };
                let stone = Stone {
                    color: if ply % 2 == 0 {
                        Color::Black
                    } else {
                        Color::White
                    },
                    spot,
                };
                let next = position.play(stone);
                assert_eq!(
                    next,
                    position.play_reference(stone),
                    "size={size}, ply={ply}"
                );
                if let Some(next) = next {
                    position = next;
                }
            }
        }
    }
}
