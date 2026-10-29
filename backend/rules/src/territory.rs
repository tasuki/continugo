//! Voronoi area scoring, matching Elm's `Rules.Territory`.
//! Each surviving stone owns the board points nearest to its centre. Cells are
//! clipped to the full 1000-unit square, including occupied space: there is no
//! stone-count bonus, komi, or automatic removal of dead groups.
//!
//! Pass unique surviving stones, such as `game.position().stones()`. Scoring is
//! independent of stone radius, connectivity, and move history.
//!
//! ```
//! use continugo_rules::{Color, Game, Spot, territory};
//!
//! let game = Game::default().play(Spot { x: 500, y: 500 })?;
//! let areas = territory::areas(game.position().stones());
//! assert_eq!(territory::result(areas), territory::Outcome::Win(Color::Black, 1_000_000.0));
//! # Ok::<(), String>(())
//! ```

use crate::{COORD_RANGE, Color, Spot, Stone};

const BOARD_AREA: f64 = (COORD_RANGE * COORD_RANGE) as f64;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Cell {
    pub color: Color,
    pub polygon: Vec<Point>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Areas {
    pub black: f64,
    pub white: f64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Outcome {
    Draw,
    Win(Color, f64),
}

// Keep the half-plane closer to site than other. The midpoint form avoids
// subtracting large squared distances, just as in the Elm implementation.
fn clip(site: Spot, other: Spot, polygon: &[Point]) -> Vec<Point> {
    let Some(&last) = polygon.last() else {
        return Vec::new();
    };
    let dx = f64::from(other.x) - f64::from(site.x);
    let dy = f64::from(other.y) - f64::from(site.y);
    let mx = (f64::from(site.x) + f64::from(other.x)) / 2.0;
    let my = (f64::from(site.y) + f64::from(other.y)) / 2.0;
    let side = |p: Point| (p.x - mx) * dx + (p.y - my) * dy;

    let mut clipped = Vec::new();
    let mut previous = last;
    for &current in polygon {
        let previous_side = side(previous);
        let current_side = side(current);
        if (previous_side <= 0.0) != (current_side <= 0.0) {
            let t = previous_side / (previous_side - current_side);
            clipped.push(Point {
                x: previous.x + t * (current.x - previous.x),
                y: previous.y + t * (current.y - previous.y),
            });
        }
        if current_side <= 0.0 {
            clipped.push(current);
        }
        previous = current;
    }
    clipped
}

fn polygon_area(polygon: &[Point]) -> f64 {
    let Some(&last) = polygon.last() else {
        return 0.0;
    };
    let mut previous = last;
    let mut twice_area = 0.0;
    for &current in polygon {
        twice_area += previous.x * current.y - previous.y * current.x;
        previous = current;
    }
    twice_area.abs() / 2.0
}

pub fn cells(stones: &[Stone]) -> Vec<Cell> {
    let size = f64::from(COORD_RANGE);
    stones
        .iter()
        .map(|stone| {
            let mut polygon = vec![
                Point { x: 0.0, y: 0.0 },
                Point { x: size, y: 0.0 },
                Point { x: size, y: size },
                Point { x: 0.0, y: size },
            ];
            for other in stones {
                polygon = clip(stone.spot, other.spot, &polygon);
            }
            Cell {
                color: stone.color,
                polygon,
            }
        })
        .collect()
}

pub fn areas(stones: &[Stone]) -> Areas {
    let mut total = Areas::default();
    for cell in cells(stones) {
        let area = polygon_area(&cell.polygon);
        match cell.color {
            Color::Black => total.black += area,
            Color::White => total.white += area,
        }
    }
    total
}

pub fn result(territory: Areas) -> Outcome {
    let margin = territory.black - territory.white;
    if margin.abs() <= BOARD_AREA * 1e-10 {
        Outcome::Draw
    } else if margin > 0.0 {
        Outcome::Win(Color::Black, margin)
    } else {
        Outcome::Win(Color::White, -margin)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn polygon_area_is_independent_of_orientation() {
        let mut triangle = vec![
            Point { x: 0.0, y: 0.0 },
            Point { x: 7.0, y: 0.0 },
            Point { x: 0.0, y: 3.0 },
        ];
        assert_eq!(polygon_area(&triangle), 10.5);
        triangle.reverse();
        assert_eq!(polygon_area(&triangle), 10.5);
        assert_eq!(polygon_area(&[]), 0.0);
        assert_eq!(polygon_area(&triangle[..2]), 0.0);
    }

    #[test]
    fn real_leads_are_not_confused_with_rounding_noise() {
        assert_eq!(result(Areas::default()), Outcome::Draw);
        assert_eq!(
            result(Areas {
                black: 500000.000001,
                white: 499999.999999
            }),
            Outcome::Draw
        );
        assert_eq!(
            result(Areas {
                black: 500001.0,
                white: 499999.0
            }),
            Outcome::Win(Color::Black, 2.0)
        );
        assert_eq!(
            result(Areas {
                black: 499999.0,
                white: 500001.0
            }),
            Outcome::Win(Color::White, 2.0)
        );
    }
}
