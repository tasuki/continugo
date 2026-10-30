//! Original per-candidate flood fill, retained only as a test/benchmark oracle.

use continugo_rules::{COORD_RANGE, Color, Position, Spot};
use std::collections::HashSet;

pub fn fills_small_eye_reference(position: &Position, color: Color, spot: Spot) -> bool {
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
