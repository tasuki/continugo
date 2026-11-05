//! Benson's pass-alive chains, using exact integer placement geometry.
//!
//! For each color, remove the opponent and connect all centers not excluded by
//! friendly stones using the rules' stone adjacency (NOT pixel connectivity).
//! Distinct components cannot share an attacking stone or opposing chain. Board
//! edges need no special approximation: connections underneath an edge stone are
//! tested by the same strict adjacency, including integer-center cutoffs.
//!
//! A region is vital to a chain when every center in it is adjacent to that
//! chain, including centers currently occupied or blocked by opponents. Opposing
//! stones are neither permanent walls nor required support. Repeatedly discard
//! chains with fewer than two vital regions, and regions adjacent to discarded
//! chains. Survivors are unconditionally alive if their owner only passes.
//! Failure to certify a chain does not mean it is dead (e.g. seki is not solved).

use continugo_rules::{Board, COORD_RANGE, Color, Position, Spot};

#[cfg(test)]
mod tests;

/// Certified chains as indices into `position.stones()`, like `Position::groups`.
/// This is a position analysis, not an eye-filling policy or a legality filter.
pub fn unconditionally_alive(position: &Position, color: Color) -> Vec<Vec<usize>> {
    let groups = position.groups(color);
    if groups.is_empty() {
        return groups;
    }
    let board = position.board();
    let blocked = disc_rows(
        board,
        position
            .stones()
            .iter()
            .filter(|s| s.color == color)
            .map(|s| s.spot),
        board.diameter().pow(2) - 1,
    );
    let mut rows = free_rows(board, &blocked);
    let mut regions = components(board, &mut rows);
    if regions.len() < 2 {
        return Vec::new();
    }
    for (group, members) in groups.iter().enumerate() {
        let adjacent = disc_rows(
            board,
            members.iter().map(|&i| position.stones()[i].spot),
            2 * board.diameter().pow(2) - 1,
        );
        let mut counts = vec![0; regions.len()];
        for (runs, spans) in rows.iter().zip(&adjacent) {
            let mut i = 0;
            let mut j = 0;
            while i < runs.len() && j < spans.len() {
                let run = &runs[i];
                let (left, right) = spans[j];
                let overlap = run.right.min(right) - run.left.max(left) + 1;
                if overlap > 0 {
                    counts[run.region] += overlap as usize;
                }
                if run.right <= right {
                    i += 1;
                } else {
                    j += 1;
                }
            }
        }
        for (region, count) in regions.iter_mut().zip(counts) {
            if count > 0 {
                region.boundary.push(group);
                if count == region.size {
                    region.vital.push(group);
                }
            }
        }
    }
    let alive = prune(groups.len(), &regions);
    groups
        .into_iter()
        .zip(alive)
        .filter_map(|(group, alive)| alive.then_some(group))
        .collect()
}

#[derive(Debug)]
struct Run {
    left: i32,
    right: i32,
    region: usize,
}

#[derive(Default)]
struct Region {
    size: usize,
    boundary: Vec<usize>,
    vital: Vec<usize>,
}

/// Sorted, merged inclusive disk spans, clipped to the stone-center domain.
fn disc_rows(
    board: Board,
    spots: impl Iterator<Item = Spot>,
    squared_radius: i32,
) -> Vec<Vec<(i32, i32)>> {
    let mut rows = vec![Vec::new(); COORD_RANGE as usize];
    let low = board.stone_radius();
    let high = COORD_RANGE - low;
    let reach = (squared_radius as u32).isqrt() as i32;
    for spot in spots {
        for y in (spot.y - reach).max(low)..=(spot.y + reach).min(high) {
            let dy = y - spot.y;
            let width = ((squared_radius - dy * dy) as u32).isqrt() as i32;
            let left = (spot.x - width).max(low);
            let right = (spot.x + width).min(high);
            if left <= right {
                rows[y as usize].push((left, right));
            }
        }
    }
    for row in &mut rows {
        row.sort_unstable();
        let mut merged = 0;
        for i in 0..row.len() {
            let (left, right) = row[i];
            if merged > 0 && left <= row[merged - 1].1 + 1 {
                row[merged - 1].1 = row[merged - 1].1.max(right);
            } else {
                row[merged] = (left, right);
                merged += 1;
            }
        }
        row.truncate(merged);
    }
    rows
}

fn free_rows(board: Board, blocked: &[Vec<(i32, i32)>]) -> Vec<Vec<Run>> {
    let mut rows: Vec<Vec<Run>> = (0..COORD_RANGE).map(|_| Vec::new()).collect();
    let low = board.stone_radius();
    let high = COORD_RANGE - low;
    for y in low..=high {
        let mut left = low;
        let row = &mut rows[y as usize];
        for &(start, end) in &blocked[y as usize] {
            if left < start {
                row.push(Run {
                    left,
                    right: start - 1,
                    region: 0,
                });
            }
            left = end + 1;
        }
        if left <= high {
            row.push(Run {
                left,
                right: high,
                region: 0,
            });
        }
    }
    rows
}

// A horizontal run is already connected. Join two runs exactly when their
// closest integer centers satisfy stone adjacency; no coordinate-pair search.
fn components(board: Board, rows: &mut [Vec<Run>]) -> Vec<Region> {
    let mut parents = Vec::new();
    for run in rows.iter_mut().flatten() {
        run.region = parents.len();
        parents.push(parents.len());
    }
    let squared_radius = 2 * board.diameter().pow(2) - 1;
    let reach = (squared_radius as u32).isqrt() as usize;
    for (y, row) in rows.iter().enumerate() {
        if row.is_empty() {
            continue;
        }
        for (dy, other) in rows[..=y].iter().rev().take(reach + 1).enumerate() {
            if other.is_empty() {
                continue;
            }
            let dy = dy as i32;
            let width = ((squared_radius - dy * dy) as u32).isqrt() as i32;
            let mut start = 0;
            for run in row {
                while start < other.len() && other[start].right < run.left - width {
                    start += 1;
                }
                for next in &other[start..] {
                    if next.left > run.right + width {
                        break;
                    }
                    let a = root(&mut parents, run.region);
                    let b = root(&mut parents, next.region);
                    parents[a.max(b)] = a.min(b);
                }
            }
        }
    }
    let mut labels = vec![usize::MAX; parents.len()];
    let mut regions: Vec<Region> = Vec::new();
    for run in rows.iter_mut().flatten() {
        let parent = root(&mut parents, run.region);
        if labels[parent] == usize::MAX {
            labels[parent] = regions.len();
            regions.push(Region::default());
        }
        run.region = labels[parent];
        regions[run.region].size += (run.right - run.left + 1) as usize;
    }
    regions
}

fn root(parents: &mut [usize], mut id: usize) -> usize {
    while parents[id] != id {
        parents[id] = parents[parents[id]];
        id = parents[id];
    }
    id
}

fn prune(groups: usize, regions: &[Region]) -> Vec<bool> {
    let mut alive = vec![true; groups];
    let mut retained = vec![true; regions.len()];
    loop {
        let mut eyes = vec![0; groups];
        for (region, &keep) in regions.iter().zip(&retained) {
            if keep {
                for &group in &region.vital {
                    eyes[group] += 1;
                }
            }
        }
        let mut changed = false;
        for (group, keep) in alive.iter_mut().enumerate() {
            if *keep && eyes[group] < 2 {
                *keep = false;
                changed = true;
            }
        }
        if !changed {
            return alive;
        }
        for (region, keep) in regions.iter().zip(&mut retained) {
            *keep &= region.boundary.iter().all(|&group| alive[group]);
        }
    }
}
