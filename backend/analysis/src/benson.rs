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
mod graph_tests;
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
    let widths: Vec<_> = (0..=reach)
        .map(|dy| ((squared_radius - dy * dy) as u32).isqrt() as i32)
        .collect();
    for spot in spots {
        for y in (spot.y - reach).max(low)..=(spot.y + reach).min(high) {
            let dy = y - spot.y;
            let width = widths[dy.unsigned_abs() as usize];
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
    let widths: Vec<_> = (0..=reach)
        .map(|dy| ((squared_radius - (dy * dy) as i32) as u32).isqrt() as i32)
        .collect();
    // First build a cheap spanning forest with same-row and adjacent-row edges.
    // These are a subset of the original exact graph, not pixel connectivity.
    for (y, row) in rows.iter().enumerate() {
        for pair in row.windows(2) {
            if pair[1].left - pair[0].right <= widths[0] {
                join(&mut parents, pair[0].region, pair[1].region);
            }
        }
        if y > 0 {
            connect_rows(row, &rows[y - 1], widths[1], &mut parents);
        }
    }
    let first = parents.first().copied().map(|id| root(&mut parents, id));
    let connected = (0..parents.len()).all(|id| Some(root(&mut parents, id)) == first);
    if !connected {
        // Only edges between distinct forest fragments can change connectivity.
        let mut labels = vec![usize::MAX; parents.len()];
        let mut fragments: Vec<Fragment> = Vec::new();
        for (y, row) in rows.iter().enumerate() {
            for run in row {
                let parent = root(&mut parents, run.region);
                if labels[parent] == usize::MAX {
                    labels[parent] = fragments.len();
                    fragments.push(Fragment {
                        representative: parent,
                        runs: Vec::new(),
                        left: run.left,
                        right: run.right,
                        top: y as i32,
                        bottom: y as i32,
                    });
                }
                let fragment = &mut fragments[labels[parent]];
                fragment.runs.push((y as i32, run.left, run.right));
                fragment.left = fragment.left.min(run.left);
                fragment.right = fragment.right.max(run.right);
                fragment.bottom = y as i32;
            }
        }
        for (i, a) in fragments.iter().enumerate() {
            for b in &fragments[..i] {
                if root(&mut parents, a.representative) == root(&mut parents, b.representative) {
                    continue;
                }
                let dx = gap(a.left, a.right, b.left, b.right);
                let dy = gap(a.top, a.bottom, b.top, b.bottom);
                if dx * dx + dy * dy > squared_radius {
                    continue;
                }
                let (small, large) = if a.runs.len() <= b.runs.len() {
                    (a, b)
                } else {
                    (b, a)
                };
                if fragments_touch(small, large, &widths) {
                    join(&mut parents, a.representative, b.representative);
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

struct Fragment {
    representative: usize,
    runs: Vec<(i32, i32, i32)>,
    left: i32,
    right: i32,
    top: i32,
    bottom: i32,
}

fn gap(a_left: i32, a_right: i32, b_left: i32, b_right: i32) -> i32 {
    (a_left - b_right).max(b_left - a_right).max(0)
}

fn fragments_touch(a: &Fragment, b: &Fragment, widths: &[i32]) -> bool {
    let reach = widths.len() as i32 - 1;
    let mut start = 0;
    for &(y, left, right) in &a.runs {
        while start < b.runs.len() && b.runs[start].0 < y - reach {
            start += 1;
        }
        for &(other_y, other_left, other_right) in &b.runs[start..] {
            if other_y > y + reach {
                break;
            }
            if gap(left, right, other_left, other_right)
                <= widths[(y - other_y).unsigned_abs() as usize]
            {
                return true;
            }
        }
    }
    false
}

fn join(parents: &mut [usize], a: usize, b: usize) {
    let a = root(parents, a);
    let b = root(parents, b);
    parents[a.max(b)] = a.min(b);
}

fn connect_rows(row: &[Run], other: &[Run], width: i32, parents: &mut [usize]) {
    let mut start = 0;
    for run in row {
        while start < other.len() && other[start].right < run.left - width {
            start += 1;
        }
        for next in &other[start..] {
            if next.left > run.right + width {
                break;
            }
            join(parents, run.region, next.region);
        }
    }
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
