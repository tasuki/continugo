//! Exact integer scanlines for liberty existence, following the frontend's
//! Rules.Liberties algorithm. Strict discs use squared radii minus one; no
//! floating-point rounding or sampled liberties. Row geometry is shared by all
//! positions with the same stone radius.

use super::{Position, Spot};
use crate::{Board, COORD_RANGE};
use std::sync::OnceLock;

/// Inclusive run of physically free integer liberty centers on one row.
/// Runs are ordered by (y, left), disjoint and nonadjacent on each row.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LibertySpan {
    pub y: i32,
    pub left: i32,
    pub right: i32,
}

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

fn first_gap(mut left: i32, right: i32, blocked: &[(i32, i32)]) -> Option<i32> {
    if left > right {
        return None;
    }
    for &(start, end) in blocked {
        if start > left {
            return Some(left);
        }
        left = left.max(end + 1);
        if left > right {
            return None;
        }
    }
    Some(left)
}

// Visit the unblocked intervals of a clipped row. False stops iteration early.
fn open_intervals(
    mut left: i32,
    right: i32,
    blocked: &[(i32, i32)],
    mut visit: impl FnMut(i32, i32) -> bool,
) -> bool {
    if left > right {
        return true;
    }
    for &(start, end) in blocked {
        if start > right {
            break;
        }
        if start > left && !visit(left, start - 1) {
            return false;
        }
        left = left.max(end + 1);
        if left > right {
            return true;
        }
    }
    visit(left, right)
}

/// Lazily owned by a prepared position, never built for move validation alone.
/// Each row is the sorted disjoint union of all existing exclusion discs.
#[derive(Debug)]
pub(super) struct RowBlockers {
    rows: Vec<Vec<(i32, i32)>>,
}

impl RowBlockers {
    pub(super) fn new(position: &Position) -> Self {
        let mut rows = vec![Vec::new(); COORD_RANGE as usize];
        let geometry = rows_on(position.board);
        let reach = geometry.overlap.len() as i32 - 1;
        let low = position.board.stone_radius();
        let high = COORD_RANGE - low;
        for stone in &position.stones {
            for y in (stone.spot.y - reach).max(low)..=(stone.spot.y + reach).min(high) {
                let width = half_width(&geometry.overlap, y - stone.spot.y);
                rows[y as usize].push((stone.spot.x - width, stone.spot.x + width));
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
        Self { rows }
    }

    fn row(&self, y: i32) -> &[(i32, i32)] {
        &self.rows[y as usize]
    }

    pub(super) fn blocks(&self, spot: Spot) -> bool {
        let row = self.row(spot.y);
        let end = row.partition_point(|&(start, _)| start <= spot.x);
        end != 0 && row[end - 1].1 >= spot.x
    }
}

impl Position {
    pub(super) fn visit_local_liberty_spans_with_blockers(
        &self,
        spot: Spot,
        blockers: &RowBlockers,
        mut visit: impl FnMut(LibertySpan),
    ) {
        let geometry = rows_on(self.board);
        let reach = geometry.adjacency.len() as i32 - 1;
        let low = self.board.stone_radius();
        let high = COORD_RANGE - low;
        for y in (spot.y - reach).max(low)..=(spot.y + reach).min(high) {
            let dy = y - spot.y;
            let outer = half_width(&geometry.adjacency, dy);
            let left = (spot.x - outer).max(low);
            let right = (spot.x + outer).min(high);
            let mut emit = |a, b| {
                let row = blockers.row(y);
                let first = row.partition_point(|&(_, end)| end < a);
                open_intervals(a, b, &row[first..], |left, right| {
                    visit(LibertySpan { y, left, right });
                    true
                });
            };
            let inner = half_width(&geometry.overlap, dy);
            if inner < 0 {
                emit(left, right);
            } else {
                emit(left, right.min(spot.x - inner - 1));
                emit(left.max(spot.x + inner + 1), right);
            }
        }
    }

    /// All geometric liberties of a group, as exact integer row intervals.
    /// Members are indices from this position's `groups` output. This matches
    /// frontend Rules.Liberties.openRowsOn: no suicide, ko, or eye filtering.
    pub fn liberty_spans(&self, members: &[usize]) -> Vec<LibertySpan> {
        self.liberty_spans_with_blockers(members, None)
    }

    pub(super) fn liberty_spans_with_blockers(
        &self,
        members: &[usize],
        cached: Option<&RowBlockers>,
    ) -> Vec<LibertySpan> {
        let Some(first) = members.first() else {
            return Vec::new();
        };
        let rows = rows_on(self.board);
        let reach = rows.adjacency.len() as i32 - 1;
        let low = self.board.stone_radius();
        let high = COORD_RANGE - low;
        let mut first_y = self.stones[*first].spot.y;
        let mut last_y = first_y;
        for &i in members {
            first_y = first_y.min(self.stones[i].spot.y);
            last_y = last_y.max(self.stones[i].spot.y);
        }
        let mut reachable: Vec<(i32, i32)> = Vec::with_capacity(members.len());
        let mut blocked: Vec<(i32, i32)> = Vec::with_capacity(if cached.is_none() {
            self.stones.len()
        } else {
            0
        });
        let mut spans = Vec::new();
        for y in (first_y - reach).max(low)..=(last_y + reach).min(high) {
            reachable.clear();
            for &i in members {
                let spot = self.stones[i].spot;
                let width = half_width(&rows.adjacency, y - spot.y);
                if width >= 0 {
                    let left = (spot.x - width).max(low);
                    let right = (spot.x + width).min(high);
                    if left <= right {
                        reachable.push((left, right));
                    }
                }
            }
            reachable.sort_unstable();
            // Merge the union of adjacency discs before subtracting blockers.
            let mut merged = 0;
            for i in 0..reachable.len() {
                let (left, right) = reachable[i];
                if merged > 0 && left <= reachable[merged - 1].1 + 1 {
                    reachable[merged - 1].1 = reachable[merged - 1].1.max(right);
                } else {
                    reachable[merged] = (left, right);
                    merged += 1;
                }
            }
            reachable.truncate(merged);
            let blockers = if let Some(cached) = cached {
                cached.row(y)
            } else {
                blocked.clear();
                for stone in &self.stones {
                    let width = half_width(&rows.overlap, y - stone.spot.y);
                    if width >= 0 {
                        blocked.push((stone.spot.x - width, stone.spot.x + width));
                    }
                }
                blocked.sort_unstable();
                &blocked
            };
            for &(left, right) in &reachable {
                open_intervals(left, right, blockers, |left, right| {
                    spans.push(LibertySpan { y, left, right });
                    true
                });
            }
        }
        spans
    }

    /// Geometrically free placements whose strict exclusion disc covers every
    /// liberty center. Span endpoints suffice: squared distance is convex in x.
    /// This is only a candidate filter; the caller must validate captures/suicide.
    #[cfg(test)]
    pub(super) fn visit_liberty_covers(
        &self,
        spans: &[LibertySpan],
        visit: impl FnMut(Spot) -> bool,
    ) {
        self.visit_liberty_covers_with_blockers(spans, None, visit);
    }

    pub(super) fn visit_liberty_covers_with_blockers(
        &self,
        spans: &[LibertySpan],
        cached: Option<&RowBlockers>,
        mut visit: impl FnMut(Spot) -> bool,
    ) {
        let d = self.board.diameter();
        let low = self.board.stone_radius();
        let high = COORD_RANGE - low;
        let (mut x_low, mut x_high, mut y_low, mut y_high) = (low, high, low, high);
        if let Some(first) = spans.first() {
            let (mut min_x, mut max_x, mut min_y, mut max_y) =
                (first.left, first.right, first.y, first.y);
            for span in spans {
                min_x = min_x.min(span.left);
                max_x = max_x.max(span.right);
                min_y = min_y.min(span.y);
                max_y = max_y.max(span.y);
            }
            x_low = low.max(max_x - d + 1);
            x_high = high.min(min_x + d - 1);
            y_low = low.max(max_y - d + 1);
            y_high = high.min(min_y + d - 1);
        }
        if x_low > x_high || y_low > y_high {
            return;
        }
        let rows = rows_on(self.board);
        let mut blocked = Vec::with_capacity(if cached.is_none() {
            self.stones.len()
        } else {
            0
        });
        'rows: for y in y_low..=y_high {
            let (mut left, mut right) = (x_low, x_high);
            for span in spans {
                let width = half_width(&rows.overlap, y - span.y);
                if width < 0 {
                    continue 'rows;
                }
                left = left.max(span.right - width);
                right = right.min(span.left + width);
                if left > right {
                    continue 'rows;
                }
            }
            let blockers = if let Some(cached) = cached {
                cached.row(y)
            } else {
                blocked.clear();
                for stone in &self.stones {
                    let width = half_width(&rows.overlap, y - stone.spot.y);
                    if width >= 0 {
                        blocked.push((stone.spot.x - width, stone.spot.x + width));
                    }
                }
                blocked.sort_unstable();
                &blocked
            };
            if !open_intervals(left, right, blockers, |left, right| {
                (left..=right).all(|x| visit(Spot { x, y }))
            }) {
                return;
            }
        }
    }

    pub(super) fn has_liberty(&self, spot: Spot) -> bool {
        self.find_liberty(spot).is_some()
    }

    pub(super) fn find_liberty(&self, spot: Spot) -> Option<Spot> {
        let mut found = None;
        self.visit_liberties(spot, |liberty| {
            found = Some(liberty);
            false
        });
        found
    }

    /// Visit exact integer liberties, stopping when the callback returns false.
    /// Quick probes may be visited again by scanlines; collectors deduplicate.
    pub(super) fn visit_liberties(&self, spot: Spot, mut visit: impl FnMut(Spot) -> bool) {
        let d = self.board.diameter();
        for (dx, dy) in [(-d, 0), (0, -d), (0, d), (d, 0)] {
            let candidate = Spot {
                x: spot.x + dx,
                y: spot.y + dy,
            };
            if self.can_place(candidate) && !visit(candidate) {
                return;
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
            for (mut start, end) in [
                (left, (spot.x - inner).min(high)),
                ((spot.x + inner).max(low), right),
            ] {
                while let Some(x) = first_gap(start, end, &blocked) {
                    if !visit(Spot { x, y }) {
                        return;
                    }
                    start = x + 1;
                }
            }
        }
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
    fn capture_cover_filter_matches_exhaustive_geometry_including_strict_touching() {
        let empty = Position::default();
        let occupied = empty
            .play(Stone {
                color: Color::Black,
                spot: Spot { x: 500, y: 500 },
            })
            .unwrap();
        let cases = [
            vec![LibertySpan {
                y: 500,
                left: 576,
                right: 576,
            }],
            vec![
                LibertySpan {
                    y: 500,
                    left: 576,
                    right: 607,
                },
                LibertySpan {
                    y: 576,
                    left: 500,
                    right: 520,
                },
            ],
            // Separation exactly 2*d: no strict exclusion disc can cover both.
            vec![
                LibertySpan {
                    y: 500,
                    left: 424,
                    right: 424,
                },
                LibertySpan {
                    y: 500,
                    left: 576,
                    right: 576,
                },
            ],
        ];
        for position in [&empty, &occupied] {
            for spans in &cases {
                let mut mask = vec![false; 1_000_000];
                position.visit_liberty_covers(spans, |spot| {
                    mask[(spot.y * 1000 + spot.x) as usize] = true;
                    true
                });
                let mut cached_mask = vec![false; 1_000_000];
                let blockers = RowBlockers::new(position);
                position.visit_liberty_covers_with_blockers(spans, Some(&blockers), |spot| {
                    cached_mask[(spot.y * 1000 + spot.x) as usize] = true;
                    true
                });
                assert_eq!(mask, cached_mask);
                for y in 0..1000 {
                    for x in 0..1000 {
                        let spot = Spot { x, y };
                        let expected = position.can_place(spot)
                            && spans.iter().all(|s| {
                                (s.left..=s.right)
                                    .all(|x| position.board.overlaps(spot, Spot { x, y: s.y }))
                            });
                        assert_eq!(
                            mask[(y * 1000 + x) as usize],
                            expected,
                            "{spot:?}, {spans:?}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn group_spans_match_pointwise_geometry_at_edges_and_touching_stones() {
        for size in [3, 9, 13, 19] {
            let board = Board::new(size).unwrap();
            let r = board.stone_radius();
            let d = board.diameter();
            let mut position = Position::new(board);
            for (color, x, y) in [
                (Color::Black, r, r),
                (Color::White, r + d, r),
                (Color::Black, r + d, r + d),
            ] {
                position = position
                    .play(Stone {
                        color,
                        spot: Spot { x, y },
                    })
                    .unwrap();
            }
            for color in [Color::Black, Color::White] {
                for members in position.groups(color) {
                    let spans = position.liberty_spans(&members);
                    assert_eq!(
                        spans,
                        position.liberty_spans_with_blockers(
                            &members,
                            Some(&RowBlockers::new(&position))
                        )
                    );
                    let mut mask = vec![false; 1_000_000];
                    let mut last: Option<LibertySpan> = None;
                    for &span in &spans {
                        assert!(span.left >= r && span.right <= 1000 - r);
                        if let Some(previous) = last {
                            assert!(
                                span.y > previous.y
                                    || (span.y == previous.y && span.left > previous.right + 1)
                            );
                        }
                        for x in span.left..=span.right {
                            mask[(span.y * 1000 + x) as usize] = true;
                        }
                        last = Some(span);
                    }
                    for y in 0..1000 {
                        for x in 0..1000 {
                            let spot = Spot { x, y };
                            let expected = position.can_place(spot)
                                && members
                                    .iter()
                                    .any(|&i| board.adjacent(position.stones[i].spot, spot));
                            assert_eq!(
                                mask[(y * 1000 + x) as usize],
                                expected,
                                "size={size}, {spot:?}"
                            );
                        }
                    }
                    assert_eq!(
                        position.group_liberties(&members, 4).len(),
                        mask.iter().filter(|&&v| v).count().min(4)
                    );
                }
            }
        }
    }

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
                    let witness = position.find_liberty(spot);
                    assert_eq!(
                        witness.is_some(),
                        position.has_liberty_reference(spot),
                        "size={size}, spot={spot:?}, gap={gap}"
                    );
                    if let Some(liberty) = witness {
                        assert!(position.can_place(liberty));
                        let distance = spot.distance_squared(liberty);
                        let squared = i64::from(d).pow(2);
                        assert!((squared..2 * squared).contains(&distance));
                    }
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
