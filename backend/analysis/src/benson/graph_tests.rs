use super::*;

#[test]
fn randomized_interval_graph_matches_all_pairs_points() {
    let mut state = 0x51_abcdef_98765432_u64;
    let mut random = || {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        state as u32
    };
    for case in 0..512 {
        let size = [1, 2, 3, 9, 13, 19, 31, 100, 500][case % 9];
        let board = Board::new(size).unwrap();
        let low = board.stone_radius();
        let high = COORD_RANGE - low;
        let side = high - low + 1;
        let mut points: Vec<_> = (0..80)
            .map(|_| {
                let x = low + (random() % side as u32) as i32;
                let y = if case % 3 == 0 {
                    low + (random() % 5) as i32
                } else {
                    low + (random() % side as u32) as i32
                }
                .min(high);
                Spot { x, y }
            })
            .collect();
        points.sort_by_key(|p| (p.y, p.x));
        points.dedup();
        let mut rows: Vec<Vec<Run>> = (0..COORD_RANGE).map(|_| Vec::new()).collect();
        for point in &points {
            let row = &mut rows[point.y as usize];
            if let Some(last) = row.last_mut()
                && last.right + 1 == point.x
            {
                last.right = point.x;
            } else {
                row.push(Run {
                    left: point.x,
                    right: point.x,
                    region: 0,
                });
            }
        }
        let regions = components(board, &mut rows);
        assert_eq!(regions.iter().map(|r| r.size).sum::<usize>(), points.len());
        let mut expected: Vec<_> = (0..points.len()).collect();
        for (i, a) in points.iter().enumerate() {
            for (j, b) in points[..i].iter().enumerate() {
                if board.adjacent(*a, *b) {
                    join_oracle(&mut expected, i, j);
                }
            }
        }
        let actual: Vec<_> = points
            .iter()
            .map(|p| {
                rows[p.y as usize]
                    .iter()
                    .find(|r| r.left <= p.x && p.x <= r.right)
                    .unwrap()
                    .region
            })
            .collect();
        for i in 0..points.len() {
            for j in 0..i {
                assert_eq!(
                    actual[i] == actual[j],
                    root(&mut expected, i) == root(&mut expected, j),
                    "case={case} size={size} {:?} {:?}",
                    points[i],
                    points[j]
                );
            }
        }
    }
}

#[test]
fn many_separated_fragments_remain_separate() {
    let mut rows: Vec<Vec<Run>> = (0..COORD_RANGE).map(|_| Vec::new()).collect();
    let mut count = 0;
    for y in (10..COORD_RANGE as usize).step_by(30) {
        for x in (10..COORD_RANGE).step_by(30) {
            rows[y].push(Run {
                left: x,
                right: x,
                region: 0,
            });
            count += 1;
        }
    }
    let regions = components(Board::new(500).unwrap(), &mut rows);
    assert_eq!(regions.len(), count);
    assert!(regions.iter().all(|region| region.size == 1));
}

fn join_oracle(parents: &mut [usize], a: usize, b: usize) {
    let a = root(parents, a);
    let b = root(parents, b);
    parents[a] = b;
}
