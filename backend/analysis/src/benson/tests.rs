use super::*;
use continugo_rules::{Play, Stone, sgf};

const CORNERS: [(i32, i32); 7] = [
    (498, 166),
    (830, 166),
    (166, 498),
    (498, 498),
    (830, 498),
    (166, 830),
    (498, 830),
];

fn position(size: i32, color: Color, spots: &[(i32, i32)]) -> Position {
    spots.iter().fold(
        Position::new(Board::new(size).unwrap()),
        |position, &(x, y)| {
            position
                .play(Stone {
                    color,
                    spot: Spot { x, y },
                })
                .unwrap()
        },
    )
}

fn regions(position: &Position, color: Color) -> (Vec<Vec<Run>>, Vec<Region>) {
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
    let regions = components(board, &mut rows);
    (rows, regions)
}

#[test]
fn two_corner_eyes_are_alive_for_either_color_but_one_is_not() {
    for color in [Color::Black, Color::White] {
        let position = position(3, color, &CORNERS);
        assert_eq!(
            unconditionally_alive(&position, color),
            position.groups(color)
        );
        assert!(unconditionally_alive(&position, color.other()).is_empty());
        for spot in [Spot { x: 166, y: 166 }, Spot { x: 834, y: 834 }] {
            assert!(
                position
                    .play(Stone {
                        color: color.other(),
                        spot
                    })
                    .is_none()
            );
            let filled = position.play(Stone { color, spot }).unwrap();
            assert!(unconditionally_alive(&filled, color).is_empty());
        }
    }
}

#[test]
fn interior_and_corner_eyes_can_support_the_same_chain() {
    let spots: Vec<_> = [100, 300, 500, 700, 900]
        .into_iter()
        .flat_map(|y| [100, 300, 500, 700, 900].into_iter().map(move |x| (x, y)))
        .filter(|p| ![(100, 100), (500, 500)].contains(p))
        .collect();
    let position = position(5, Color::Black, &spots);
    assert_eq!(
        unconditionally_alive(&position, Color::Black),
        position.groups(Color::Black)
    );
}

#[test]
fn both_colors_are_certified_without_marking_an_unrelated_chain_alive() {
    let corner = [
        (165, 55),
        (275, 55),
        (55, 165),
        (275, 165),
        (55, 275),
        (165, 275),
        (275, 275),
    ];
    let mut position = position(9, Color::Black, &corner);
    for (x, y) in corner {
        position = position
            .play(Stone {
                color: Color::White,
                spot: Spot {
                    x: COORD_RANGE - x,
                    y: COORD_RANGE - y,
                },
            })
            .unwrap();
    }
    position = position
        .play(Stone {
            color: Color::Black,
            spot: Spot { x: 500, y: 500 },
        })
        .unwrap();
    assert_eq!(
        unconditionally_alive(&position, Color::Black),
        position.groups(Color::Black)[..1]
    );
    assert_eq!(
        unconditionally_alive(&position, Color::White),
        position.groups(Color::White)
    );
}

#[test]
fn an_invaded_vital_region_still_counts_without_relying_on_opponent_stones() {
    let spots: Vec<_> = CORNERS.into_iter().filter(|p| *p != (498, 166)).collect();
    let empty_region = position(3, Color::Black, &spots);
    assert_eq!(
        unconditionally_alive(&empty_region, Color::Black),
        empty_region.groups(Color::Black)
    );

    let mut occupied = Position::new(Board::new(3).unwrap())
        .play(Stone {
            color: Color::White,
            spot: Spot { x: 498, y: 166 },
        })
        .unwrap();
    for (x, y) in spots {
        occupied = occupied
            .play(Stone {
                color: Color::Black,
                spot: Spot { x, y },
            })
            .unwrap();
    }
    assert_eq!(occupied.stones().len(), 7);
    assert_eq!(
        unconditionally_alive(&occupied, Color::Black),
        occupied.groups(Color::Black)
    );
    // Filling the remaining space in this invaded eye cannot capture Black:
    // the other eye remains, and the invading chain would have no liberties.
    assert!(
        occupied
            .play(Stone {
                color: Color::White,
                spot: Spot { x: 166, y: 166 },
            })
            .is_none()
    );
    assert!(unconditionally_alive(&occupied, Color::White).is_empty());
}

#[test]
fn border_separation_uses_enemy_connectivity_at_the_exact_integer_cutoff() {
    for (size, cutoff) in [(9, 133), (13, 92), (19, 63)] {
        let board = Board::new(size).unwrap();
        for shift in [0, 1] {
            for rotation in 0..4 {
                let spots: Vec<_> = (cutoff + shift..=COORD_RANGE - board.stone_radius())
                    .step_by(board.diameter() as usize)
                    .map(|x| match rotation {
                        0 => (x, 500),
                        1 => (500, x),
                        2 => (COORD_RANGE - x, 500),
                        _ => (500, COORD_RANGE - x),
                    })
                    .collect();
                let position = position(size, Color::Black, &spots);
                let (_, regions) = regions(&position, Color::Black);
                assert_eq!(
                    regions.len(),
                    if shift == 0 { 2 } else { 1 },
                    "size={size}, shift={shift}, rotation={rotation}"
                );
                // Two regions alone are insufficient: these large strips are
                // not vital to the wall, since not every center is adjacent.
                assert!(unconditionally_alive(&position, Color::Black).is_empty());
            }
        }
    }
}

#[test]
fn exported_edge_gaps_distinguish_an_open_connection_a_tiny_eye_and_no_space() {
    for (sgf, region_count, alive) in [
        (
            include_str!("fixtures/edge-gap-open-not-certified.sgf"),
            2,
            false,
        ),
        (include_str!("fixtures/edge-gap-closed-alive.sgf"), 3, true),
        (
            include_str!("fixtures/edge-gap-no-center-not-certified.sgf"),
            2,
            false,
        ),
    ] {
        // Analysis exports contain consecutive Black placements, not alternating turns.
        let mut position = Position::new(Board::new(9).unwrap());
        for play in sgf::decode(sgf).unwrap() {
            let Play::Place(stone) = play else {
                panic!("expected a placement")
            };
            position = position.play(stone).unwrap();
        }
        let (rows, regions) = regions(&position, Color::Black);
        assert_eq!(regions.len(), region_count, "{sgf}");
        assert!(position.can_place(Spot { x: 55, y: 55 }));
        assert_eq!(
            unconditionally_alive(&position, Color::Black),
            if alive {
                position.groups(Color::Black)
            } else {
                Vec::new()
            },
            "{sgf}",
        );
        if alive {
            // The second eye consists of exactly two integer centers at the top edge.
            let eye = rows[55].iter().find(|r| r.left == 211).unwrap();
            assert_eq!(eye.right, 212);
            assert_eq!(regions[eye.region].size, 2);
        }
    }
}

#[test]
fn pruning_cascades_through_nonvital_boundaries_but_keeps_independent_life() {
    let region = |boundary: &[usize], vital: &[usize]| Region {
        boundary: boundary.to_vec(),
        vital: vital.to_vec(),
        ..Region::default()
    };
    let regions = [
        region(&[0, 1], &[0]),
        region(&[0], &[0]),
        region(&[1], &[1]),
        region(&[1, 2], &[1]),
        region(&[2, 3], &[2]),
        region(&[3], &[3]),
        region(&[3], &[3]),
    ];
    assert_eq!(prune(4, &regions), [false, false, false, true]);
    let shared = [region(&[0, 1], &[0, 1]), region(&[0, 1], &[0, 1])];
    assert_eq!(prune(2, &shared), [true, true]);
    assert_eq!(prune(2, &shared[..1]), [false, false]);
}

#[test]
fn interval_components_match_an_all_pairs_integer_point_graph() {
    for size in [1, 3, 13, 500] {
        let board = Board::new(size).unwrap();
        let low = board.stone_radius();
        let side = COORD_RANGE - 2 * low + 1;
        let d = board.diameter();
        let mut points: Vec<_> = (0..64)
            .map(|i| Spot {
                x: low + (i * 137 + i * i * 17) % side,
                y: low + (i * 71 + i * i * 29) % side,
            })
            .collect();
        for (dx, dy) in [(0, 0), (d - 1, d), (d, d), (d + 1, d)] {
            let spot = Spot {
                x: low + dx,
                y: low + dy,
            };
            if board.contains(spot) {
                points.push(spot);
            }
        }
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
        let mut labels: Vec<_> = (0..points.len()).collect();
        for (i, &a) in points.iter().enumerate() {
            for (j, &b) in points.iter().enumerate().take(i) {
                if board.adjacent(a, b) {
                    let old = labels[j];
                    let new = labels[i];
                    for label in &mut labels {
                        if *label == old {
                            *label = new;
                        }
                    }
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
            for j in 0..points.len() {
                assert_eq!(
                    actual[i] == actual[j],
                    labels[i] == labels[j],
                    "size={size}, {:?}, {:?}",
                    points[i],
                    points[j]
                );
            }
        }
    }
}

#[test]
fn empty_and_open_positions_are_not_certified() {
    for size in [1, 3, 13, 500] {
        assert!(
            unconditionally_alive(&Position::new(Board::new(size).unwrap()), Color::Black)
                .is_empty()
        );
    }
    let position = position(13, Color::Black, &[(500, 500)]);
    assert!(unconditionally_alive(&position, Color::Black).is_empty());
}
