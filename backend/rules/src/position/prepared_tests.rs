use super::*;

#[test]
fn group_blockers_are_lazy_reused_and_do_not_change_move_or_profile_results() {
    let position = Position::default()
        .play(Stone {
            color: Color::Black,
            spot: Spot { x: 38, y: 38 },
        })
        .unwrap();
    let prepared = position.prepare();
    assert!(prepared.liberty_spans(&[]).is_empty());
    let empty = prepared.analyze_group(&[]);
    assert!(empty.liberty_spans.is_empty());
    assert!(empty.capture_in_one.is_none());
    assert!(prepared.blockers.get().is_none());
    let stone = Stone {
        color: Color::White,
        spot: Spot { x: 500, y: 500 },
    };
    let before = prepared.analyze_move(stone, 4).unwrap();
    assert!(prepared.blockers.get().is_none());
    let members = position.groups(Color::Black).remove(0);
    let spans = prepared.liberty_spans(&members);
    let blockers = prepared.blockers.get().unwrap() as *const _;
    assert_eq!(spans, position.liberty_spans(&members));
    assert_eq!(spans, prepared.liberty_spans(&members));
    assert_eq!(blockers, prepared.blockers.get().unwrap() as *const _);
    let ordinary = prepared.analyze_group(&members);
    assert_eq!(ordinary.liberty_spans, spans);
    let (profiled, timings) = prepared.analyze_group_profiled(&members);
    assert_eq!(blockers, prepared.blockers.get().unwrap() as *const _);
    assert_eq!(ordinary.liberty_spans, position.liberty_spans(&members));
    assert_eq!(ordinary.liberty_spans, profiled.liberty_spans);
    assert_eq!(ordinary.capture_in_one, profiled.capture_in_one);
    assert!(timings.liberty_spans_seconds >= 0.0);
    assert!(timings.capturability_seconds >= 0.0);
    let after = prepared.analyze_move(stone, 4).unwrap();
    assert_eq!(before.position, after.position);
    assert_eq!(before.liberties, after.liberties);
    assert_eq!(Some(after.position), position.play(stone));
}

#[test]
fn capped_liberties_are_distinct_exact_coordinates_not_cached_witness_counts() {
    let board = Board::new(3).unwrap();
    let mut position = Position::new(board);
    for y in [166, 498, 830] {
        for x in [166, 498, 830] {
            if (x, y) != (166, 166) {
                position.stones.push(Stone {
                    color: Color::Black,
                    spot: Spot { x, y },
                });
            }
        }
    }
    let members = position.groups(Color::Black).remove(0);
    let mut oracle = Vec::new();
    for y in 0..1000 {
        for x in 0..1000 {
            let spot = Spot { x, y };
            if position.can_place(spot)
                && members
                    .iter()
                    .any(|&i| board.adjacent(position.stones[i].spot, spot))
            {
                oracle.push(spot);
            }
        }
    }
    assert_eq!(oracle, vec![Spot { x: 166, y: 166 }]);
    for cap in [0, 1, 2, 4, 8] {
        assert_eq!(
            position.group_liberties(&members, cap).len(),
            oracle.len().min(cap)
        );
    }
    assert!(
        position
            .prepare()
            .analyze_move(
                Stone {
                    color: Color::Black,
                    spot: oracle[0]
                },
                4
            )
            .is_none()
    );
}

#[test]
fn one_move_capturability_matches_exhaustive_legal_placements_on_dense_positions() {
    let board = Board::new(3).unwrap();
    for pattern in [0u16, 0x155, 0x54] {
        for holes in [vec![0], vec![4], vec![8], vec![0, 8]] {
            let stones = (0..9)
                .filter(|i| !holes.contains(i))
                .map(|i| Stone {
                    color: if pattern & (1 << i) == 0 {
                        Color::Black
                    } else {
                        Color::White
                    },
                    spot: Spot {
                        x: 166 + 332 * (i % 3),
                        y: 166 + 332 * (i / 3),
                    },
                })
                .collect();
            let position = Position { board, stones };
            let prepared = position.prepare();
            let free: Vec<_> = (0..1000)
                .flat_map(|y| (0..1000).map(move |x| Spot { x, y }))
                .filter(|&spot| position.can_place(spot))
                .collect();
            for color in [Color::Black, Color::White] {
                for members in position.groups(color) {
                    let removes_group = |next: &Position| {
                        members
                            .iter()
                            .all(|&i| !next.stones.contains(&position.stones[i]))
                    };
                    let oracle = free.iter().copied().find(|&spot| {
                        position
                            .play(Stone {
                                color: color.other(),
                                spot,
                            })
                            .is_some_and(|next| removes_group(&next))
                    });
                    assert_eq!(
                        prepared.liberty_spans(&members),
                        position.liberty_spans(&members)
                    );
                    let analysis = prepared.analyze_group(&members);
                    assert_eq!(
                        analysis.capture_in_one.is_some(),
                        oracle.is_some(),
                        "pattern={pattern}, holes={holes:?}, group={members:?}"
                    );
                    if let Some(spot) = analysis.capture_in_one {
                        assert!(removes_group(
                            &position
                                .play(Stone {
                                    color: color.other(),
                                    spot
                                })
                                .unwrap()
                        ));
                    }
                }
            }
        }
    }
}

#[test]
fn dense_groups_connections_and_simultaneous_captures_match_the_uncached_validator() {
    let board = Board::new(3).unwrap();
    for pattern in [0u16, 0x155, 0x54, 0x1ff] {
        for hole in 0..9 {
            let stones = (0..9)
                .filter(|&i| i != hole)
                .map(|i| Stone {
                    color: if pattern & (1 << i) == 0 {
                        Color::Black
                    } else {
                        Color::White
                    },
                    spot: Spot {
                        x: 166 + 332 * (i % 3),
                        y: 166 + 332 * (i / 3),
                    },
                })
                .collect();
            // Include setup positions with dead groups, not only reachable games.
            let position = Position { board, stones };
            let prepared = position.prepare();
            for color in [Color::Black, Color::White] {
                assert_eq!(
                    prepared
                        .groups(color)
                        .map(<[usize]>::to_vec)
                        .collect::<Vec<_>>(),
                    position.groups(color)
                );
                for dx in [-1, 0, 1] {
                    let stone = Stone {
                        color,
                        spot: Spot {
                            x: 166 + 332 * (hole % 3) + dx,
                            y: 166 + 332 * (hole / 3),
                        },
                    };
                    assert_eq!(
                        prepared.play(stone),
                        position.play(stone),
                        "pattern={pattern}, hole={hole}, {stone:?}"
                    );
                }
            }
        }
    }
}
