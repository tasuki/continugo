use continugo_rules::{Board, Color, Position, Spot, Stone};

#[test]
fn local_spans_match_exhaustive_fixed_obstacle_geometry() {
    for size in [1, 3, 9, 13, 19, 500] {
        let board = Board::new(size).unwrap();
        let r = board.stone_radius();
        let mut position = Position::new(board);
        for (i, spot) in [
            Spot { x: r, y: r },
            Spot { x: 500, y: 500 },
            Spot { x: 800, y: 200 },
        ]
        .into_iter()
        .enumerate()
        {
            if let Some(next) = position.play(Stone {
                color: if i % 2 == 0 {
                    Color::Black
                } else {
                    Color::White
                },
                spot,
            }) {
                position = next;
            }
        }
        let before = position.clone();
        let prepared = position.prepare();
        for spot in [
            Spot { x: r, y: r },
            Spot { x: 500, y: 500 },
            Spot {
                x: 500 + board.diameter(),
                y: 500,
            },
        ] {
            let spans = prepared.local_liberty_spans(spot);
            let mut streamed = Vec::new();
            prepared.visit_local_liberty_spans(spot, |s| streamed.push(s));
            assert_eq!(spans, streamed);
            let mut actual = vec![false; 1_000_000];
            for span in spans {
                for x in span.left..=span.right {
                    actual[(span.y * 1000 + x) as usize] = true;
                }
            }
            for y in (0..1000).step_by(31) {
                for x in (0..1000).step_by(31) {
                    assert_eq!(
                        prepared.is_local_liberty_center(spot, Spot { x, y }),
                        actual[(y * 1000 + x) as usize],
                        "point query size={size}, p={spot:?}, q=({x},{y})"
                    );
                }
            }
            for s in prepared.local_liberty_spans(spot) {
                assert!(prepared.is_local_liberty_center(spot, Spot { x: s.left, y: s.y }));
                assert!(prepared.is_local_liberty_center(spot, Spot { x: s.right, y: s.y }));
            }
            let reach = 2 * board.diameter();
            for y in (spot.y - reach).max(0)..=(spot.y + reach).min(999) {
                for x in (spot.x - reach).max(0)..=(spot.x + reach).min(999) {
                    let q = Spot { x, y };
                    let expected = board.contains(spot)
                        && board.contains(q)
                        && board.adjacent(spot, q)
                        && !board.overlaps(spot, q)
                        && position.stones().iter().all(|s| !board.overlaps(s.spot, q));
                    assert_eq!(
                        actual[(y * 1000 + x) as usize],
                        expected,
                        "size={size}, p={spot:?}, q={q:?}"
                    );
                }
            }
        }
        assert!(
            prepared
                .local_liberty_spans(Spot {
                    x: i32::MIN,
                    y: i32::MAX
                })
                .is_empty()
        );
        assert_eq!(position, before);
    }
}
