use super::*;

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
