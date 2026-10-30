use continugo_rules::{Board, Color, Game, Play, Position, Spot, Stone, sgf};

#[test]
fn prepared_candidates_match_both_colors_across_sizes_and_preserve_the_source() {
    let mut rng = 42u64;
    for size in [1, 2, 3, 9, 13, 19, 500] {
        let mut position = Position::new(Board::new(size).unwrap());
        for _ in 0..48 {
            let before = position.clone();
            let prepared = position.prepare();
            let mut chosen = None;
            for _ in 0..12 {
                rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                let spot = Spot {
                    x: ((rng >> 32) % 1000) as i32,
                    y: ((rng >> 16) % 1000) as i32,
                };
                for color in [Color::Black, Color::White] {
                    let stone = Stone { color, spot };
                    let next = prepared.play(stone);
                    assert_eq!(next, position.play(stone), "size={size}, {stone:?}");
                    if next.is_some() {
                        chosen = next;
                    }
                }
            }
            assert_eq!(position, before);
            if let Some(next) = chosen {
                position = next;
            }
        }
    }
}

#[test]
fn capture_and_suicide_fixtures_match_with_fresh_analysis_after_every_placement() {
    for fixture in [
        include_str!("../../../fixtures/rules/capture-four.txt"),
        include_str!("../../../fixtures/rules/capture-gains-liberties.txt"),
        include_str!("../../../fixtures/rules/capture-one-eyed-group.txt"),
        include_str!("../../../fixtures/rules/one-eyed-group-survives.txt"),
        include_str!("../../../fixtures/rules/suicide.txt"),
    ] {
        let record = fixture
            .lines()
            .find_map(|line| line.strip_prefix("record: "))
            .unwrap();
        let attempt = fixture
            .lines()
            .find_map(|line| line.strip_prefix("move: "))
            .unwrap();
        let mut position = Position::default();
        for play in sgf::decode(record)
            .unwrap()
            .into_iter()
            .chain([sgf::decode_play(attempt).unwrap()])
        {
            if let Play::Place(stone) = play {
                let next = position.prepare().play(stone);
                assert_eq!(next, position.play(stone), "{stone:?}");
                if let Some(next) = next {
                    position = next;
                }
            }
        }
    }
}

#[test]
fn prepared_games_are_immutable_snapshots_and_preserve_turns_errors_and_history() {
    let game = Game::default().play(Spot { x: 200, y: 200 }).unwrap();
    let prepared = game.prepare();
    let child = game.play(Spot { x: 400, y: 400 }).unwrap();
    for spot in [
        Spot { x: 200, y: 200 },
        Spot { x: 276, y: 200 }, // Exact touching is legal.
        Spot { x: 275, y: 200 }, // Strict overlap is illegal.
        Spot { x: 400, y: 400 },
        Spot {
            x: i32::MIN,
            y: i32::MAX,
        },
    ] {
        assert_eq!(prepared.play(spot), game.play(spot));
    }
    assert_ne!(
        prepared.play(Spot { x: 600, y: 600 }).unwrap().record(),
        child.record()
    );
    let finished = child.pass().unwrap().pass().unwrap();
    assert_eq!(
        finished.prepare().play(Spot { x: 600, y: 600 }),
        finished.play(Spot { x: 600, y: 600 })
    );
}
