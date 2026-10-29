use continugo_rules::{Color, Game, Play, Spot, Stone, sgf};

fn restore(record: &str) -> Result<Game, String> {
    Game::from_record(&sgf::decode(record)?)
}

#[test]
fn new_games_start_empty_with_black_to_play() {
    let game = Game::default();
    assert_eq!(game.on_move(), Color::Black);
    assert!(game.position().stones().is_empty());
    assert!(game.record().is_empty());
    assert!(!game.is_finished());
    assert_eq!(Game::from_record(&[]), Ok(game));
}

#[test]
fn placements_and_passes_alternate_and_record_chronologically() {
    let empty = Game::default();
    let first = empty.play(Spot { x: 200, y: 200 }).unwrap();
    assert_eq!(first.on_move(), Color::White);
    assert_eq!(
        first.position().stones(),
        &[Stone {
            color: Color::Black,
            spot: Spot { x: 200, y: 200 },
        }]
    );
    let passed = first.pass().unwrap();
    assert_eq!(passed.on_move(), Color::Black);
    assert_eq!(passed.position(), first.position());
    let next = passed.play(Spot { x: 400, y: 400 }).unwrap();
    assert_eq!(next.on_move(), Color::White);
    assert_eq!(next.position().stones().len(), 2);
    assert_eq!(next.record(), sgf::decode("B[dSdS];W[];B[hKhK]").unwrap());
    assert_eq!(Game::from_record(next.record()), Ok(next));
    assert_eq!(empty, Game::default());
}

#[test]
fn rejected_placements_preserve_the_position_turn_and_record() {
    let game = Game::default().play(Spot { x: 200, y: 200 }).unwrap();
    let snapshot = game.clone();
    for spot in [
        Spot { x: 200, y: 200 },
        Spot { x: 250, y: 200 },
        Spot { x: 0, y: 0 },
    ] {
        assert!(game.play(spot).is_err());
        assert_eq!(game, snapshot);
    }
    let next = game.play(Spot { x: 400, y: 400 }).unwrap();
    assert_eq!(next.record().last().unwrap().color(), Color::White);
}

#[test]
fn two_passes_finish_the_game_and_block_all_further_turns() {
    let once = Game::default().pass().unwrap();
    assert!(!once.is_finished());
    let finished = once.pass().unwrap();
    assert!(finished.is_finished());
    assert_eq!(
        finished.record(),
        &[Play::Pass(Color::Black), Play::Pass(Color::White)]
    );
    assert_eq!(
        finished.play(Spot { x: 500, y: 500 }),
        Err("Game is finished".into())
    );
    assert_eq!(finished.pass(), Err("Game is finished".into()));
    assert_eq!(Game::from_record(finished.record()), Ok(finished));
}

#[test]
fn a_placement_interrupts_consecutive_passes() {
    let game = Game::default()
        .pass()
        .unwrap()
        .play(Spot { x: 500, y: 500 })
        .unwrap()
        .pass()
        .unwrap();
    assert!(!game.is_finished());
    assert_eq!(game.on_move(), Color::White);
    assert!(game.pass().unwrap().is_finished());
}

#[test]
fn imports_reject_non_alternating_placements_and_passes() {
    for record in [
        "W[bbbb]",
        "W[]",
        "B[bbbb];B[mmmm]",
        "B[bbbb];B[]",
        "B[];B[bbbb]",
        "B[];B[]",
    ] {
        assert!(restore(record).is_err(), "Accepted {record}");
    }
    assert_eq!(
        restore("B[];B[bbbb]"),
        Err("Move 2: expected White, got Black".into())
    );
}

#[test]
fn imports_never_skip_illegal_placements_or_turns_after_completion() {
    assert_eq!(
        restore("B[aaaa]"),
        Err("Move 1: Illegal placement at (0, 0)".into())
    );
    assert_eq!(
        restore("B[bbbb];W[bbbb];B[mmmm]"),
        Err("Move 2: Illegal placement at (53, 53)".into())
    );
    for record in ["B[];W[];B[bbbb]", "B[];W[];B[]"] {
        assert_eq!(restore(record), Err("Move 3: Game is finished".into()));
    }
}
