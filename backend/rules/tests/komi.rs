use continugo_rules::{Board, Color, Game, Spot, sgf, territory::Outcome};

#[test]
fn komi_changes_score_and_survives_prepared_children_and_passes() {
    let game = Game::new(Board::new(9).unwrap()).with_komi(6.3).unwrap();
    assert_ne!(game, Game::new(game.board()));
    assert_eq!(
        game.result(),
        Outcome::Win(Color::White, 6.3 * 1_000_000.0 / 81.0)
    );
    let child = game.prepare().play(Spot { x: 500, y: 500 }).unwrap();
    let finished = child.pass().unwrap().pass().unwrap();
    assert_eq!(finished.komi(), 6.3);
    assert_eq!(finished, finished.clone());
    assert_ne!(game, game.clone().with_komi(6.300_000_1).unwrap());
    assert_eq!(Game::default(), Game::default().with_komi(-0.0).unwrap());
    assert_eq!(
        sgf::format_result(finished.board(), finished.result()),
        "B+74.7"
    );
    for komi in [f64::NEG_INFINITY, f64::NAN, f64::INFINITY] {
        assert!(game.clone().with_komi(komi).is_err());
    }
}

#[test]
fn sgf_round_trips_root_komi_and_rejects_invalid_overrides() {
    let mut record = sgf::Record::default();
    assert_eq!(record.komi().unwrap(), 0.0);
    record.set_komi(6.3).unwrap();
    assert_eq!(
        sgf::decode_record(&sgf::encode_record(&record).unwrap()).unwrap(),
        record
    );
    for property in ["KM[NaN]", "KM[inf]", "KM[-inf]", "KM[7][8]", "KM[7]KM[8]"] {
        assert!(sgf::decode_record(&format!("(;FF[9071]GM[9071]SZ[9]{property})")).is_err());
    }
    assert!(sgf::decode_record("(;FF[9071]GM[9071]SZ[9];B[]KM[7])").is_err());
    for komi in [-100.0, -6.3, 14.5, 100.0, f64::MAX] {
        let game = Game::default().with_komi(komi).unwrap();
        assert_eq!(game.pass().unwrap().komi(), komi);
        record.set_komi(komi).unwrap();
        let decoded = sgf::decode_record(&sgf::encode_record(&record).unwrap()).unwrap();
        assert_eq!(decoded.komi().unwrap(), komi);
    }
}
