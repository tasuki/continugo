// Read the same files as Elm, validating complete game histories independently.
use continugo_rules::{Board, Game, Play, sgf};
use std::{fs, path::Path};

fn field<'a>(line: &'a str, key: &str) -> Result<&'a str, String> {
    line.strip_prefix(&format!("{key}:"))
        .map(str::trim)
        .ok_or_else(|| format!("Expected {key}: field, got: {line}"))
}

fn check_fixture(contents: &str) -> Result<(), String> {
    let mut lines: Vec<_> = contents.trim().lines().collect();
    let board = if lines.first().is_some_and(|line| line.starts_with("board:")) {
        let size = field(lines.remove(0), "board")?
            .parse()
            .map_err(|_| "Board size must be an integer".to_string())?;
        Board::new(size)?
    } else {
        Board::default()
    };
    let [name, record, attempted_move, result] = lines.as_slice() else {
        return Err("Expected exactly four lines: name, record, move, result".into());
    };
    let name = field(name, "name")?;
    let record = field(record, "record")?;
    let attempted_move = field(attempted_move, "move")?;
    let result = field(result, "result")?;
    let expected = if result == "illegal" {
        None
    } else {
        Some(
            result
                .parse::<isize>()
                .map_err(|_| format!("Invalid result: {result}"))?,
        )
    };

    let mut plays = sgf::decode(record)?;
    if sgf::decode(&sgf::encode(&plays)?)? != plays {
        return Err("Record did not round-trip through SGF".into());
    }
    let game = Game::from_record_on(board, &plays)?;
    let stone = match sgf::decode_play(attempted_move)? {
        Play::Place(stone) => stone,
        Play::Pass(_) => return Err("Expected a placement, not a pass".into()),
    };
    if stone.color != game.on_move() {
        return Err(format!("{name}: wrong attempted colour"));
    }
    let next = game.play(stone.spot);
    assert_eq!(
        game.prepare().play(stone.spot),
        next,
        "{name}: prepared play"
    );
    plays.push(Play::Place(stone));
    let restored = Game::from_record_on(board, &plays);
    match &next {
        Ok(after) => assert_eq!(restored, Ok(after.clone()), "{name}: restoration"),
        Err(error) => assert_eq!(
            restored,
            Err(format!("Move {}: {error}", plays.len())),
            "{name}: illegal import"
        ),
    }
    let actual = next.ok().map(|after| {
        after.position().stones().len() as isize - game.position().stones().len() as isize
    });
    if actual != expected {
        return Err(format!("{name}: expected {expected:?}, got {actual:?}"));
    }
    Ok(())
}

#[test]
fn shared_rule_fixtures() {
    let directory = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/rules");
    let mut files: Vec<_> = fs::read_dir(directory)
        .expect("Cannot read shared rule fixtures")
        .map(|entry| entry.expect("Cannot read fixture directory entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "txt"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "No shared rule fixtures found");

    for file in files {
        let contents = fs::read_to_string(&file).expect("Cannot read fixture");
        if let Err(error) = check_fixture(&contents) {
            panic!("{}: {error}", file.display());
        }
    }
}

#[test]
fn malformed_or_invalid_fixture_histories_and_pass_attempts_are_rejected() {
    for record in [
        "B[bbbb];bad",
        "B[bbbb];",
        "W[bbbb]",
        "B[bbbb];B[mmmm]",
        "B[aaaa]",
    ] {
        assert!(
            check_fixture(&format!(
                "name: Bad\nrecord: {record}\nmove: W[mmmm]\nresult: 1"
            ))
            .is_err()
        );
    }
    assert!(check_fixture("name: Bad\nrecord: B[bbbb]\nmove: B[mmmm]\nresult: 1").is_err());
    assert!(check_fixture("name: Bad\nrecord:\nmove: W[]\nresult: illegal").is_err());
    assert_eq!(
        check_fixture("board: 0\nname: Bad\nrecord:\nmove: B[bbbb]\nresult: 1"),
        Err("Board size must be between 1 and 500, got: 0".into())
    );
}
