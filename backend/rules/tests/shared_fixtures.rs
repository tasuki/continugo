// Read the same files as Elm, but decode and check them independently.
use continugo_rules::{Color, Game, Play, Position, sgf};
use std::{fs, path::Path};

fn field<'a>(line: &'a str, key: &str) -> Result<&'a str, String> {
    line.strip_prefix(&format!("{key}:"))
        .map(str::trim)
        .ok_or_else(|| format!("Expected {key}: field, got: {line}"))
}

fn check_fixture(contents: &str) -> Result<(), String> {
    let lines: Vec<_> = contents.trim().lines().collect();
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

    let plays = sgf::decode(record)?;
    if sgf::encode(&plays)? != record {
        return Err("Record did not round-trip through SGF".into());
    }
    let mut position = Position::default();
    for play in plays {
        // Match Elm's replayPlacements: allow setup-like sequences and skip
        // passes and illegal placements, but never malformed record entries.
        let next = match play {
            Play::Place(stone) => position.play(stone),
            Play::Pass(_) => None,
        };
        if let Some(next) = next {
            position = next;
        }
    }
    let stone = match sgf::decode_play(attempted_move)? {
        Play::Place(stone) => stone,
        Play::Pass(_) => return Err("Expected a placement, not a pass".into()),
    };
    let actual = position
        .play(stone)
        .map(|next| next.stones().len() as isize - position.stones().len() as isize);
    if actual != expected {
        return Err(format!("{name}: expected {expected:?}, got {actual:?}"));
    }
    Ok(())
}

#[test]
fn shared_placement_fixtures() {
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
fn a_shared_capture_survives_strict_game_restoration() {
    let contents = include_str!("../../../fixtures/rules/capture-four.txt");
    let lines: Vec<_> = contents.lines().collect();
    let record = sgf::decode(field(lines[1], "record").unwrap()).unwrap();
    let Play::Place(stone) = sgf::decode_play(field(lines[2], "move").unwrap()).unwrap() else {
        panic!("Expected a placement in capture-four.txt");
    };
    let before = Game::from_record(&record).unwrap();
    let after = before.play(stone.spot).unwrap();
    assert_eq!(
        after.position().stones().len() as isize - before.position().stones().len() as isize,
        -3
    );
    assert_eq!(after.on_move(), Color::White);
    assert_eq!(Game::from_record(after.record()), Ok(after));
}

#[test]
fn malformed_fixture_records_and_pass_attempts_are_rejected() {
    assert!(check_fixture("name: Bad\nrecord: B[bbbb];bad\nmove: W[mmmm]\nresult: 1").is_err());
    assert!(check_fixture("name: Bad\nrecord: B[bbbb];\nmove: W[mmmm]\nresult: 1").is_err());
    assert!(check_fixture("name: Bad\nrecord:\nmove: W[]\nresult: illegal").is_err());
}
