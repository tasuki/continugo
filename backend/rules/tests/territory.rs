use continugo_rules::{
    Color, Play, Stone, sgf,
    territory::{self, Outcome},
};
use std::{fs, path::Path};

const TOLERANCE: f64 = 0.00001;

fn field<'a>(line: &'a str, key: &str) -> Result<&'a str, String> {
    line.strip_prefix(&format!("{key}:"))
        .map(str::trim)
        .ok_or_else(|| format!("Expected {key}: field, got: {line}"))
}

fn close(expected: f64, actual: f64) -> bool {
    actual.is_finite() && (expected - actual).abs() <= TOLERANCE
}

fn check_fixture(contents: &str) -> Result<(), String> {
    let lines: Vec<_> = contents.trim().lines().collect();
    let [name, sites, black, white] = lines.as_slice() else {
        return Err("Expected exactly four lines: name, stones, black, white".into());
    };
    let name = field(name, "name")?;
    let mut stones: Vec<Stone> = Vec::new();
    for play in sgf::decode(field(sites, "stones")?)? {
        let Play::Place(stone) = play else {
            return Err("Scoring fixtures cannot contain passes".into());
        };
        if stones.iter().any(|other| other.spot == stone.spot) {
            return Err("Scoring fixtures must contain unique stone centres".into());
        }
        stones.push(stone);
    }
    let expected_black: f64 = field(black, "black")?
        .parse()
        .map_err(|_| "Invalid black area")?;
    let expected_white: f64 = field(white, "white")?
        .parse()
        .map_err(|_| "Invalid white area")?;
    if [expected_black, expected_white]
        .iter()
        .any(|area| !area.is_finite() || *area < 0.0)
    {
        return Err("Expected areas must be finite and non-negative".into());
    }
    let actual = territory::areas(&stones);
    if !close(expected_black, actual.black) || !close(expected_white, actual.white) {
        return Err(format!(
            "{name}: expected ({expected_black}, {expected_white}), got {actual:?}"
        ));
    }
    let expected_margin = expected_black - expected_white;
    match territory::result(actual) {
        Outcome::Draw if expected_margin == 0.0 => Ok(()),
        Outcome::Win(color, margin) if expected_margin != 0.0 => {
            let expected_color = if expected_margin > 0.0 {
                Color::Black
            } else {
                Color::White
            };
            if color == expected_color && close(expected_margin.abs(), margin) {
                Ok(())
            } else {
                Err(format!(
                    "{name}: wrong winner or margin: {color:?}, {margin}"
                ))
            }
        }
        outcome => Err(format!("{name}: unexpected outcome: {outcome:?}")),
    }
}

#[test]
fn shared_territory_fixtures() {
    let directory = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/territory");
    let mut files: Vec<_> = fs::read_dir(directory)
        .expect("Cannot read shared territory fixtures")
        .map(|entry| entry.expect("Cannot read fixture directory entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "txt"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "No shared territory fixtures found");
    for file in files {
        let contents = fs::read_to_string(&file).expect("Cannot read fixture");
        if let Err(error) = check_fixture(&contents) {
            panic!("{}: {error}", file.display());
        }
    }
}

#[test]
fn scoring_fixtures_reject_malformed_positions_and_expectations() {
    for sites in ["B[]", "bad", "B[bbbb];W[bbbb]"] {
        assert!(check_fixture(&format!("name: Bad\nstones: {sites}\nblack: 0\nwhite: 0")).is_err());
    }
    assert!(check_fixture("name: Bad\nstones:\nblack: NaN\nwhite: 0").is_err());
}
