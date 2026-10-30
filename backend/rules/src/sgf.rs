//! ContinuGo SGF: a move-free root followed by a single sequence of moves.
//! `FF[9071]GM[9071]` identifies our format; coordinates use two base-52 digits
//! (`a-zA-Z`) per axis, not Go's usual one. Variations are not supported.
//! Root and per-move properties (including comments) are preserved by the record API.
//! Move properties exclude B/W, which are represented separately by `plays`.
//! Empty PB, PW and RE properties are omitted when writing.
//! The play-only API discards annotations and uses a 13x13 root when writing.
//! Use the record API to round-trip comments. Decoding also
//! accepts legacy compact records. Neither API checks move legality or applies
//! board metadata: replay on the appropriate board separately.
//!
//! ```
//! use continugo_rules::sgf;
//!
//! let plays = sgf::decode("B[bbbb];W[]")?;
//! assert_eq!(sgf::encode(&plays)?,
//!     "(;FF[9071]GM[9071]SZ[13]\n;B[bbbb]\n;W[]\n)");
//! # Ok::<(), String>(())
//! ```

use crate::{Color, Play, Spot, Stone};

const ALPHABET: &[u8; 52] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

pub type Property = (String, Vec<String>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Record {
    pub root: Vec<Property>,
    pub plays: Vec<Play>,
    /// Non-move properties for each play. Decoding always aligns these with
    /// `plays`; encoding also accepts an empty vector for unannotated records.
    pub move_properties: Vec<Vec<Property>>,
}

impl Default for Record {
    fn default() -> Self {
        Self {
            root: [("FF", "9071"), ("GM", "9071"), ("SZ", "13")]
                .into_iter()
                .map(|(name, value)| (name.into(), vec![value.into()]))
                .collect(),
            plays: Vec::new(),
            move_properties: Vec::new(),
        }
    }
}

pub fn decode_play(text: &str) -> Result<Play, String> {
    let invalid = || format!("Invalid SGF move: {text}");
    let (color, coordinates) = if let Some(rest) = text.strip_prefix("B[") {
        (Color::Black, rest)
    } else if let Some(rest) = text.strip_prefix("W[") {
        (Color::White, rest)
    } else {
        return Err(invalid());
    };
    let coordinates = coordinates.strip_suffix(']').ok_or_else(invalid)?;
    if coordinates.is_empty() {
        return Ok(Play::Pass(color));
    }
    let [a, b, c, d] = coordinates.as_bytes() else {
        return Err(invalid());
    };
    let digit = |byte: u8| match byte {
        b'a'..=b'z' => Ok(i32::from(byte - b'a')),
        b'A'..=b'Z' => Ok(i32::from(byte - b'A') + 26),
        _ => Err(invalid()),
    };
    Ok(Play::Place(Stone {
        color,
        spot: Spot {
            x: digit(*a)? * 52 + digit(*b)?,
            y: digit(*c)? * 52 + digit(*d)?,
        },
    }))
}

pub fn decode(text: &str) -> Result<Vec<Play>, String> {
    decode_record(text).map(|record| record.plays)
}

pub fn decode_record(text: &str) -> Result<Record, String> {
    if !text.trim_start().starts_with('(') {
        let plays = if text.is_empty() {
            Vec::new()
        } else {
            text.split(';')
                .enumerate()
                .map(|(index, entry)| {
                    decode_play(entry)
                        .map_err(|_| format!("Invalid SGF entry {}: {entry}", index + 1))
                })
                .collect::<Result<_, _>>()?
        };
        return Ok(Record {
            move_properties: vec![Vec::new(); plays.len()],
            plays,
            ..Record::default()
        });
    }

    let mut parser = Parser { rest: text };
    parser.expect('(')?;
    parser.expect(';')?;
    let root = parser.properties()?;
    validate_root(&root)?;
    let mut plays = Vec::new();
    let mut move_properties = Vec::new();
    while parser.rest.trim_start().starts_with(';') {
        parser.expect(';')?;
        let mut properties = parser.properties()?;
        let moves: Vec<_> = properties
            .iter()
            .filter(|(name, _)| name == "B" || name == "W")
            .collect();
        let [(name, values)] = moves.as_slice() else {
            return Err("Each non-root SGF node must contain exactly one move".into());
        };
        let [value] = values.as_slice() else {
            return Err("An SGF move must have exactly one value".into());
        };
        plays.push(decode_play(&format!("{name}[{value}]"))?);
        properties.retain(|(name, _)| name != "B" && name != "W");
        move_properties.push(properties);
    }
    parser.expect(')')?;
    if !parser.rest.trim().is_empty() {
        return Err("Unexpected content after SGF tree".into());
    }
    Ok(Record {
        root,
        plays,
        move_properties,
    })
}

fn validate_root(root: &[Property]) -> Result<(), String> {
    for name in ["FF", "GM"] {
        if !root
            .iter()
            .any(|(key, values)| key == name && values == &["9071"])
        {
            return Err(format!("Expected {name}[9071] in SGF root"));
        }
    }
    if root.iter().any(|(name, _)| name == "B" || name == "W") {
        return Err("SGF root must not contain a move".into());
    }
    Ok(())
}

struct Parser<'a> {
    rest: &'a str,
}

impl Parser<'_> {
    fn expect(&mut self, ch: char) -> Result<(), String> {
        self.rest = self.rest.trim_start();
        self.rest = self
            .rest
            .strip_prefix(ch)
            .ok_or_else(|| format!("Expected '{ch}' in SGF"))?;
        Ok(())
    }

    fn properties(&mut self) -> Result<Vec<Property>, String> {
        let mut properties: Vec<Property> = Vec::new();
        loop {
            self.rest = self.rest.trim_start();
            let length = self.rest.bytes().take_while(u8::is_ascii_uppercase).count();
            if length == 0 {
                break;
            }
            let name = self.rest[..length].to_string();
            self.rest = &self.rest[length..];
            if properties.iter().any(|(key, _)| key == &name) {
                return Err(format!("Duplicate SGF property: {name}"));
            }
            let mut values = vec![self.value()?];
            while self.rest.trim_start().starts_with('[') {
                values.push(self.value()?);
            }
            properties.push((name, values));
        }
        Ok(properties)
    }

    fn value(&mut self) -> Result<String, String> {
        self.expect('[')?;
        let mut value = String::new();
        let mut chars = self.rest.chars();
        while let Some(ch) = chars.next() {
            match ch {
                ']' => {
                    self.rest = chars.as_str();
                    return Ok(value);
                }
                '\\' => match chars.next() {
                    Some('\r') => {
                        if chars.as_str().starts_with('\n') {
                            chars.next();
                        }
                    }
                    Some('\n') => {
                        if chars.as_str().starts_with('\r') {
                            chars.next();
                        }
                    }
                    Some(escaped) => value.push(escaped),
                    None => break,
                },
                '\r' => {
                    if chars.as_str().starts_with('\n') {
                        chars.next();
                    }
                    value.push('\n');
                }
                _ => value.push(ch),
            }
        }
        Err("Unterminated SGF property value".into())
    }
}

pub fn encode_play(play: Play) -> Result<String, String> {
    let (color, spot) = match play {
        Play::Place(stone) => (stone.color, Some(stone.spot)),
        Play::Pass(color) => (color, None),
    };
    let mut text = match color {
        Color::Black => String::from("B["),
        Color::White => String::from("W["),
    };
    if let Some(spot) = spot {
        for coordinate in [spot.x, spot.y] {
            if !(0..52 * 52).contains(&coordinate) {
                return Err(format!("Coordinate cannot be encoded in SGF: {coordinate}"));
            }
            text.push(ALPHABET[(coordinate / 52) as usize] as char);
            text.push(ALPHABET[(coordinate % 52) as usize] as char);
        }
    }
    text.push(']');
    Ok(text)
}

pub fn encode(plays: &[Play]) -> Result<String, String> {
    encode_record(&Record {
        plays: plays.to_vec(),
        ..Record::default()
    })
}

pub fn encode_record(record: &Record) -> Result<String, String> {
    validate_root(&record.root)?;
    if !record.move_properties.is_empty() && record.move_properties.len() != record.plays.len() {
        return Err("SGF move properties must align with plays".into());
    }
    let mut text = String::from("(;FF[9071]GM[9071]");
    write_properties(&mut text, &record.root, true)?;
    for (index, &play) in record.plays.iter().enumerate() {
        text.push_str("\n;");
        text.push_str(&encode_play(play)?);
        if let Some(properties) = record.move_properties.get(index) {
            write_properties(&mut text, properties, false)?;
        }
    }
    text.push_str("\n)");
    Ok(text)
}

fn write_properties(text: &mut String, properties: &[Property], root: bool) -> Result<(), String> {
    let mut seen = Vec::new();
    for (name, values) in properties {
        if name.is_empty()
            || !name.bytes().all(|ch| ch.is_ascii_uppercase())
            || values.is_empty()
            || seen.contains(&name)
            || (!root && matches!(name.as_str(), "B" | "W"))
        {
            return Err(format!("Invalid or duplicate SGF property: {name}"));
        }
        seen.push(name);
        if root
            && (name == "FF"
                || name == "GM"
                || (matches!(name.as_str(), "PB" | "PW" | "RE")
                    && values.iter().all(String::is_empty)))
        {
            continue;
        }
        text.push_str(name);
        for value in values {
            text.push('[');
            text.push_str(&value.replace('\\', "\\\\").replace(']', "\\]"));
            text.push(']');
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn place(color: Color, x: i32, y: i32) -> Play {
        Play::Place(Stone {
            color,
            spot: Spot { x, y },
        })
    }

    #[test]
    fn legacy_records_become_sgf_documents() {
        for (plays, compact) in [
            (vec![], ""),
            (
                vec![place(Color::Black, 100, 200), place(Color::White, 300, 400)],
                "B[bWdS];W[fOhK]",
            ),
            (
                vec![place(Color::Black, 53, 53), place(Color::White, 636, 636)],
                "B[bbbb];W[mmmm]",
            ),
            (
                vec![Play::Pass(Color::Black), Play::Pass(Color::White)],
                "B[];W[]",
            ),
            (
                vec![
                    place(Color::Black, 0, 25),
                    place(Color::White, 26, 51),
                    place(Color::Black, 52, 2703),
                ],
                "B[aaaz];W[aAaZ];B[baZZ]",
            ),
        ] {
            assert_eq!(decode(compact), Ok(plays.clone()));
            let mut expected = String::from("(;FF[9071]GM[9071]SZ[13]");
            for entry in compact.split(';').filter(|entry| !entry.is_empty()) {
                expected.push_str(&format!("\n;{entry}"));
            }
            expected.push_str("\n)");
            assert_eq!(encode(&plays), Ok(expected.clone()));
            assert_eq!(decode(&expected), Ok(plays));
        }
    }

    #[test]
    fn empty_player_names_and_result_are_omitted() {
        let record = decode_record("(;FF[9071]GM[9071]SZ[13]PB[]PW[]RE[]C[])").unwrap();
        assert_eq!(
            encode_record(&record).unwrap(),
            "(;FF[9071]GM[9071]SZ[13]C[]\n)"
        );
    }

    #[test]
    fn root_metadata_and_escaped_values_round_trip() {
        let text = "(;FF[9071]GM[9071]SZ[19]PB[A\\]lice]PW[白\\\\黒]RE[B+2.5]C[a; (b)\nline]XX[one][two]\n;B[bbbb]C[move comment]\n;W[]\n)";
        let record = decode_record(text).unwrap();
        assert_eq!(record.root[3].1, vec!["A]lice"]);
        assert_eq!(record.root[4].1, vec!["白\\黒"]);
        assert_eq!(record.root[5].1, vec!["B+2.5"]);
        assert_eq!(
            record.move_properties,
            vec![vec![("C".into(), vec!["move comment".into()])], vec![]]
        );
        assert_eq!(decode_record(&encode_record(&record).unwrap()), Ok(record));
        assert_eq!(
            decode(" \n(; GM[9071] FF[9071] ; B [bbbb] ; W [] ) \n")
                .unwrap()
                .len(),
            2
        );
        let continued = decode_record("(;FF[9071]GM[9071]PB[a\\\r\nb])").unwrap();
        assert_eq!(continued.root[2].1, vec!["ab"]);
    }

    #[test]
    fn move_comments_and_unknown_properties_round_trip_including_passes() {
        let mut record = decode_record("B[bbbb];W[]").unwrap();
        record.move_properties = vec![
            vec![
                (
                    "C".into(),
                    vec!["雪; (note) [x] \\ path\n{\"policy\":[1,2]}".into()],
                ),
                ("XX".into(), vec!["one".into(), "two".into()]),
            ],
            vec![("C".into(), vec![String::new()])],
        ];
        let encoded = encode_record(&record).unwrap();
        assert_eq!(decode_record(&encoded), Ok(record.clone()));
        assert_eq!(decode(&encoded), Ok(record.plays));
        let before_move = decode_record("(;FF[9071]GM[9071];C[before]B[bbbb])").unwrap();
        assert_eq!(before_move.move_properties[0][0].1, vec!["before"]);
    }

    #[test]
    fn encoding_rejects_misaligned_or_invalid_move_properties() {
        let mut record = decode_record("B[bbbb]").unwrap();
        for properties in [
            vec![("B".into(), vec!["mmmm".into()])],
            vec![("W".into(), vec![String::new()])],
            vec![
                ("C".into(), vec![String::new()]),
                ("C".into(), vec![String::new()]),
            ],
            vec![("comment".into(), vec![String::new()])],
            vec![("C".into(), vec![])],
        ] {
            record.move_properties = vec![properties];
            assert!(encode_record(&record).is_err());
        }
        record.move_properties = vec![vec![], vec![]];
        assert!(encode_record(&record).is_err());
    }

    #[test]
    fn malformed_records_are_rejected() {
        for text in [
            "invalid",
            "invalid;B[bbbb]",
            "B[bbbb];invalid",
            "B[bbbb];invalid;W[mmmm]",
            "B[bb]",
            "X[bbbb]",
            "B[bb1b]",
            " B[bbbb]",
            "B[bbbb]\n",
            ";B[bbbb]",
            "B[bbbb];",
            "B[bbbb];;W[mmmm]",
            "(;B[bbbb])",
            "B[ébb]",
            "(;FF[4]GM[1];B[bbbb])",
            "(;FF[9071]GM[9071]B[bbbb])",
            "(;FF[9071]GM[9071];B[bbbb]W[])",
            "(;FF[9071]GM[9071];B[bbbb][mmmm])",
            "(;FF[9071]GM[9071];B[bbbb](;W[]))",
            "(;FF[9071]GM[9071])garbage",
            "(;FF[9071]GM[9071]PB[unfinished)",
            "(;FF[9071]GM[9071]PB[a]PB[b])",
            "(;FF[9071]GM[9071];)",
            "(;FF[9071]GM[9071]",
            "(;FF[9071]GM[9071])()",
        ] {
            assert!(decode(text).is_err(), "Accepted {text:?}");
        }
        assert_eq!(
            decode("B[bbbb];invalid;W[mmmm]"),
            Err("Invalid SGF entry 2: invalid".into())
        );
    }

    #[test]
    fn encoding_rejects_unrepresentable_coordinates() {
        for coordinate in [-1, 2704, i32::MIN, i32::MAX] {
            assert!(encode(&[place(Color::Black, coordinate, 200)]).is_err());
            assert!(encode(&[place(Color::White, 200, coordinate)]).is_err());
        }
    }
}
