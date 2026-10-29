//! Read and write ContinuGo's compact SGF-like records, in chronological order.
//! This is not a full SGF document parser: only `B[xxxx]`, `W[xxxx]`, and passes
//! (`B[]`, `W[]`), separated by semicolons, are supported. An empty string is an
//! empty record. Malformed entries reject the entire record.
//!
//! Coordinates use two base-52 digits (`a-zA-Z`) per axis. Encoding rejects
//! coordinates outside 0..=2703; decoding checks syntax, not placement legality.
//!
//! ```
//! use continugo_rules::sgf;
//!
//! let plays = sgf::decode("B[bbbb];W[]")?;
//! assert_eq!(sgf::encode(&plays)?, "B[bbbb];W[]");
//! # Ok::<(), String>(())
//! ```

use crate::{Color, Play, Spot, Stone};

const ALPHABET: &[u8; 52] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

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
    if text.is_empty() {
        return Ok(Vec::new());
    }
    text.split(';')
        .enumerate()
        .map(|(index, entry)| {
            decode_play(entry).map_err(|_| format!("Invalid SGF entry {}: {entry}", index + 1))
        })
        .collect()
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
    plays
        .iter()
        .map(|&play| encode_play(play))
        .collect::<Result<Vec<_>, _>>()
        .map(|entries| entries.join(";"))
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
    fn known_coordinates_and_both_pass_colors_round_trip() {
        let cases = [
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
        ];
        for (plays, text) in cases {
            assert_eq!(decode(text), Ok(plays.clone()), "{text}");
            assert_eq!(encode(&plays), Ok(text.to_string()), "{plays:?}");
        }
    }

    #[test]
    fn malformed_entries_reject_the_whole_record() {
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
