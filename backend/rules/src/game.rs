//! Normal games start with Black and alternate turns, including passes.
//! Moves return a new game, leaving the original unchanged on success or error.
//! Records are chronological, matching `sgf`. Import rejects wrong colours,
//! illegal placements, and moves after two consecutive passes. Setup/handicap
//! sequences belong at the `Position` level instead. Ko is not enforced yet.
//! Use `Game::new(board)` and `from_record_on(board, record)` for non-default
//! boards: a play list does not include the SGF root's board configuration.
//!
//! ```
//! use continugo_rules::{Color, Game, Spot, sgf};
//!
//! let game = Game::default().play(Spot { x: 200, y: 200 })?;
//! assert_eq!(game.on_move(), Color::White);
//! let game = game.pass()?.pass()?;
//! assert!(game.is_finished());
//! let record = sgf::encode(game.record())?;
//! assert_eq!(Game::from_record(&sgf::decode(&record)?)?, game);
//! # Ok::<(), String>(())
//! ```

use crate::position::PreparedGroups;
use crate::{Board, Color, Play, Position, Spot, Stone};

/// Owned immutable snapshot for validating many candidates without stale caches.
/// Children are ordinary games; preparing a child builds fresh analysis.
pub struct PreparedGame {
    game: Game,
    groups: PreparedGroups,
}

impl PreparedGame {
    pub fn play(&self, spot: Spot) -> Result<Game, String> {
        self.game
            .play_inner(spot, |stone| self.groups.play(&self.game.position, stone))
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Game {
    position: Position,
    record: Vec<Play>,
}

impl Game {
    pub fn new(board: Board) -> Self {
        Self {
            position: Position::new(board),
            record: Vec::new(),
        }
    }

    pub fn board(&self) -> Board {
        self.position.board()
    }

    pub fn position(&self) -> &Position {
        &self.position
    }

    pub fn record(&self) -> &[Play] {
        &self.record
    }

    pub fn on_move(&self) -> Color {
        self.record
            .last()
            .map_or(Color::Black, |play| play.color().other())
    }

    pub fn is_finished(&self) -> bool {
        matches!(self.record.as_slice(), [.., Play::Pass(_), Play::Pass(_)])
    }

    pub fn prepare(&self) -> PreparedGame {
        PreparedGame {
            game: self.clone(),
            groups: PreparedGroups::new(&self.position),
        }
    }

    pub fn play(&self, spot: Spot) -> Result<Self, String> {
        self.play_inner(spot, |stone| self.position.play(stone))
    }

    fn play_inner(
        &self,
        spot: Spot,
        validate: impl FnOnce(Stone) -> Option<Position>,
    ) -> Result<Self, String> {
        if self.is_finished() {
            return Err("Game is finished".into());
        }
        let stone = Stone {
            color: self.on_move(),
            spot,
        };
        let position = validate(stone)
            .ok_or_else(|| format!("Illegal placement at ({}, {})", spot.x, spot.y))?;
        Ok(self.advance(Play::Place(stone), position))
    }

    pub fn pass(&self) -> Result<Self, String> {
        if self.is_finished() {
            return Err("Game is finished".into());
        }
        Ok(self.advance(Play::Pass(self.on_move()), self.position.clone()))
    }

    fn advance(&self, play: Play, position: Position) -> Self {
        let mut record = self.record.clone();
        record.push(play);
        Self { position, record }
    }

    /// Import a chronological record, rejecting the first invalid move.
    pub fn from_record(record: &[Play]) -> Result<Self, String> {
        Self::from_record_on(Board::default(), record)
    }

    pub fn from_record_on(board: Board, record: &[Play]) -> Result<Self, String> {
        let mut game = Self::new(board);
        for (index, &play) in record.iter().enumerate() {
            if play.color() != game.on_move() {
                return Err(format!(
                    "Move {}: expected {:?}, got {:?}",
                    index + 1,
                    game.on_move(),
                    play.color()
                ));
            }
            let next = match play {
                Play::Place(stone) => game.play(stone.spot),
                Play::Pass(_) => game.pass(),
            };
            game = next.map_err(|error| format!("Move {}: {error}", index + 1))?;
        }
        Ok(game)
    }
}
