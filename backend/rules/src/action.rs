//! Indexed actions for policy outputs and game records. Coordinates use
//! `x + 1000*y`; pass follows the million coordinate actions. An index identifies
//! a candidate, not its legality, which is checked by `Game` when applied.

use crate::{COORD_RANGE, Game, Spot};

pub const COORDINATE_ACTIONS: u32 = (COORD_RANGE * COORD_RANGE) as u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Action(u32);

impl Action {
    pub const PASS: Self = Self(COORDINATE_ACTIONS);

    pub fn from_index(index: u32) -> Option<Self> {
        (index <= COORDINATE_ACTIONS).then_some(Self(index))
    }

    pub fn index(self) -> u32 {
        self.0
    }

    pub fn spot(self) -> Option<Spot> {
        (self != Self::PASS).then(|| Spot {
            x: (self.0 % COORD_RANGE as u32) as i32,
            y: (self.0 / COORD_RANGE as u32) as i32,
        })
    }

    pub fn apply(self, game: &Game) -> Result<Game, String> {
        match self.spot() {
            Some(spot) => game.play(spot),
            None => game.pass(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Board;

    #[test]
    fn coordinate_indices_round_trip() {
        for index in 0..=COORDINATE_ACTIONS {
            assert_eq!(Action::from_index(index).unwrap().index(), index);
        }
        assert_eq!(Action::PASS.index(), COORDINATE_ACTIONS);
        assert!(Action::from_index(COORDINATE_ACTIONS + 1).is_none());
        assert!(Action::from_index(u32::MAX).is_none());
    }

    #[test]
    fn actions_follow_the_configured_game_rules() {
        for size in [9, 13, 19] {
            let game = Game::new(Board::new(size).unwrap());
            let action = Action::from_index(200_300).unwrap();
            let spot = Spot { x: 300, y: 200 };
            let placed = action.apply(&game).unwrap();
            assert_eq!(placed, game.play(spot).unwrap());
            assert_eq!(placed.position().stones()[0].spot, spot);
            assert!(action.apply(&placed).is_err());
            assert!(Action::from_index(0).unwrap().apply(&game).is_err());
            let ended = Action::PASS
                .apply(&Action::PASS.apply(&placed).unwrap())
                .unwrap();
            assert!(ended.is_finished());
            assert!(Action::PASS.apply(&ended).is_err());
            assert!(action.apply(&ended).is_err());
        }
    }
}
