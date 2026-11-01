//! Static-skill Glicko for immutable bots, following Glickman's original update:
//! https://www.glicko.net/glicko/glicko.pdf
//!
//! There is NO elapsed-time, inactivity or per-period process-noise inflation.
//! Empty evidence leaves a rating unchanged. A deviation of zero denotes a fixed
//! reference; its rating never changes. For simultaneous updates, every opponent
//! must use its PRE-period rating, not a rating already updated by this period.
//! The default 100 is a prior centered on the reference bot, not a measured skill:
//! applications should display untested players as unrated until matches exist.

use std::f64::consts::{LN_10, PI};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rating {
    pub rating: f64,
    pub deviation: f64,
}

impl Default for Rating {
    fn default() -> Self {
        Self {
            rating: 100.0,
            deviation: 350.0,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct GameResult {
    pub opponent: Rating,
    /// 1 for a win, 0 for a loss, 0.5 for a real draw. Omit unknown outcomes.
    pub score: f64,
}

impl Rating {
    pub const REFERENCE: Self = Self {
        rating: 100.0,
        deviation: 0.0,
    };

    pub fn validate(self) -> Result<(), &'static str> {
        if !self.rating.is_finite() || !self.deviation.is_finite() || self.deviation < 0.0 {
            return Err("rating must be finite and deviation finite and nonnegative");
        }
        Ok(())
    }

    pub fn update(self, games: &[GameResult]) -> Result<Self, &'static str> {
        self.validate()?;
        let q = LN_10 / 400.0;
        let mut information = 0.0;
        let mut gradient = 0.0;
        for game in games {
            game.opponent.validate()?;
            if !game.score.is_finite() || !(0.0..=1.0).contains(&game.score) {
                return Err("score must be finite and in 0..=1");
            }
            let g = 1.0 / (1.0 + 3.0 * (q * game.opponent.deviation / PI).powi(2)).sqrt();
            let log_odds = q * g * (self.rating - game.opponent.rating);
            // Stable logistic for both signs, including extreme rating differences.
            let expected = if log_odds >= 0.0 {
                1.0 / (1.0 + (-log_odds).exp())
            } else {
                let odds = log_odds.exp();
                odds / (1.0 + odds)
            };
            information += (q * g).powi(2) * expected * (1.0 - expected);
            gradient += q * g * (game.score - expected);
        }
        if games.is_empty() || self.deviation == 0.0 {
            return Ok(self);
        }
        let precision = self.deviation.recip().powi(2) + information;
        let result = Self {
            rating: self.rating + gradient / precision,
            deviation: precision.sqrt().recip(),
        };
        result.validate()?;
        if result.deviation == 0.0 {
            return Err("rating update exceeded numerical precision");
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn published_glicko_example_without_pre_period_inflation() {
        let player = Rating {
            rating: 1500.0,
            deviation: 200.0,
        };
        let games = [
            (1400.0, 30.0, 1.0),
            (1550.0, 100.0, 0.0),
            (1700.0, 300.0, 0.0),
        ]
        .map(|(rating, deviation, score)| GameResult {
            opponent: Rating { rating, deviation },
            score,
        });
        let result = player.update(&games).unwrap();
        assert!((result.rating - 1464.106).abs() < 0.001);
        assert!((result.deviation - 151.399).abs() < 0.001);
    }

    #[test]
    fn fixed_reference_no_inactivity_or_active_process_noise() {
        let prior = Rating::default();
        let win = GameResult {
            opponent: Rating::REFERENCE,
            score: 1.0,
        };
        let rated = prior.update(&[win; 20]).unwrap();
        assert!(rated.rating > prior.rating);
        assert!(rated.deviation < prior.deviation);
        let mut unchanged = rated;
        for _ in 0..1000 {
            unchanged = unchanged.update(&[]).unwrap();
        }
        assert_eq!(unchanged, rated);
        assert_eq!(
            Rating::REFERENCE.update(&[win; 20]).unwrap(),
            Rating::REFERENCE
        );
        let draw = GameResult {
            opponent: prior,
            score: 0.5,
        };
        let result = prior.update(&[draw; 20]).unwrap();
        assert_eq!(result.rating, prior.rating);
        assert!(result.deviation < prior.deviation);
    }

    #[test]
    fn invalid_evidence_is_rejected_even_for_fixed_references() {
        for score in [f64::NAN, f64::INFINITY, -1.0, 1.1] {
            assert!(
                Rating::REFERENCE
                    .update(&[GameResult {
                        opponent: Rating::default(),
                        score,
                    }])
                    .is_err()
            );
        }
        assert!(
            Rating {
                rating: 100.0,
                deviation: -1.0
            }
            .update(&[])
            .is_err()
        );
    }
}
