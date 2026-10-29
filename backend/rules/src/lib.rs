//! ContinuGo rules for a 1000-unit board. The default is the 13x13 equivalent;
//! `Board::new(size)` configures other sizes, deriving stone radius from size.
//!
//! `Position` handles placements of either colour. `Game` adds alternating
//! turns, records, and two-pass completion. `territory` provides Voronoi area
//! scoring. Ko is not implemented yet.

pub mod board;
pub mod game;
pub mod position;
pub mod sgf;
pub mod territory;

pub use board::{Board, COORD_RANGE};
pub use game::Game;
pub use position::{Color, Play, Position, Spot, Stone};
