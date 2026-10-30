//! Position analysis built on the shared rules. These queries neither change
//! move legality nor decide which legal moves a bot should choose.

pub mod eyes;

pub use eyes::fills_small_eye;
