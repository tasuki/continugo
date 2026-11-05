//! Position analysis built on the shared rules. These queries neither change
//! move legality nor decide which legal moves a bot should choose.

pub mod availability;
pub mod benson;
pub mod eyes;

pub use availability::GeometricAvailability;
pub use benson::unconditionally_alive;
pub use eyes::{SmallEyeAnalysis, fills_small_eye};
