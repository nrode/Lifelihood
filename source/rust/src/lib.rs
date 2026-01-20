//! Lifelihood - Life history modeling using continuous-time multi-event models
//!
//! This library provides the computational core for the lifelihood R package.
//! It implements maximum likelihood estimation for life history models including
//! mortality, maturity, and reproduction events.
//!
//! # Architecture
//!
//! The library is organized into the following modules:
//!
//! - `rng`: Random number generation (Marsaglia PRNG)
//! - `math`: Mathematical functions (special functions, integration, matrix operations)
//! - `model`: Data structures and model computations (distributions, events, likelihood)
//! - `optim`: Optimization algorithms (simulated annealing, MCMC, Hessian)
//! - `io`: Input/output handling (file parsing, output writing)

pub mod rng;
pub mod math;
pub mod model;
pub mod optim;
pub mod io;

// Re-export commonly used types
pub use model::types::*;
pub use rng::Marsaglia;
