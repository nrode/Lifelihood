//! Link and delink functions
//!
//! Transform parameters between bounded and unbounded spaces.

use crate::math::constants::MINUS;

/// Link function: transform unbounded value to bounded parameter space
///
/// Maps x ∈ (-∞, +∞) to [min_bound, max_bound] using the logistic function:
/// link(x) = min + (max - min) / (1 + exp(-x))
///
/// # Arguments
///
/// * `x` - Unbounded value
/// * `min_bound` - Minimum bound of the parameter
/// * `max_bound` - Maximum bound of the parameter
///
/// # Returns
///
/// Value in [min_bound, max_bound]
pub fn link(x: f64, min_bound: f64, max_bound: f64) -> f64 {
    if x > 200.0 {
        max_bound - MINUS
    } else if x < -200.0 {
        min_bound + MINUS
    } else {
        min_bound + (max_bound - min_bound) / (1.0 + (-x).exp())
    }
}

/// Delink function: transform bounded value to unbounded space
///
/// Inverse of link function. Maps val ∈ [min_bound, max_bound] to (-∞, +∞):
/// delink(val) = ln((val - min) / (max - val))
///
/// # Arguments
///
/// * `val` - Value in bounded space
/// * `min_bound` - Minimum bound of the parameter
/// * `max_bound` - Maximum bound of the parameter
///
/// # Returns
///
/// Unbounded value in (-∞, +∞)
pub fn delink(val: f64, min_bound: f64, max_bound: f64) -> f64 {
    if val <= min_bound {
        -200.0
    } else if val >= max_bound {
        200.0
    } else {
        ((val - min_bound) / (max_bound - val)).ln()
    }
}

/// Apply link transformation to a vector of values
pub fn link_vec(x: &[f64], bounds: &[(f64, f64)]) -> Vec<f64> {
    x.iter()
        .zip(bounds.iter())
        .map(|(&xi, &(min, max))| link(xi, min, max))
        .collect()
}

/// Apply delink transformation to a vector of values
pub fn delink_vec(val: &[f64], bounds: &[(f64, f64)]) -> Vec<f64> {
    val.iter()
        .zip(bounds.iter())
        .map(|(&v, &(min, max))| delink(v, min, max))
        .collect()
}

/// Compute the derivative of the link function
///
/// d/dx link(x) = (max - min) * exp(-x) / (1 + exp(-x))^2
pub fn link_derivative(x: f64, min_bound: f64, max_bound: f64) -> f64 {
    if x.abs() > 200.0 {
        MINUS
    } else {
        let exp_neg_x = (-x).exp();
        let denom = 1.0 + exp_neg_x;
        (max_bound - min_bound) * exp_neg_x / (denom * denom)
    }
}

/// Compute the derivative of the delink function
///
/// d/dval delink(val) = (max - min) / ((val - min) * (max - val))
pub fn delink_derivative(val: f64, min_bound: f64, max_bound: f64) -> f64 {
    let diff_low = val - min_bound;
    let diff_high = max_bound - val;

    if diff_low <= MINUS || diff_high <= MINUS {
        1.0 / MINUS  // Large value for numerical stability
    } else {
        (max_bound - min_bound) / (diff_low * diff_high)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_link_bounds() {
        // Test that link maps to correct bounds
        let min = 0.0;
        let max = 10.0;

        // Very negative x should map near min
        assert!((link(-100.0, min, max) - min).abs() < 0.01);

        // Very positive x should map near max
        assert!((link(100.0, min, max) - max).abs() < 0.01);

        // x = 0 should map to midpoint
        assert!((link(0.0, min, max) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_delink_inverse() {
        let min = 1.0;
        let max = 100.0;

        for x in [-10.0, -1.0, 0.0, 1.0, 10.0] {
            let linked = link(x, min, max);
            let delinked = delink(linked, min, max);
            assert!(
                (x - delinked).abs() < 1e-10,
                "x={}, linked={}, delinked={}",
                x,
                linked,
                delinked
            );
        }
    }

    #[test]
    fn test_link_monotonic() {
        let min = 0.0;
        let max = 1.0;

        let mut prev = link(-100.0, min, max);
        for i in -99..=100 {
            let x = i as f64;
            let curr = link(x, min, max);
            assert!(curr >= prev, "link should be monotonically increasing");
            prev = curr;
        }
    }

    #[test]
    fn test_delink_bounds() {
        let min = 0.0;
        let max = 10.0;

        // Values at bounds should map to extreme values
        assert!(delink(min, min, max) <= -100.0);
        assert!(delink(max, min, max) >= 100.0);

        // Midpoint should map to 0
        assert!((delink(5.0, min, max) - 0.0).abs() < 1e-10);
    }
}
