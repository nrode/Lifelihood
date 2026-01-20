//! Numerical integration routines
//!
//! Port of mathromb.pas Romberg integration algorithm.

/// Romberg numerical integration
///
/// Computes the definite integral of a function using Romberg's method,
/// which is an adaptive method based on Richardson extrapolation.
///
/// # Arguments
///
/// * `f` - The function to integrate
/// * `lower` - Lower bound of integration
/// * `upper` - Upper bound of integration
/// * `tol` - Tolerance for convergence
///
/// # Returns
///
/// The approximate value of the integral
pub fn romberg<F>(f: F, lower: f64, upper: f64, tol: f64) -> f64
where
    F: Fn(f64) -> f64,
{
    // Use fixed-size arrays like Pascal version
    let mut nx: [usize; 16] = [0; 16];
    let mut t: [f64; 136] = [0.0; 136];

    let mut pieces = 1usize;
    nx[0] = 1;

    let mut delta_x = (upper - lower) / pieces as f64;
    let c = (f(lower) + f(upper)) * 0.5;
    t[0] = delta_x * c;

    let mut n = 1usize;
    let mut nn = 2usize;
    let mut sum = c;
    let mut j = 0usize;

    loop {
        n += 1;
        let mut fotom = 4.0f64;
        nx[n - 1] = nn;
        pieces *= 2;
        let l = pieces - 1;
        delta_x = (upper - lower) / pieces as f64;

        // Compute trapezoidal sum for 2^(n-1)+1 points
        for ii in 1..=((l + 1) / 2) {
            let i = ii * 2 - 1;
            let x = lower + (i as f64) * delta_x;
            sum += f(x);
        }
        t[nn - 1] = delta_x * sum;

        let ntra = nx[n - 2];
        let k = n - 1;

        // Compute n-th row of T array
        for m in 1..=k {
            j = nn + m;
            let nt = nx[n - 2] + m - 1;
            t[j - 1] = (fotom * t[j - 2] - t[nt - 1]) / (fotom - 1.0);
            fotom *= 4.0;
        }

        // Check convergence
        if n > 4 {
            if t[nn] != 0.0 {
                let converged = (t[ntra] - t[nn]).abs() <= (t[nn] * tol).abs()
                    || (t[nn - 2] - t[j - 1]).abs() <= (t[j - 1] * tol).abs();
                if converged || n > 15 {
                    break;
                }
            }
        }

        nn = j + 1;
    }

    t[j - 1]
}

/// Simpler adaptive Simpson's rule integration
/// Used as a fallback or for simpler cases
pub fn simpson<F>(f: F, lower: f64, upper: f64, n: usize) -> f64
where
    F: Fn(f64) -> f64,
{
    let n = if n % 2 == 0 { n } else { n + 1 }; // Ensure even
    let h = (upper - lower) / n as f64;

    let mut sum = f(lower) + f(upper);

    for i in 1..n {
        let x = lower + (i as f64) * h;
        let weight = if i % 2 == 0 { 2.0 } else { 4.0 };
        sum += weight * f(x);
    }

    sum * h / 3.0
}

/// Adaptive quadrature using Simpson's rule with error estimation
pub fn adaptive_simpson<F>(f: &F, a: f64, b: f64, tol: f64, max_depth: usize) -> f64
where
    F: Fn(f64) -> f64,
{
    adaptive_simpson_inner(f, a, b, tol, simpson_rule(f, a, b), max_depth)
}

fn simpson_rule<F>(f: &F, a: f64, b: f64) -> f64
where
    F: Fn(f64) -> f64,
{
    let c = (a + b) / 2.0;
    let h = (b - a) / 6.0;
    h * (f(a) + 4.0 * f(c) + f(b))
}

fn adaptive_simpson_inner<F>(f: &F, a: f64, b: f64, tol: f64, whole: f64, depth: usize) -> f64
where
    F: Fn(f64) -> f64,
{
    let c = (a + b) / 2.0;
    let left = simpson_rule(f, a, c);
    let right = simpson_rule(f, c, b);
    let delta = left + right - whole;

    if depth == 0 || delta.abs() <= 15.0 * tol {
        left + right + delta / 15.0
    } else {
        let half_tol = tol / 2.0;
        adaptive_simpson_inner(f, a, c, half_tol, left, depth - 1)
            + adaptive_simpson_inner(f, c, b, half_tol, right, depth - 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    #[test]
    fn test_romberg_polynomial() {
        // Integral of x^2 from 0 to 1 = 1/3
        let result = romberg(|x| x * x, 0.0, 1.0, 1e-10);
        assert!((result - 1.0 / 3.0).abs() < 1e-6);
    }

    #[test]
    fn test_romberg_sin() {
        // Integral of sin(x) from 0 to pi = 2
        let result = romberg(|x| x.sin(), 0.0, PI, 1e-10);
        assert!((result - 2.0).abs() < 1e-6);
    }

    #[test]
    fn test_romberg_exp() {
        // Integral of exp(x) from 0 to 1 = e - 1
        let result = romberg(|x| x.exp(), 0.0, 1.0, 1e-10);
        let expected = std::f64::consts::E - 1.0;
        assert!((result - expected).abs() < 1e-6);
    }

    #[test]
    fn test_simpson_polynomial() {
        // Integral of x^2 from 0 to 1 = 1/3
        let result = simpson(|x| x * x, 0.0, 1.0, 100);
        assert!((result - 1.0 / 3.0).abs() < 1e-6);
    }

    #[test]
    fn test_adaptive_simpson() {
        // Integral of 1/(1+x^2) from 0 to 1 = pi/4
        let result = adaptive_simpson(&|x| 1.0 / (1.0 + x * x), 0.0, 1.0, 1e-10, 20);
        assert!((result - PI / 4.0).abs() < 1e-8);
    }
}
