//! Survival distributions
//!
//! Port of survival functions from Unit2.pas

use super::types::{DistributionType, SurvivalFunction};
use crate::math::constants::MINUS;
use crate::math::special::{erfra, expo, gamma, power};

/// Compute survival probability S(x) for a given distribution
///
/// # Arguments
///
/// * `x` - Time point
/// * `su` - Survival function specification
/// * `sex` - Sex indicator (0 = female, 1 = male)
/// * `tinf` - Maximum censoring time
///
/// # Returns
///
/// Survival probability at time x
pub fn surv(x: f64, su: &SurvivalFunction, sex: i32, tinf: f64) -> f64 {
    if x >= tinf {
        return 0.0;
    }

    let eps = 1e-23;
    let vp = &su.vp;

    // Get parameter values, ensuring they're positive
    let vp0 = if vp[0] <= 0.0 { eps } else { vp[0] };
    let vp1 = if vp.len() > 1 && vp[1] <= 0.0 {
        eps
    } else if vp.len() > 1 {
        vp[1]
    } else {
        1.0
    };
    let vp2 = if vp.len() > 2 { vp[2] } else { 0.0 };

    if x <= 0.0 {
        return 1.0;
    }

    // Apply male ratio if sex == 1 and vp2 > 0
    let ratio = if sex == 1 && vp2 > 0.0 { vp2 } else { 1.0 };

    match su.name {
        DistributionType::Exp => {
            // Exponential: hazard = 1/vp0, mean survival time = vp0
            // S(x) = exp(-x / (vp0 * ratio))
            expo(-x / (vp0 * ratio))
        }
        DistributionType::Wei => {
            // Weibull: mean = vp0, shape = vp1
            // paramaux = vp0 / Gamma(1 + 1/vp1) to match expected mean
            let paramaux = vp0 / gamma(1.0 + 1.0 / vp1);
            // S(x) = exp(-(x / (paramaux * ratio))^vp1)
            expo(-power(x / (paramaux * ratio), vp1))
        }
        DistributionType::Lgn => {
            // Log-normal: mean = vp0, sigma = vp1
            // mu = ln(mean * ratio) - 0.5 * sigma^2
            let paramaux = (vp0 * ratio).ln() - 0.5 * vp1 * vp1;
            // S(x) = 0.5 * (1 + erf((mu - ln(x)) / (sigma * sqrt(2))))
            let racine2 = std::f64::consts::SQRT_2;
            0.5 * (1.0 + erfra((paramaux - x.ln()) / (vp1 * racine2)))
        }
        DistributionType::Gam => {
            // Gamma distribution
            // Using the regularized incomplete gamma function
            let shape = vp1;
            let scale = vp0 / (shape * ratio);
            let z = x / scale;
            // S(x) = 1 - P(shape, z) where P is regularized lower incomplete gamma
            1.0 - crate::math::special::igamma(shape, z)
        }
    }
}

/// Compute survival with juvenile mortality increase
///
/// Applies a multiplicative factor to hazard for ages < tc
pub fn survp(x: f64, su: &SurvivalFunction, sex: i32, tc: f64, tinf: f64) -> f64 {
    let vp = &su.vp;

    // vp[3] is the hazard ratio for juvenile period
    // If vp[3] <= 0 or x >= tc, use normal survival
    if vp.len() < 4 || vp[3] <= 0.0 || x >= tc || x <= 0.0 {
        return surv(x, su, sex, tinf);
    }

    // For ages < tc, the hazard is multiplied by vp[3]
    // This is implemented by computing survival in two phases
    let hazard_ratio = vp[3];

    // Survival up to tc with increased hazard, then from tc to x with normal hazard
    if x <= tc {
        // All time spent in juvenile period with increased hazard
        // S_p(x) = S(x)^hazard_ratio for exponential
        // For other distributions, we need to compute the cumulative hazard
        surv(x * hazard_ratio, su, sex, tinf)
    } else {
        // Time split between juvenile and adult periods
        let s_tc = surv(tc * hazard_ratio, su, sex, tinf);
        let s_x = surv(x, su, sex, tinf);
        let s_tc_normal = surv(tc, su, sex, tinf);

        if s_tc_normal.abs() < MINUS {
            0.0
        } else {
            s_tc * s_x / s_tc_normal
        }
    }
}

/// Compute survival with senescence effects on reproduction
///
/// # Arguments
///
/// * `x` - Time point
/// * `su` - Survival function for reproduction
/// * `t` - Time since maturity
/// * `ratiomax` - Maximum ratio for clutch size reduction
/// * `tinf` - Maximum censoring time
pub fn surv_with_senescence(
    x: f64,
    su: &SurvivalFunction,
    t: f64,
    ratiomax: f64,
    tinf: f64,
) -> f64 {
    let vp = &su.vp;

    // Check if senescence parameters are present
    if vp.len() < 8 {
        return surv(x, su, 0, tinf);
    }

    // vp[6] = linear senescence effect on reproduction rate
    // vp[7] = quadratic senescence effect on reproduction rate
    let sen_t = vp[6];
    let sen_t2 = vp[7];

    if sen_t.abs() < MINUS && sen_t2.abs() < MINUS {
        return surv(x, su, 0, tinf);
    }

    // Compute senescence factor
    // Factor increases hazard, so survival decreases
    let mut factor = 1.0 + sen_t * t + sen_t2 * t * t;

    // Clamp factor to prevent extreme values
    if factor < 1.0 / ratiomax {
        factor = 1.0 / ratiomax;
    }
    if factor > ratiomax {
        factor = ratiomax;
    }

    // Apply factor to expected time (inversely affects hazard)
    let mut su_mod = su.clone();
    su_mod.vp[0] = su.vp[0] / factor;

    surv(x, &su_mod, 0, tinf)
}

/// Compute probability density function (hazard * survival)
pub fn pdf(x: f64, su: &SurvivalFunction, sex: i32, tinf: f64) -> f64 {
    hazard(x, su, sex, tinf) * surv(x, su, sex, tinf)
}

/// Compute hazard function h(x)
pub fn hazard(x: f64, su: &SurvivalFunction, sex: i32, tinf: f64) -> f64 {
    if x >= tinf || x <= 0.0 {
        return 0.0;
    }

    let eps = 1e-23;
    let vp = &su.vp;
    let vp0 = if vp[0] <= 0.0 { eps } else { vp[0] };
    let vp1 = if vp.len() > 1 && vp[1] <= 0.0 {
        eps
    } else if vp.len() > 1 {
        vp[1]
    } else {
        1.0
    };
    let vp2 = if vp.len() > 2 { vp[2] } else { 0.0 };

    let ratio = if sex == 1 && vp2 > 0.0 { vp2 } else { 1.0 };

    match su.name {
        DistributionType::Exp => {
            // h(x) = 1 / (vp0 * ratio)
            1.0 / (vp0 * ratio)
        }
        DistributionType::Wei => {
            // h(x) = (vp1 / paramaux) * (x / paramaux)^(vp1 - 1)
            let paramaux = vp0 / gamma(1.0 + 1.0 / vp1);
            let scaled = x / (paramaux * ratio);
            (vp1 / (paramaux * ratio)) * power(scaled, vp1 - 1.0)
        }
        DistributionType::Lgn => {
            // h(x) = f(x) / S(x) where f is density and S is survival
            let racine2 = std::f64::consts::SQRT_2;
            let paramaux = (vp0 * ratio).ln() - 0.5 * vp1 * vp1;

            // Density of log-normal
            let z = (x.ln() - paramaux) / vp1;
            let f = expo(-0.5 * z * z) / (x * vp1 * (2.0 * std::f64::consts::PI).sqrt());

            // Survival
            let s = 0.5 * (1.0 + erfra((paramaux - x.ln()) / (vp1 * racine2)));

            if s > MINUS {
                f / s
            } else {
                0.0
            }
        }
        DistributionType::Gam => {
            // Gamma hazard
            let shape = vp1;
            let scale = vp0 / (shape * ratio);

            // f(x) = x^(shape-1) * exp(-x/scale) / (scale^shape * Gamma(shape))
            let ln_f = (shape - 1.0) * x.ln()
                - x / scale
                - shape * scale.ln()
                - crate::math::special::ln_gamma(shape);
            let f = expo(ln_f);

            let s = 1.0 - crate::math::special::igamma(shape, x / scale);

            if s > MINUS {
                f / s
            } else {
                0.0
            }
        }
    }
}

/// Compute cumulative hazard function H(x) = -ln(S(x))
pub fn cumulative_hazard(x: f64, su: &SurvivalFunction, sex: i32, tinf: f64) -> f64 {
    let s = surv(x, su, sex, tinf);
    if s > MINUS {
        -s.ln()
    } else {
        -MINUS.ln() // Large positive value
    }
}

/// Compute quantile function (inverse survival)
pub fn quantile(p: f64, su: &SurvivalFunction, sex: i32, tinf: f64) -> f64 {
    if p <= 0.0 {
        return 0.0;
    }
    if p >= 1.0 {
        return tinf;
    }

    // Use bisection to find x such that S(x) = 1 - p
    let target = 1.0 - p;
    let mut low = 0.0;
    let mut high = tinf;

    for _ in 0..100 {
        let mid = (low + high) / 2.0;
        let s_mid = surv(mid, su, sex, tinf);

        if (s_mid - target).abs() < 1e-10 {
            return mid;
        }

        if s_mid > target {
            low = mid;
        } else {
            high = mid;
        }
    }

    (low + high) / 2.0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_exp_surv(mean: f64) -> SurvivalFunction {
        let mut sf = SurvivalFunction::new(DistributionType::Exp, 5);
        sf.vp[0] = mean;
        sf
    }

    fn make_weibull_surv(mean: f64, shape: f64) -> SurvivalFunction {
        let mut sf = SurvivalFunction::new(DistributionType::Wei, 5);
        sf.vp[0] = mean;
        sf.vp[1] = shape;
        sf
    }

    #[test]
    fn test_exponential_survival() {
        let sf = make_exp_surv(10.0);
        let tinf = 1000.0;

        // S(0) = 1
        assert!((surv(0.0, &sf, 0, tinf) - 1.0).abs() < 1e-10);

        // S(mean) = exp(-1) ≈ 0.368
        let s_mean = surv(10.0, &sf, 0, tinf);
        assert!((s_mean - (-1.0_f64).exp()).abs() < 1e-10);

        // Survival is monotonically decreasing
        assert!(surv(5.0, &sf, 0, tinf) > surv(10.0, &sf, 0, tinf));
        assert!(surv(10.0, &sf, 0, tinf) > surv(20.0, &sf, 0, tinf));
    }

    #[test]
    fn test_weibull_survival() {
        let sf = make_weibull_surv(10.0, 2.0);
        let tinf = 1000.0;

        // S(0) = 1
        assert!((surv(0.0, &sf, 0, tinf) - 1.0).abs() < 1e-10);

        // Survival is monotonically decreasing
        assert!(surv(5.0, &sf, 0, tinf) > surv(10.0, &sf, 0, tinf));

        // With shape=1, Weibull = Exponential
        let sf_exp = make_weibull_surv(10.0, 1.0);
        let sf_exp_direct = make_exp_surv(10.0);
        assert!((surv(5.0, &sf_exp, 0, tinf) - surv(5.0, &sf_exp_direct, 0, tinf)).abs() < 0.1);
    }

    #[test]
    fn test_hazard_exponential() {
        let sf = make_exp_surv(10.0);
        let tinf = 1000.0;

        // Exponential hazard is constant = 1/mean
        let h1 = hazard(1.0, &sf, 0, tinf);
        let h5 = hazard(5.0, &sf, 0, tinf);
        let h10 = hazard(10.0, &sf, 0, tinf);

        assert!((h1 - 0.1).abs() < 1e-10);
        assert!((h5 - 0.1).abs() < 1e-10);
        assert!((h10 - 0.1).abs() < 1e-10);
    }

    #[test]
    fn test_survival_tinf() {
        let sf = make_exp_surv(10.0);
        let tinf = 100.0;

        // S(tinf) = 0
        assert!((surv(tinf, &sf, 0, tinf) - 0.0).abs() < 1e-10);
        assert!((surv(tinf + 1.0, &sf, 0, tinf) - 0.0).abs() < 1e-10);
    }
}
