//! Event probability calculations
//!
//! Port of probevent and related functions from Unit2.pas

use super::distributions::{surv, survp};
use super::types::{Event, EventType, LifeHistory, SurvivalFunction};
use crate::math::constants::MINUS;
use crate::math::special::{expo, p_poisson};

/// Compute probability of an event
///
/// # Arguments
///
/// * `event` - The event to compute probability for
/// * `mort` - Mortality survival function
/// * `mat` - Maturity survival function
/// * `pon` - Reproduction survival function
/// * `integrale` - Integral value for conditioning
/// * `sex` - Sex indicator
/// * `hv` - Life history
/// * `tc` - Critical time for juvenile mortality
/// * `tinf` - Maximum censoring time
/// * `ratiomax` - Maximum ratio for senescence
/// * `matclutch` - Whether maturity and first clutch are combined
///
/// # Returns
///
/// Log probability of the event
pub fn prob_event(
    event: &Event,
    event_idx: usize,
    mort: &SurvivalFunction,
    mat: &SurvivalFunction,
    pon: &SurvivalFunction,
    sex: i32,
    hv: &LifeHistory,
    tc: f64,
    tinf: f64,
    ratiomax: f64,
) -> f64 {
    match event.event_type {
        EventType::Sex => prob_sex(event, mort, sex, tc, tinf),
        EventType::Mat => prob_mat(event, mort, mat, sex, tc, tinf),
        EventType::Pon => prob_pon(event, event_idx, mort, pon, hv, sex, tc, tinf, ratiomax),
        EventType::Mor => prob_mor(event, mort, sex, tc, tinf),
        EventType::Nop => prob_nop(event, event_idx, pon, hv, ratiomax),
    }
}

/// Probability of sex determination event
fn prob_sex(event: &Event, mort: &SurvivalFunction, sex: i32, tc: f64, tinf: f64) -> f64 {
    // Sex is determined: survival from t1 to t2
    let s1 = survp(event.debut, mort, sex, tc, tinf);
    let s2 = survp(event.fin, mort, sex, tc, tinf);

    if s1 < MINUS {
        return f64::NEG_INFINITY;
    }

    // Probability = integral of hazard * survival over [t1, t2]
    // For censored events (t2 = tinf), this simplifies
    if event.t2 >= tinf {
        // Right-censored: just survival to t1
        if s1 > MINUS {
            s1.ln()
        } else {
            f64::NEG_INFINITY
        }
    } else {
        // Observed interval: S(t1) - S(t2)
        let diff = s1 - s2;
        if diff > MINUS {
            diff.ln()
        } else {
            f64::NEG_INFINITY
        }
    }
}

/// Probability of maturity event
fn prob_mat(
    event: &Event,
    mort: &SurvivalFunction,
    mat: &SurvivalFunction,
    sex: i32,
    tc: f64,
    tinf: f64,
) -> f64 {
    if event.t2 >= tinf {
        // Censored maturity: didn't mature before censoring
        // P = integral from 0 to t1 of survival without maturing
        // = S_mort(t1) * S_mat(t1)
        let s_mort = survp(event.t1, mort, sex, tc, tinf);
        let s_mat = surv(event.t1, mat, 0, tinf);

        if s_mort > MINUS && s_mat > MINUS {
            (s_mort * s_mat).ln()
        } else {
            f64::NEG_INFINITY
        }
    } else {
        // Observed maturity in [t1, t2]
        // P = integral over [t1, t2] of f_mat(t) * S_mort(t)
        // ≈ (S_mat(t1) - S_mat(t2)) * S_mort((t1+t2)/2)
        let s_mat1 = surv(event.debut, mat, 0, tinf);
        let s_mat2 = surv(event.fin, mat, 0, tinf);
        let s_mort = survp((event.debut + event.fin) / 2.0, mort, sex, tc, tinf);

        let diff_mat = s_mat1 - s_mat2;
        if diff_mat > MINUS && s_mort > MINUS {
            (diff_mat * s_mort).ln()
        } else {
            f64::NEG_INFINITY
        }
    }
}

/// Probability of reproduction (ponte) event
fn prob_pon(
    event: &Event,
    event_idx: usize,
    mort: &SurvivalFunction,
    pon: &SurvivalFunction,
    hv: &LifeHistory,
    sex: i32,
    tc: f64,
    tinf: f64,
    ratiomax: f64,
) -> f64 {
    // Get time since maturity for senescence calculations
    let t_since_mat = get_time_since_maturity(event_idx, hv);

    // Reproduction rate (hazard of reproduction)
    let lambda = compute_reproduction_rate(pon, t_since_mat, ratiomax);

    // Probability of reproducing in interval [t1, t2] conditional on survival
    let s_mort1 = survp(event.debut, mort, sex, tc, tinf);

    if s_mort1 < MINUS {
        return f64::NEG_INFINITY;
    }

    // For an observed reproduction event:
    // P = lambda * integral over [t1, t2] of exp(-lambda * t) * S_mort(t) dt
    // ≈ lambda * (t2 - t1) * S_mort_avg * exp(-lambda * t_avg)

    let dt = event.fin - event.debut;
    if dt < MINUS {
        return f64::NEG_INFINITY;
    }

    // Log probability
    let log_lambda = if lambda > MINUS {
        lambda.ln()
    } else {
        MINUS.ln()
    };
    let log_s_mort = if s_mort1 > MINUS {
        s_mort1.ln()
    } else {
        f64::NEG_INFINITY
    };

    // Include clutch size probability if applicable
    let log_clutch = clutch_size_prob(event.tp, pon, t_since_mat, ratiomax);

    log_lambda + log_s_mort + log_clutch
}

/// Probability of mortality event
fn prob_mor(event: &Event, mort: &SurvivalFunction, sex: i32, tc: f64, tinf: f64) -> f64 {
    if event.t2 >= tinf {
        // Right-censored mortality: survived to t1
        let s = survp(event.t1, mort, sex, tc, tinf);
        if s > MINUS {
            s.ln()
        } else {
            f64::NEG_INFINITY
        }
    } else {
        // Observed death in interval [t1, t2]
        let s1 = survp(event.debut, mort, sex, tc, tinf);
        let s2 = survp(event.fin, mort, sex, tc, tinf);

        let diff = s1 - s2;
        if diff > MINUS {
            diff.ln()
        } else {
            f64::NEG_INFINITY
        }
    }
}

/// Probability of non-reproduction event (time without reproducing)
fn prob_nop(
    event: &Event,
    event_idx: usize,
    pon: &SurvivalFunction,
    hv: &LifeHistory,
    ratiomax: f64,
) -> f64 {
    let t_since_mat = get_time_since_maturity(event_idx, hv);

    // Reproduction rate
    let lambda = compute_reproduction_rate(pon, t_since_mat, ratiomax);

    // Time without reproducing
    let dt = event.fin - event.debut;
    if dt < 0.0 {
        return 0.0; // No time elapsed, probability = 1, log = 0
    }

    // P(no reproduction in [t1, t2]) = exp(-lambda * dt)
    -lambda * dt
}

/// Get time since maturity for a given event
fn get_time_since_maturity(event_idx: usize, hv: &LifeHistory) -> f64 {
    // Find the maturity event
    for i in 0..event_idx {
        if hv.events[i].event_type == EventType::Mat {
            let mat_time = (hv.events[i].t1 + hv.events[i].t2) / 2.0;
            let event_time = (hv.events[event_idx].t1 + hv.events[event_idx].t2) / 2.0;
            return (event_time - mat_time).max(0.0);
        }
    }
    0.0
}

/// Compute reproduction rate with senescence
fn compute_reproduction_rate(pon: &SurvivalFunction, t_since_mat: f64, ratiomax: f64) -> f64 {
    let vp = &pon.vp;

    // Base rate = 1/vp[0] (mean waiting time)
    let base_rate = if vp[0] > MINUS { 1.0 / vp[0] } else { 0.0 };

    // Apply senescence if parameters present
    if vp.len() >= 8 {
        let sen_t = vp[6];
        let sen_t2 = vp[7];

        let mut factor = 1.0 + sen_t * t_since_mat + sen_t2 * t_since_mat * t_since_mat;

        // Clamp factor
        if factor < 1.0 / ratiomax {
            factor = 1.0 / ratiomax;
        }
        if factor > ratiomax {
            factor = ratiomax;
        }

        base_rate * factor
    } else {
        base_rate
    }
}

/// Compute clutch size probability (truncated Poisson)
fn clutch_size_prob(
    clutch_size: i32,
    pon: &SurvivalFunction,
    t_since_mat: f64,
    ratiomax: f64,
) -> f64 {
    if clutch_size <= 0 {
        return 0.0; // Log(1) = 0
    }

    let vp = &pon.vp;

    // Mean clutch size = vp[2]
    let mut mean_clutch = if vp.len() > 2 && vp[2] > MINUS {
        vp[2]
    } else {
        1.0
    };

    // Apply senescence on clutch size if parameters present
    if vp.len() >= 10 {
        let sen_n_t = vp[8];
        let sen_n_t2 = vp[9];

        let mut factor = 1.0 - sen_n_t * t_since_mat - sen_n_t2 * t_since_mat * t_since_mat;

        // Clamp factor
        if factor < 1.0 / ratiomax {
            factor = 1.0 / ratiomax;
        }
        if factor > 1.0 {
            factor = 1.0;
        }

        mean_clutch *= factor;
    }

    // Truncated Poisson: P(X = k | X > 0)
    // = P(X = k) / (1 - P(X = 0))
    // = P(X = k) / (1 - exp(-lambda))
    if mean_clutch < MINUS {
        return f64::NEG_INFINITY;
    }

    let p_k = p_poisson(mean_clutch, clutch_size);
    let p_0 = expo(-mean_clutch);
    let p_positive = 1.0 - p_0;

    if p_positive > MINUS && p_k > MINUS {
        (p_k / p_positive).ln()
    } else {
        f64::NEG_INFINITY
    }
}

/// Compute probability of censored maturity
pub fn censored_mat(
    t1: f64,
    t2: f64,
    mort: &SurvivalFunction,
    mat: &SurvivalFunction,
    sex: i32,
    tc: f64,
    tinf: f64,
) -> f64 {
    // Probability that individual didn't mature by time t2
    // given they were alive at t1 and survived to t2
    let s_mat = surv(t2, mat, 0, tinf);
    let s_mort1 = survp(t1, mort, sex, tc, tinf);
    let s_mort2 = survp(t2, mort, sex, tc, tinf);

    if s_mort1 < MINUS {
        return 0.0;
    }

    // P(not mature by t2 | alive at t1, survived to t2)
    // = S_mat(t2) * P(survive [t1, t2])
    s_mat * (s_mort2 / s_mort1)
}

/// Compute probability of censored mortality
pub fn censored_mort(t: f64, mort: &SurvivalFunction, sex: i32, tc: f64, tinf: f64) -> f64 {
    // Probability of surviving to time t
    survp(t, mort, sex, tc, tinf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::types::DistributionType;

    fn make_exp_surv(mean: f64) -> SurvivalFunction {
        let mut sf = SurvivalFunction::new(DistributionType::Exp, 5);
        sf.vp[0] = mean;
        sf
    }

    #[test]
    fn test_prob_mor_censored() {
        let mort = make_exp_surv(100.0);
        let event = Event::new(EventType::Mor, 50.0, 1000.0);
        let tinf = 1000.0;

        let log_p = prob_mor(&event, &mort, 0, 0.0, tinf);

        // Should be log(S(50)) for exponential
        let expected = surv(50.0, &mort, 0, tinf).ln();
        assert!((log_p - expected).abs() < 1e-6);
    }

    #[test]
    fn test_clutch_size_prob() {
        let mut pon = SurvivalFunction::new(DistributionType::Exp, 12);
        pon.vp[0] = 10.0; // Mean reproduction interval
        pon.vp[2] = 3.0; // Mean clutch size

        // Probability of clutch size 3 should be reasonable
        let log_p = clutch_size_prob(3, &pon, 0.0, 2.0);
        assert!(log_p.is_finite());
        assert!(log_p < 0.0); // Log probability should be negative
    }
}
