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
        EventType::Nop => prob_nop(event, event_idx, pon, hv, tinf, ratiomax),
    }
}

/// Probability of sex determination event
///
/// Port of Pascal probevent for 'sex' from Unit2.pas:839-845
/// If mo.vp[4] (sex ratio) is 0, returns 1.0 (log = 0)
/// Otherwise returns: sex * probmale + (1 - sex) * (1 - probmale)
fn prob_sex(_event: &Event, mort: &SurvivalFunction, sex: i32, _tc: f64, _tinf: f64) -> f64 {
    // Check if sex ratio parameter is used (mo.vp[4])
    if mort.vp.len() <= 4 || mort.vp[4] == 0.0 {
        // No sex ratio effect, probability = 1, log = 0
        0.0
    } else {
        let prob_male = mort.vp[4];
        let prob = sex as f64 * prob_male + (1.0 - sex as f64) * (1.0 - prob_male) + MINUS;
        prob.ln()
    }
}

/// Probability of maturity event
///
/// Port of Pascal censored_mat from Unit2.pas:534-537
/// P = surv(t1, mat, sex) - surv(t2, mat, sex)
/// This is the probability of maturing in interval [t1, t2]
/// Note: mortality is handled separately in the 'mor' event
fn prob_mat(
    event: &Event,
    _mort: &SurvivalFunction,
    mat: &SurvivalFunction,
    _sex: i32,
    _tc: f64,
    tinf: f64,
) -> f64 {
    // Pascal: censored_mat := surv(t1, su, sex) - surv(t2, su, sex)
    // Note: Pascal uses sex=0 (female) for maturity calculations
    let s_mat1 = surv(event.t1, mat, 0, tinf);
    let s_mat2 = surv(event.t2, mat, 0, tinf);

    let diff = s_mat1 - s_mat2;
    if diff > MINUS {
        (diff + MINUS).ln()
    } else {
        // For censored events (t2 >= tinf), s_mat2 = 0, so diff = s_mat1
        // This should still work, but add protection
        if s_mat1 > MINUS {
            (s_mat1 + MINUS).ln()
        } else {
            f64::NEG_INFINITY
        }
    }
}

/// Probability of reproduction (ponte) event
///
/// Port of Pascal probevent for 'pon' from Unit2.pas:870-882
/// P = (survtotpon(t1-debut, ...) - survtotpon(t2-debut, ...)) * ppoissontrunc(expected_clutch, clutch_size)
///
/// In the simple case (no senescence), survtotpon just returns surv(tfromlastpon, pon, sex)
/// So: P = (surv(t1-debut, pon) - surv(t2-debut, pon)) * ppoissontrunc(expected_clutch, clutch_size)
fn prob_pon(
    event: &Event,
    event_idx: usize,
    _mort: &SurvivalFunction,
    pon: &SurvivalFunction,
    hv: &LifeHistory,
    _sex: i32,
    _tc: f64,
    tinf: f64,
    ratiomax: f64,
) -> f64 {
    // Time since last ponte (or maturity)
    // t1 - debut is the waiting time from the previous event to t1
    let t_from_last_1 = event.t1 - event.debut;
    let t_from_last_2 = event.t2 - event.debut;

    // Get time since maturity for senescence calculations
    let t_since_mat = get_time_since_maturity(event_idx, hv);

    // Survival in the reproduction waiting time process
    // Use survtotpon logic - for now, simplified without senescence
    let s1 = surv_with_senescence(t_from_last_1, pon, t_since_mat, ratiomax, tinf);
    let s2 = surv_with_senescence(t_from_last_2, pon, t_since_mat, ratiomax, tinf);

    let diff = s1 - s2;

    // Expected clutch size with senescence effects
    let expected_clutch = compute_expected_clutch_size(pon, t_since_mat, ratiomax);

    // Clutch size probability (truncated Poisson)
    let p_clutch = p_poisson_trunc(expected_clutch, event.tp);

    let prob = diff * p_clutch;
    if prob > MINUS {
        (prob + MINUS).ln()
    } else {
        f64::NEG_INFINITY
    }
}

/// Compute survival with senescence for reproduction
/// Port of survtotpon from Unit2.pas:560-586
fn surv_with_senescence(
    t: f64,
    pon: &SurvivalFunction,
    t_since_mat: f64,
    ratiomax: f64,
    tinf: f64,
) -> f64 {
    if t < 0.0 {
        return 1.0;
    }

    let vp = &pon.vp;

    // Check if senescence parameters are present and non-zero
    let has_senescence = vp.len() > 7 && (vp[6] != 0.0 || vp[7] != 0.0);

    if has_senescence {
        // Apply senescence to the mean reproduction time
        // vp[6] = linear senescence, vp[7] = quadratic senescence
        let base_mean = vp[0];

        // Compute modified mean based on senescence
        // The Pascal code modifies vp[0] in unbounded space then links back
        // For simplicity, we apply the senescence factor directly
        let sen_factor = 1.0 + vp[6] * t_since_mat + vp[7] * t_since_mat * t_since_mat;
        let clamped_factor = sen_factor.max(1.0 / ratiomax).min(ratiomax);

        // Create modified survival function
        let mut modified_pon = pon.clone();
        modified_pon.vp[0] = base_mean / clamped_factor;

        surv(t, &modified_pon, 0, tinf)
    } else {
        surv(t, pon, 0, tinf)
    }
}

/// Compute expected clutch size with senescence
fn compute_expected_clutch_size(pon: &SurvivalFunction, t_since_mat: f64, ratiomax: f64) -> f64 {
    let vp = &pon.vp;

    // Base expected clutch size = vp[2]
    let mut expected = if vp.len() > 2 && vp[2] > 0.0 {
        vp[2]
    } else {
        1.0
    };

    // Apply clutch size senescence if parameters present (vp[8], vp[9])
    if vp.len() > 9 && (vp[8] != 0.0 || vp[9] != 0.0) {
        let sen_factor = 1.0 - vp[8] * t_since_mat - vp[9] * t_since_mat * t_since_mat;
        let clamped = sen_factor.max(1.0 / ratiomax).min(1.0);
        expected *= clamped;
    }

    // Ensure expected is at least 1 (for truncated Poisson)
    expected.max(1.0 + MINUS)
}

/// Truncated Poisson probability P(X = k | X > 0)
/// Port of ppoissontrunc from Unit2.pas:653-685
fn p_poisson_trunc(expected: f64, k: i32) -> f64 {
    if k <= 0 || expected < MINUS {
        return MINUS;
    }

    // Convert expected (truncated Poisson mean) to regular Poisson mean (mu)
    // expected = mu / (1 - exp(-mu))
    // Approximation: mu ≈ expected - exp(-(expected - 1))
    let mu = expected - expo(-(expected - 1.0));

    if mu <= 0.0 {
        return MINUS;
    }

    // P(X = k) for Poisson
    let p_k = p_poisson(mu, k);

    // P(X = 0) for Poisson
    let p_0 = expo(-mu);

    // P(X = k | X > 0) = P(X = k) / (1 - P(X = 0))
    let p_positive = 1.0 - p_0;
    if p_positive > MINUS {
        p_k / p_positive
    } else {
        MINUS
    }
}

/// Probability of mortality event
///
/// Port of Pascal censored_mort from Unit2.pas:625-650
/// Uses t1, t2 (not debut/fin) for the survival calculations.
/// Note: This is a simplified version that doesn't include the full
/// survival-reproduction tradeoff (Sda function). The Sda terms are
/// omitted for now, assuming d, da, dn parameters are not fitted.
fn prob_mor(event: &Event, mort: &SurvivalFunction, sex: i32, tc: f64, tinf: f64) -> f64 {
    // Check if juvenile mortality parameter is used (vp[3] > 0)
    let use_juvenile = mort.vp.len() > 3 && mort.vp[3] > 0.0;

    if use_juvenile {
        // With juvenile mortality period (tc)
        if event.t2 < tc {
            // Death before critical time
            let s_tc = surv(tc, mort, sex, tinf);
            let prob = 1.0 - mort.vp[3] * s_tc;
            if prob > MINUS {
                (prob + MINUS).ln()
            } else {
                f64::NEG_INFINITY
            }
        } else if event.t2 < tinf {
            // Observed death after tc
            if event.t1 < tc {
                let s2 = survp(event.t2, mort, sex, tc, tinf);
                let prob = 1.0 - s2;
                if prob > MINUS {
                    (prob + MINUS).ln()
                } else {
                    f64::NEG_INFINITY
                }
            } else {
                let s1 = survp(event.t1, mort, sex, tc, tinf);
                let s2 = survp(event.t2, mort, sex, tc, tinf);
                let diff = s1 - s2;
                if diff > MINUS {
                    (diff + MINUS).ln()
                } else {
                    f64::NEG_INFINITY
                }
            }
        } else {
            // Censored mortality (t2 >= tinf)
            if event.t1 < tc {
                // Survived past tc, censored
                0.0 // log(1) = 0
            } else {
                let s1 = survp(event.t1, mort, sex, tc, tinf);
                if s1 > MINUS {
                    (s1 + MINUS).ln()
                } else {
                    f64::NEG_INFINITY
                }
            }
        }
    } else {
        // Without juvenile mortality - use simple surv function
        if event.t2 >= tinf {
            // Right-censored mortality: survived to t1
            let s = surv(event.t1, mort, sex, tinf);
            if s > MINUS {
                (s + MINUS).ln()
            } else {
                f64::NEG_INFINITY
            }
        } else {
            // Observed death in interval [t1, t2]
            let s1 = surv(event.t1, mort, sex, tinf);
            let s2 = surv(event.t2, mort, sex, tinf);

            let diff = s1 - s2;
            if diff > MINUS {
                (diff + MINUS).ln()
            } else {
                f64::NEG_INFINITY
            }
        }
    }
}

/// Probability of non-reproduction event (time without reproducing)
///
/// Port of Pascal probevent for 'nop' from Unit2.pas:867-869
/// P = survtotpon(t1 - debut, t1 - mat.fin, pon, hv, sex)
///
/// This is the probability of NOT reproducing in the time from the last event
/// (debut) to the mortality time (t1).
fn prob_nop(
    event: &Event,
    event_idx: usize,
    pon: &SurvivalFunction,
    hv: &LifeHistory,
    tinf: f64,
    ratiomax: f64,
) -> f64 {
    // Time since last ponte (or maturity) to t1
    let t_from_last = event.t1 - event.debut;

    if t_from_last < 0.0 {
        // No time elapsed or negative (edge case), probability = 1, log = 0
        return 0.0;
    }

    // Get time since maturity for senescence calculations
    let t_since_mat = get_time_since_maturity(event_idx, hv);

    // Use survival with senescence, evaluated at the relative time
    // This is the probability of NOT reproducing during the waiting time
    let s = surv_with_senescence(t_from_last, pon, t_since_mat, ratiomax, tinf);

    if s > MINUS {
        (s + MINUS).ln()
    } else {
        f64::NEG_INFINITY
    }
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
    fn test_p_poisson_trunc() {
        // Test truncated Poisson probability
        // For expected clutch size of 3, P(k=3|k>0) should be reasonable
        let p = p_poisson_trunc(3.0, 3);
        assert!(p > 0.0);
        assert!(p < 1.0);

        // Sum of probabilities should be close to 1
        let mut sum = 0.0;
        for k in 1..20 {
            sum += p_poisson_trunc(3.0, k);
        }
        assert!((sum - 1.0).abs() < 0.01);
    }
}
