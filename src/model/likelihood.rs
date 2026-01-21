//! Likelihood computation
//!
//! Port of the main likelihood function from Unit2.pas

use super::distributions::{surv, survp};
use super::events::prob_event;
use super::link::link;
use super::types::{
    EventType, FunctionDescriptor, Group, LifeHistory, ModelState, SurvivalFunction,
};
use crate::math::constants::MINUS;

/// Compute the log-likelihood of the model
///
/// This is the main objective function for optimization.
///
/// # Arguments
///
/// * `fd` - Function descriptor with current parameter values
/// * `groups` - Vector of groups with individuals
/// * `state` - Model state with configuration
///
/// # Returns
///
/// The negative log-likelihood (to be minimized)
pub fn compute_likelihood(
    fd: &mut FunctionDescriptor,
    groups: &mut [Group],
    state: &ModelState,
) -> f64 {
    // First, update all group parameters from the variable values
    update_group_parameters(fd, groups);

    let mut total_ll = 0.0;

    // Loop over all groups
    for group in groups.iter() {
        if !group.use_flag || group.individuals.is_empty() {
            continue;
        }

        // Loop over individuals in the group
        for ind in &group.individuals {
            // Sum probabilities across life histories, then take log
            // This matches Pascal's approach: ans2 := ans2 + ans3 then ln(ans2)
            // Life histories represent mutually exclusive alternatives, so we sum P(h_k)
            let mut prob_sum = 0.0;

            for hv in &ind.life_histories {
                let log_prob = compute_life_history_likelihood(hv, group, state);
                // Convert log-probability to probability
                // Use exp with protection against underflow
                if log_prob > -700.0 {
                    // exp(-700) ≈ 0, avoid underflow
                    prob_sum += log_prob.exp();
                }
            }

            // Take log of sum (this is the correct likelihood contribution)
            if prob_sum > MINUS {
                total_ll += prob_sum.ln();
            } else {
                // Match Pascal's penalty for impossible events: 10 * ln(minus)
                total_ll += 10.0 * MINUS.ln();
            }
        }
    }

    // Store result
    fd.curr_result = -total_ll;

    // Update best if improved
    if fd.curr_result < fd.best_result || fd.best_result == f64::NEG_INFINITY {
        fd.best_result = fd.curr_result;
        for (_i, vi) in fd.var_info.iter_mut().enumerate() {
            vi.best_value = vi.value;
        }
    }

    // Return negative log-likelihood (for minimization)
    -total_ll
}

/// Update group parameters from variable values
fn update_group_parameters(fd: &FunctionDescriptor, groups: &mut [Group]) {
    for group in groups.iter_mut() {
        if !group.use_flag {
            continue;
        }

        // Update mortality parameters
        // Parameters 0-4: expt_death, survival_param2, ratio_expt_death, prob_death, sex_ratio
        update_survival_params(
            &mut group.mort,
            &group.param,
            fd,
            0, // Start index for mortality params
        );

        // Update maturity parameters
        // Parameters 5-7: expt_maturity, maturity_param2, ratio_expt_maturity
        update_survival_params(
            &mut group.mat,
            &group.param,
            fd,
            5, // Start index for maturity params
        );

        // Update reproduction parameters
        // Parameters 8-19: expt_reproduction, reproduction_param2, n_offspring, etc.
        update_survival_params(
            &mut group.ponte,
            &group.param,
            fd,
            8, // Start index for reproduction params
        );
    }
}

/// Update survival function parameters from variable values
///
/// Uses the parameterization: param = link(sum of unbounded effects * indicators)
/// This means effects are added in unbounded space BEFORE the link transformation,
/// which is how dummy coding works for categorical effects.
fn update_survival_params(
    sf: &mut SurvivalFunction,
    param: &[super::types::ModelParamInst],
    fd: &FunctionDescriptor,
    start_idx: usize,
) {
    // Each survival function has multiple parameters
    // Map from parameter indices to variable indices
    for (i, vp) in sf.vp.iter_mut().enumerate() {
        let param_idx = start_idx + i;
        if param_idx >= param.len() {
            break;
        }

        let p = &param[param_idx];
        if p.nb_terms == 0 {
            continue;
        }

        // Sum contributions in UNBOUNDED space first
        // (intercept + effects where indicator=1)
        let mut unbounded_sum = 0.0;
        let mut min_bound = 0.0;
        let mut max_bound = 1.0;

        for j in 0..p.nb_terms {
            if j < p.po.len() && p.po[j] < fd.var_info.len() {
                let vi = &fd.var_info[p.po[j]];
                let indicator = p.valpo.get(j).unwrap_or(&1.0);

                // Add unbounded value weighted by indicator
                unbounded_sum += vi.value * indicator;

                // Use bounds from first term (intercept)
                if j == 0 {
                    min_bound = vi.min_bound;
                    max_bound = vi.max_bound;
                }
            }
        }

        // Transform the total from unbounded to bounded space
        *vp = link(unbounded_sum, min_bound, max_bound);
    }
}

/// Compute log-likelihood for a single life history
fn compute_life_history_likelihood(hv: &LifeHistory, group: &Group, state: &ModelState) -> f64 {
    let mut ll = 0.0;

    // Get sex from first event (usually sex event)
    let sex = get_sex(hv);

    // Process each event
    for (i, event) in hv.events.iter().enumerate() {
        let event_ll = prob_event(
            event,
            i,
            &group.mort,
            &group.mat,
            &group.ponte,
            sex,
            hv,
            state.tc,
            state.tinf,
            state.ratiomax,
        );

        if event_ll.is_finite() {
            ll += event_ll;
        } else {
            // Return very negative value for impossible events
            return -1e10;
        }
    }

    ll
}

/// Get sex indicator from life history
fn get_sex(hv: &LifeHistory) -> i32 {
    for event in &hv.events {
        if event.event_type == EventType::Sex {
            return event.tp;
        }
    }
    0 // Default to female
}

/// Compute fitness (expected reproductive output)
///
/// Uses numerical integration over the life span weighted by
/// survival and reproduction rates.
pub fn compute_fitness(group: &Group, state: &ModelState, r: f64) -> f64 {
    let tinf = state.tinf;
    let tc = state.tc;

    // Integrate: exp(-r*t) * S_mort(t) * S_mat(t) * lambda_pon(t) * E[clutch] dt
    // from 0 to tinf

    let n_points = 100;
    let dt = tinf / (n_points as f64);

    let mut fitness = 0.0;

    for i in 0..n_points {
        let t = (i as f64 + 0.5) * dt;

        // Survival to time t (female)
        let s_mort = survp(t, &group.mort, 0, tc, tinf);
        if s_mort < MINUS {
            continue;
        }

        // Probability of having matured by time t
        let s_mat = 1.0 - surv(t, &group.mat, 0, tinf);

        // Reproduction rate (hazard)
        let lambda = if group.ponte.vp[0] > MINUS {
            1.0 / group.ponte.vp[0]
        } else {
            0.0
        };

        // Expected clutch size
        let mean_clutch = if group.ponte.vp.len() > 2 && group.ponte.vp[2] > MINUS {
            group.ponte.vp[2]
        } else {
            1.0
        };

        // Discount factor
        let discount = (-r * t).exp();

        fitness += discount * s_mort * s_mat * lambda * mean_clutch * dt;
    }

    fitness
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::types::*;

    fn make_simple_group() -> Group {
        let mut group = Group::new(
            DistributionType::Exp,
            DistributionType::Exp,
            DistributionType::Exp,
        );
        group.use_flag = true;
        group.mort.vp[0] = 100.0; // Mean survival time
        group.mat.vp[0] = 20.0; // Mean maturity time
        group.ponte.vp[0] = 30.0; // Mean reproduction interval
        group.ponte.vp[2] = 3.0; // Mean clutch size
        group
    }

    fn make_simple_state() -> ModelState {
        let mut state = ModelState::new();
        state.tc = 0.0;
        state.tinf = 1000.0;
        state.ratiomax = 2.0;
        state.matclutch = false;
        state
    }

    #[test]
    fn test_compute_fitness() {
        let group = make_simple_group();
        let state = make_simple_state();

        let fitness = compute_fitness(&group, &state, 0.0);

        // Fitness should be positive
        assert!(fitness > 0.0);
    }
}
