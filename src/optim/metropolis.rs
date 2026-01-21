//! Simulated annealing optimization
//!
//! Port of Metropolise and automatic_met from Unit2.pas

use crate::model::likelihood::compute_likelihood;
use crate::model::types::{FunctionDescriptor, Group, MetropolisParams, ModelState};
use crate::rng::Marsaglia;

/// Run simulated annealing optimization
///
/// # Arguments
///
/// * `fd` - Function descriptor with variables to optimize
/// * `params` - Metropolis algorithm parameters
/// * `groups` - Groups with data
/// * `state` - Model state
/// * `rng` - Random number generator
///
/// # Returns
///
/// The best (minimum) objective function value found
pub fn optimize(
    fd: &mut FunctionDescriptor,
    params: &MetropolisParams,
    groups: &mut [Group],
    state: &ModelState,
    rng: &mut Marsaglia,
) -> f64 {
    // Initialize
    let nv = fd.number_of_variables;
    if nv == 0 {
        return 0.0;
    }

    // Initialize steps
    // Port of Pascal Unit2.pas:1546 - step := 1 in unbounded space
    // The optimization happens in unbounded parameter space where typical values
    // range from about -20 to +20. A step of 1 provides good exploration.
    for vi in fd.var_info.iter_mut() {
        vi.step = 1.0;
    }

    // Initial evaluation
    compute_likelihood(fd, groups, state);

    // Store initial values as best
    for vi in fd.var_info.iter_mut() {
        vi.best_value = vi.value;
    }

    // Temperature schedule
    let temp0 = params.temp0;
    let tempf = params.tempf;
    let ntr = params.ntr;
    let nst = params.nst;

    // Calculate cooling rate
    let cooling_rate = if ntr > 1 && temp0 > tempf {
        (tempf / temp0).powf(1.0 / (ntr as f64 - 1.0))
    } else {
        1.0
    };

    let mut temp = temp0;
    let mut n_accepted = 0;
    let mut n_total = 0;

    // Main annealing loop
    for _tr in 0..ntr {
        for _st in 0..nst {
            // Choose a random variable to modify
            let vi_idx = (rng.next() * nv as f64).floor() as usize % nv;

            // Save current value
            let old_value = fd.var_info[vi_idx].value;
            let old_ll = fd.curr_result;

            // Propose new value
            let delta = (rng.next() - 0.5) * 2.0 * fd.var_info[vi_idx].step;
            fd.var_info[vi_idx].value = old_value + delta;

            // Evaluate new likelihood
            compute_likelihood(fd, groups, state);
            let new_ll = fd.curr_result;

            // Metropolis acceptance criterion
            let delta_ll = new_ll - old_ll;
            let accept = if delta_ll < 0.0 {
                // Better solution - always accept
                true
            } else {
                // Worse solution - accept with probability exp(-delta/T)
                let p_accept = (-delta_ll / temp).exp();
                rng.next() < p_accept
            };

            if accept {
                n_accepted += 1;
                // Update best if improved
                if fd.curr_result < fd.best_result {
                    fd.best_result = fd.curr_result;
                    for vi in fd.var_info.iter_mut() {
                        vi.best_value = vi.value;
                    }
                }
            } else {
                // Reject - restore old value
                fd.var_info[vi_idx].value = old_value;
                fd.curr_result = old_ll;
            }

            n_total += 1;
        }

        // Adapt step sizes based on acceptance rate
        let accept_rate = n_accepted as f64 / n_total as f64;
        adapt_steps(fd, accept_rate, params);

        // Cool down
        temp *= cooling_rate;

        // Reset counters for next temperature
        n_accepted = 0;
        n_total = 0;
    }

    // Final local search refinement
    local_search(fd, groups, state, params);

    // Restore best values
    for vi in fd.var_info.iter_mut() {
        vi.value = vi.best_value;
    }
    fd.curr_result = fd.best_result;

    fd.best_result
}

/// Adapt step sizes based on acceptance rate
fn adapt_steps(fd: &mut FunctionDescriptor, accept_rate: f64, params: &MetropolisParams) {
    // Target acceptance rate around 0.3-0.5
    // Port of Pascal Unit2.pas:1161-1180
    for vi in fd.var_info.iter_mut() {
        if accept_rate > 0.5 {
            // Too many acceptances - increase step size
            vi.step *= params.bgeup;
        } else if accept_rate < 0.2 {
            // Too few acceptances - decrease step size
            vi.step *= params.bgedown;
        }

        // Clamp step size in unbounded space
        // Pascal uses minbound=-20, maxbound=20, so range is 40
        // But we allow larger steps up to 20 and minimum of 1e-6
        if vi.step > 20.0 {
            vi.step = 20.0;
        }
        if vi.step < 1e-6 {
            vi.step = 1e-6;
        }
    }
}

/// Local search refinement
fn local_search(
    fd: &mut FunctionDescriptor,
    groups: &mut [Group],
    state: &ModelState,
    params: &MetropolisParams,
) {
    let nv = fd.number_of_variables;
    let max_iter = params.maxrep * nv as i32;

    // Restore best values
    for vi in fd.var_info.iter_mut() {
        vi.value = vi.best_value;
    }

    // Reduce step sizes for local search
    for vi in fd.var_info.iter_mut() {
        vi.step *= 0.1;
    }

    let mut improved = true;
    let mut iter = 0;

    while improved && iter < max_iter {
        improved = false;
        iter += 1;

        for vi_idx in 0..nv {
            // Try moving in positive direction
            let old_value = fd.var_info[vi_idx].value;
            fd.var_info[vi_idx].value = old_value + fd.var_info[vi_idx].step;

            compute_likelihood(fd, groups, state);

            if fd.curr_result < fd.best_result {
                fd.best_result = fd.curr_result;
                for vi in fd.var_info.iter_mut() {
                    vi.best_value = vi.value;
                }
                improved = true;
            } else {
                // Try negative direction
                fd.var_info[vi_idx].value = old_value - fd.var_info[vi_idx].step;
                compute_likelihood(fd, groups, state);

                if fd.curr_result < fd.best_result {
                    fd.best_result = fd.curr_result;
                    for vi in fd.var_info.iter_mut() {
                        vi.best_value = vi.value;
                    }
                    improved = true;
                } else {
                    // No improvement - restore
                    fd.var_info[vi_idx].value = old_value;
                }
            }
        }

        // Further reduce step sizes
        for vi in fd.var_info.iter_mut() {
            vi.step *= 0.9;
        }
    }
}

/// Automatic optimization with multiple restarts
pub fn automatic_met(
    fd: &mut FunctionDescriptor,
    params: &MetropolisParams,
    groups: &mut [Group],
    state: &ModelState,
    rng: &mut Marsaglia,
) -> f64 {
    // Run optimization
    optimize(fd, params, groups, state, rng);

    fd.best_result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::types::VarInfo;

    #[test]
    fn test_adapt_steps() {
        let mut fd = FunctionDescriptor::new();
        fd.var_info
            .push(VarInfo::new("test".to_string(), 0.0, 10.0));
        fd.var_info[0].step = 1.0;

        let params = MetropolisParams::new();

        // High acceptance rate should increase step
        let old_step = fd.var_info[0].step;
        adapt_steps(&mut fd, 0.6, &params);
        assert!(fd.var_info[0].step > old_step);

        // Low acceptance rate should decrease step
        fd.var_info[0].step = 1.0;
        adapt_steps(&mut fd, 0.1, &params);
        assert!(fd.var_info[0].step < 1.0);
    }
}
