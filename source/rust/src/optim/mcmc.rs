//! MCMC sampling
//!
//! Port of promenade function from Unit2.pas

use crate::model::likelihood::compute_likelihood;
use crate::model::types::{FunctionDescriptor, Group, ModelState};
use crate::rng::Marsaglia;

/// Run MCMC sampling (random walk Metropolis)
///
/// # Arguments
///
/// * `fd` - Function descriptor with variables and samples storage
/// * `params` - Metropolis parameters (for step sizes)
/// * `groups` - Groups with data
/// * `state` - Model state
/// * `rng` - Random number generator
/// * `n_samples` - Number of samples to collect
/// * `interval` - Interval between samples (thinning)
pub fn mcmc_sample(
    fd: &mut FunctionDescriptor,
    groups: &mut [Group],
    state: &ModelState,
    rng: &mut Marsaglia,
    n_samples: i32,
    interval: i32,
) {
    let nv = fd.number_of_variables;
    if nv == 0 || n_samples <= 0 {
        return;
    }

    // Initialize sample storage
    fd.sample.clear();
    fd.sample.reserve(n_samples as usize);
    for vi in fd.var_info.iter_mut() {
        vi.sample.clear();
        vi.sample.reserve(n_samples as usize);
    }

    // Start from best values
    for vi in fd.var_info.iter_mut() {
        vi.value = vi.best_value;
    }

    // Initial evaluation
    compute_likelihood(fd, groups, state);
    let mut current_ll = fd.curr_result;

    // Set step sizes based on optimization results
    for vi in fd.var_info.iter_mut() {
        vi.step = (vi.max_bound - vi.min_bound) / 20.0;
    }

    let total_iterations = n_samples * interval;
    let mut sample_idx = 0;

    for iter in 0..total_iterations {
        // Choose random variable
        let vi_idx = (rng.next() * nv as f64).floor() as usize % nv;

        // Save current state
        let old_value = fd.var_info[vi_idx].value;

        // Propose new value (symmetric random walk)
        let delta = (rng.next() - 0.5) * 2.0 * fd.var_info[vi_idx].step;
        let new_value = old_value + delta;

        // Check bounds
        if new_value < fd.var_info[vi_idx].min_bound || new_value > fd.var_info[vi_idx].max_bound {
            // Out of bounds - reject
            continue;
        }

        fd.var_info[vi_idx].value = new_value;

        // Evaluate
        compute_likelihood(fd, groups, state);
        let new_ll = fd.curr_result;

        // Metropolis-Hastings acceptance
        let log_alpha = -(new_ll - current_ll); // Negative because we're minimizing
        let accept = if log_alpha >= 0.0 {
            true
        } else {
            rng.next() < log_alpha.exp()
        };

        if accept {
            current_ll = new_ll;
        } else {
            // Reject - restore
            fd.var_info[vi_idx].value = old_value;
            fd.curr_result = current_ll;
        }

        // Store sample at intervals
        if (iter + 1) % interval == 0 && sample_idx < n_samples as usize {
            fd.sample.push(current_ll);
            for vi in fd.var_info.iter_mut() {
                vi.sample.push(vi.value);
            }
            sample_idx += 1;
        }
    }

    // Compute mean and variance of likelihood
    if !fd.sample.is_empty() {
        let n = fd.sample.len() as f64;
        let mean: f64 = fd.sample.iter().sum::<f64>() / n;
        let var: f64 = fd.sample.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n;
        fd.mean_result = mean;
        fd.var_result = var;
    }
}

/// Compute MCMC standard errors using batch means
pub fn compute_mcmc_se(samples: &[f64], n_batches: usize) -> f64 {
    if samples.is_empty() || n_batches == 0 {
        return 0.0;
    }

    let n = samples.len();
    let batch_size = n / n_batches;
    if batch_size == 0 {
        return 0.0;
    }

    // Compute batch means
    let mut batch_means = Vec::with_capacity(n_batches);
    for i in 0..n_batches {
        let start = i * batch_size;
        let end = if i == n_batches - 1 {
            n
        } else {
            start + batch_size
        };
        let batch: &[f64] = &samples[start..end];
        let mean: f64 = batch.iter().sum::<f64>() / batch.len() as f64;
        batch_means.push(mean);
    }

    // Compute variance of batch means
    let overall_mean: f64 = batch_means.iter().sum::<f64>() / n_batches as f64;
    let var: f64 = batch_means
        .iter()
        .map(|x| (x - overall_mean).powi(2))
        .sum::<f64>()
        / (n_batches as f64 - 1.0);

    // SE = sqrt(batch_size * var / n)
    (batch_size as f64 * var / n as f64).sqrt()
}

/// Compute effective sample size
pub fn effective_sample_size(samples: &[f64]) -> f64 {
    let n = samples.len();
    if n < 2 {
        return n as f64;
    }

    // Compute mean
    let mean: f64 = samples.iter().sum::<f64>() / n as f64;

    // Compute variance
    let var: f64 = samples.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / (n as f64 - 1.0);
    if var < 1e-10 {
        return n as f64;
    }

    // Compute autocorrelation at lag 1
    let mut cov = 0.0;
    for i in 0..(n - 1) {
        cov += (samples[i] - mean) * (samples[i + 1] - mean);
    }
    cov /= (n - 1) as f64;
    let rho = cov / var;

    // ESS approximation
    let ess = n as f64 * (1.0 - rho) / (1.0 + rho);
    ess.max(1.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcmc_se() {
        // Test with constant samples - SE should be 0
        let samples = vec![1.0; 100];
        let se = compute_mcmc_se(&samples, 10);
        assert!(se.abs() < 1e-10);
    }

    #[test]
    fn test_effective_sample_size() {
        // Independent samples should have ESS ≈ n
        let samples: Vec<f64> = (0..100).map(|i| i as f64).collect();
        let ess = effective_sample_size(&samples);
        // ESS should be reasonable (not extremely small)
        assert!(ess > 10.0);
    }
}
