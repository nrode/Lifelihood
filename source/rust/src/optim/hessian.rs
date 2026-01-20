//! Hessian computation and standard errors
//!
//! Port of Hessian and calcSE functions from Unit2.pas

use crate::math::matrix::gauss_jordan_invert;
use crate::model::likelihood::compute_likelihood;
use crate::model::types::{FunctionDescriptor, Group, ModelState};

/// Compute the Hessian matrix using numerical differentiation
///
/// Uses central differences for second derivatives.
///
/// # Arguments
///
/// * `fd` - Function descriptor with current parameter values
/// * `groups` - Groups with data
/// * `state` - Model state
///
/// # Returns
///
/// The Hessian matrix (n x n where n = number of variables)
pub fn compute_hessian(
    fd: &mut FunctionDescriptor,
    groups: &mut [Group],
    state: &ModelState,
) -> Vec<Vec<f64>> {
    let nv = fd.number_of_variables;
    if nv == 0 {
        return vec![];
    }

    // Use best values
    for vi in fd.var_info.iter_mut() {
        vi.value = vi.best_value;
    }

    // Compute base likelihood
    compute_likelihood(fd, groups, state);
    let f0 = fd.curr_result;

    // Initialize Hessian matrix
    let mut hessian = vec![vec![0.0; nv]; nv];

    // Step sizes for numerical differentiation
    let h: Vec<f64> = fd
        .var_info
        .iter()
        .map(|vi| {
            let range = vi.max_bound - vi.min_bound;
            (range * 1e-4).max(1e-8)
        })
        .collect();

    // Compute diagonal elements (second derivatives)
    for i in 0..nv {
        let old_val = fd.var_info[i].value;
        let hi = h[i];

        // f(x + h)
        fd.var_info[i].value = old_val + hi;
        compute_likelihood(fd, groups, state);
        let f_plus = fd.curr_result;

        // f(x - h)
        fd.var_info[i].value = old_val - hi;
        compute_likelihood(fd, groups, state);
        let f_minus = fd.curr_result;

        // Second derivative: (f(x+h) - 2*f(x) + f(x-h)) / h^2
        hessian[i][i] = (f_plus - 2.0 * f0 + f_minus) / (hi * hi);

        // Restore
        fd.var_info[i].value = old_val;
    }

    // Compute off-diagonal elements (mixed partial derivatives)
    for i in 0..nv {
        for j in (i + 1)..nv {
            let old_i = fd.var_info[i].value;
            let old_j = fd.var_info[j].value;
            let hi = h[i];
            let hj = h[j];

            // f(x + hi, y + hj)
            fd.var_info[i].value = old_i + hi;
            fd.var_info[j].value = old_j + hj;
            compute_likelihood(fd, groups, state);
            let f_pp = fd.curr_result;

            // f(x + hi, y - hj)
            fd.var_info[j].value = old_j - hj;
            compute_likelihood(fd, groups, state);
            let f_pm = fd.curr_result;

            // f(x - hi, y + hj)
            fd.var_info[i].value = old_i - hi;
            fd.var_info[j].value = old_j + hj;
            compute_likelihood(fd, groups, state);
            let f_mp = fd.curr_result;

            // f(x - hi, y - hj)
            fd.var_info[j].value = old_j - hj;
            compute_likelihood(fd, groups, state);
            let f_mm = fd.curr_result;

            // Mixed derivative: (f_pp - f_pm - f_mp + f_mm) / (4 * hi * hj)
            let d2 = (f_pp - f_pm - f_mp + f_mm) / (4.0 * hi * hj);
            hessian[i][j] = d2;
            hessian[j][i] = d2; // Symmetric

            // Restore
            fd.var_info[i].value = old_i;
            fd.var_info[j].value = old_j;
        }
    }

    // Restore original values
    for vi in fd.var_info.iter_mut() {
        vi.value = vi.best_value;
    }

    hessian
}

/// Compute standard errors from the Hessian matrix
///
/// SE = sqrt(diag(H^(-1))) where H is the Hessian
///
/// # Arguments
///
/// * `fd` - Function descriptor to store SEs
/// * `groups` - Groups with data
/// * `state` - Model state
///
/// # Returns
///
/// True if successful, false if Hessian is singular
pub fn calc_se(fd: &mut FunctionDescriptor, groups: &mut [Group], state: &ModelState) -> bool {
    let nv = fd.number_of_variables;
    if nv == 0 {
        return true;
    }

    // Compute Hessian
    let mut hessian = compute_hessian(fd, groups, state);

    // Invert Hessian to get variance-covariance matrix
    match gauss_jordan_invert(&mut hessian) {
        Ok(_det) => {
            // Extract standard errors from diagonal
            for i in 0..nv {
                let var = hessian[i][i];
                if var > 0.0 {
                    fd.var_info[i].se = var.sqrt();
                } else {
                    fd.var_info[i].se = 0.0; // Invalid variance
                }
            }
            true
        }
        Err(_) => {
            // Hessian is singular
            for vi in fd.var_info.iter_mut() {
                vi.se = 0.0;
            }
            false
        }
    }
}

/// Get the inverse Hessian (variance-covariance matrix)
pub fn get_inverse_hessian(
    fd: &mut FunctionDescriptor,
    groups: &mut [Group],
    state: &ModelState,
) -> Option<Vec<Vec<f64>>> {
    let mut hessian = compute_hessian(fd, groups, state);

    match gauss_jordan_invert(&mut hessian) {
        Ok(_) => Some(hessian),
        Err(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: Full tests require setting up complete model state and groups
    // which is complex. Here we just test basic functionality.

    #[test]
    fn test_empty_hessian() {
        let fd = FunctionDescriptor::new();
        let mut groups = vec![];
        let state = ModelState::new();

        let hessian = compute_hessian(&mut fd.clone(), &mut groups, &state);
        assert!(hessian.is_empty());
    }
}
