# Implementation Plan: Fixing Rust Backend

This document provides step-by-step instructions for fixing the identified issues in the Rust implementation.

## Overview

The fixes are organized by priority. Each section includes:
- What to change
- Why (mathematical justification)
- Exact file locations and code changes
- Testing approach

---

## Fix #1: Multiple Life Histories Aggregation (CRITICAL)

### Problem
Rust sums log-probabilities across life histories, but the correct approach is to sum probabilities then take the log.

### Mathematical Background
When an individual has multiple possible life histories (e.g., due to censored/uncertain events), these represent **mutually exclusive alternatives**. The likelihood contribution for individual $i$ with life histories $h_1, h_2, ...$ is:

$$L_i = P(h_1) + P(h_2) + ... = \sum_k P(h_k)$$

The log-likelihood is:
$$\ell_i = \ln\left(\sum_k P(h_k)\right)$$

**NOT** $\sum_k \ln(P(h_k))$ which equals $\ln\left(\prod_k P(h_k)\right)$.

### File: `src/model/likelihood.rs`

#### Change 1: Modify `compute_likelihood` function (lines 36-50)

**Current code:**
```rust
// Loop over individuals in the group
for ind in &group.individuals {
    // Loop over life histories
    for hv in &ind.life_histories {
        let ll_hv = compute_life_history_likelihood(hv, group, state);
        total_ll += ll_hv;
    }
}
```

**New code:**
```rust
// Loop over individuals in the group
for ind in &group.individuals {
    // Sum probabilities across life histories, then take log
    let mut prob_sum = 0.0;

    for hv in &ind.life_histories {
        let log_prob = compute_life_history_likelihood(hv, group, state);
        // Convert log-probability to probability
        // Use exp with protection against underflow
        if log_prob > -700.0 {  // exp(-700) ≈ 0, avoid underflow
            prob_sum += log_prob.exp();
        }
    }

    // Take log of sum (this is the correct likelihood contribution)
    if prob_sum > MINUS {
        total_ll += prob_sum.ln();
    } else {
        total_ll += 10.0 * MINUS.ln();  // Match Pascal's penalty for impossible events
    }
}
```

#### Change 2: Add MINUS constant import if not present

At the top of the file, ensure:
```rust
use crate::math::constants::MINUS;
```

### Testing
- Single life history: Results should be identical (log of one probability = log of that probability)
- Multiple life histories: Results will differ - verify against Pascal output

---

## Fix #2: SE Sentinel Value (CRITICAL)

### Problem
Rust uses 0.0 for unfitted/failed SEs. R code expects -1.0 as sentinel.

### File: `src/model/types.rs`

#### Change 1: Modify `VarInfo::new` (around line 376-389)

**Current code:**
```rust
impl VarInfo {
    pub fn new(name: String, min_bound: f64, max_bound: f64) -> Self {
        VarInfo {
            min_bound,
            max_bound,
            value: 0.0,
            best_value: 0.0,
            step: 1.0,
            se: 0.0,  // <-- Change this
            sample: Vec::new(),
            name,
            typ: 0,
            loc: 0,
        }
    }
}
```

**New code:**
```rust
impl VarInfo {
    pub fn new(name: String, min_bound: f64, max_bound: f64) -> Self {
        VarInfo {
            min_bound,
            max_bound,
            value: 0.0,
            best_value: 0.0,
            step: 1.0,
            se: -1.0,  // Sentinel value: -1 means "not computed"
            sample: Vec::new(),
            name,
            typ: 0,
            loc: 0,
        }
    }
}
```

### File: `src/optim/hessian.rs`

#### Change 2: Modify `calc_se` function (lines 148-168)

**Current code:**
```rust
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
```

**New code:**
```rust
match gauss_jordan_invert(&mut hessian) {
    Ok(_det) => {
        // Extract standard errors from diagonal
        // Note: For maximization, Hessian is negative definite at optimum
        // The inverse Hessian diagonal should be negative
        // SE = sqrt(-diag(H^{-1}))
        for i in 0..nv {
            let var = hessian[i][i];
            if var < 0.0 {
                // Negative diagonal is expected (maximizing likelihood)
                fd.var_info[i].se = (-var).sqrt();
            } else if var > 0.0 {
                // Positive diagonal - unusual but handle it
                // This might happen if we're minimizing negative log-likelihood
                fd.var_info[i].se = var.sqrt();
            } else {
                fd.var_info[i].se = -1.0; // Sentinel: variance is zero or undefined
            }
        }
        true
    }
    Err(_) => {
        // Hessian is singular - all SEs are undefined
        for vi in fd.var_info.iter_mut() {
            vi.se = -1.0;  // Sentinel value
        }
        false
    }
}
```

### File: `src/io/output.rs`

#### Change 3: Update output handling (optional, for clarity)

The output format should remain the same (writing `se` value directly), but add a comment noting that -1.0 is a sentinel:

```rust
// Parameter estimates with standard errors
// SE = -1.00000000 indicates SE was not computed (unfitted param or Hessian failure)
for vi in &fd.var_info {
    writeln!(writer, "{} {:.8} {:.8}", vi.name, vi.best_value, vi.se)?;
}
```

---

## Fix #3: Hessian Sign Convention (CRITICAL)

### Problem
The Hessian computation and SE extraction need to handle the sign correctly.

### Mathematical Background
When **maximizing** log-likelihood $\ell(\theta)$:
- At the maximum, the Hessian $H = \frac{\partial^2 \ell}{\partial \theta^2}$ is **negative semi-definite**
- Fisher Information: $I = -H$ (positive semi-definite)
- Variance-covariance matrix: $\text{Var}(\hat\theta) \approx I^{-1} = (-H)^{-1}$
- Standard error: $\text{SE}(\hat\theta_i) = \sqrt{[(-H)^{-1}]_{ii}}$

When we invert $H$ directly (calling it $H^{-1}$):
- If $H$ is negative definite, $H^{-1}$ is also negative definite
- So $\text{diag}(H^{-1}) < 0$
- Therefore: $\text{SE} = \sqrt{-\text{diag}(H^{-1})}$

This is exactly what Pascal does: `sqrt(-H[i-1, i-1])` where `H` is already the inverted Hessian.

### Important Consideration

The Rust code returns **negative log-likelihood** for minimization (`-total_ll`). This affects the sign of the Hessian:
- If we compute Hessian of $-\ell(\theta)$, it equals $-H$
- At the minimum, $-H$ is positive semi-definite
- So `hessian[i][i] > 0` in Rust's case

**This means the current Rust code might be partially correct** if it's computing the Hessian of the negative log-likelihood consistently. However, we need to verify this.

### File: `src/optim/hessian.rs`

#### Change: Review and potentially modify `compute_hessian` and `calc_se`

First, verify what `compute_likelihood` returns at line 38:
```rust
compute_likelihood(fd, groups, state);
let f0 = fd.curr_result;  // This is -log_likelihood (for minimization)
```

If `curr_result` stores `-log_likelihood`, then the Hessian being computed is for `-ℓ(θ)`, which should be positive semi-definite at minimum. In this case:
- `hessian[i][i] > 0` is expected
- `SE = sqrt(hessian[i][i])` is correct

**Verify by adding debug output or checking:**
```rust
// After computing hessian, before inversion:
// Expected: diagonal elements should be positive if computing Hessian of -log_lik
// Add this temporarily for debugging:
#[cfg(debug_assertions)]
for i in 0..nv {
    eprintln!("Hessian diagonal before inversion [{}]: {}", i, hessian[i][i]);
}
```

**The fix in Change 2 above handles both cases** by checking the sign of the diagonal after inversion.

---

## Fix #4: Hessian Step Size Alignment (LOW PRIORITY)

### Problem
Pascal uses `range/1000`, Rust uses `range*1e-4` (10x smaller).

### File: `src/optim/hessian.rs`

#### Change: Modify step size calculation (lines 45-52)

**Current code:**
```rust
let h: Vec<f64> = fd
    .var_info
    .iter()
    .map(|vi| {
        let range = vi.max_bound - vi.min_bound;
        (range * 1e-4).max(1e-8)
    })
    .collect();
```

**New code (matching Pascal):**
```rust
let h: Vec<f64> = fd
    .var_info
    .iter()
    .map(|vi| {
        let range = vi.max_bound - vi.min_bound;
        (range / 1000.0).max(1e-8)  // Match Pascal: range/1000
    })
    .collect();
```

---

## Fix #5: Hessian Mixed Partial Formula (LOW PRIORITY)

### Problem
Pascal uses a different formula for off-diagonal elements.

### Analysis
The standard 4-point formula Rust uses:
$$\frac{\partial^2 f}{\partial x_i \partial x_j} \approx \frac{f(x_i+h, x_j+h) - f(x_i+h, x_j-h) - f(x_i-h, x_j+h) + f(x_i-h, x_j-h)}{4h_i h_j}$$

This is mathematically correct and commonly used. Pascal's 6-point formula may provide slightly different numerical behavior but shouldn't fundamentally change results.

### Recommendation
Keep Rust's formula unless testing shows significant discrepancies. The 4-point formula is standard and well-understood.

---

## Fix #6: Hessian Invalid Flag in Output (MEDIUM PRIORITY)

### Problem
Pascal marks invalid Hessian with `(!INVALID)`, need to ensure Rust does the same.

### File: `src/optim/hessian.rs`

#### Change: Return validity flag from `calc_se`

The function already returns `bool` indicating success. Ensure this propagates to output.

### File: `src/main.rs` or wherever output is generated

Ensure the Hessian validity is tracked:
```rust
let hessian_valid = if compute_se {
    calc_se(&mut fd, &mut groups, &state)
} else {
    false
};

// When writing output:
let config = OutputConfig {
    // ...
    hessian: if hessian_valid {
        get_inverse_hessian(&mut fd, &mut groups, &state)
    } else {
        None  // This will trigger (!INVALID) in output
    },
    // ...
};
```

---

## Fix #7: Inverse Hessian Output Sign Convention (CRITICAL)

### Problem
R code computes variance from the inverse Hessian matrix: `x %*% var_parameter %*% t(x)`. This produces negative values, causing `sqrt()` to return NaN.

### Root Cause
- Pascal maximizes log-likelihood → Hessian is negative definite → inverse Hessian is negative definite
- Rust minimizes -log-likelihood → Hessian is positive definite → inverse Hessian is positive definite
- The R code was written for Pascal's convention and expects the inverse Hessian to be negative definite
- When R reads Rust's positive definite inverse Hessian, matrix operations produce negative variances

### File: `src/optim/hessian.rs`

#### Change: Negate inverse Hessian in `get_inverse_hessian`

```rust
pub fn get_inverse_hessian(
    fd: &mut FunctionDescriptor,
    groups: &mut [Group],
    state: &ModelState,
) -> Option<Vec<Vec<f64>>> {
    let mut hessian = compute_hessian(fd, groups, state);

    match gauss_jordan_invert(&mut hessian) {
        Ok(_) => {
            // Negate the inverse Hessian to match Pascal's convention
            // Pascal outputs inverse of Hessian of log_lik (negative definite)
            // Rust computes inverse of Hessian of -log_lik (positive definite)
            // Negating converts from Rust's convention to Pascal's
            for row in hessian.iter_mut() {
                for val in row.iter_mut() {
                    *val = -*val;
                }
            }
            Some(hessian)
        }
        Err(_) => None,
    }
}
```

### Mathematical Justification
- Variance-covariance matrix: Var(θ̂) = I(θ)^(-1) where I = -H_log_lik
- Pascal outputs: H_log_lik^(-1) (negative definite), R then uses -H_log_lik^(-1) = Var
- Rust computes: H_{-log_lik}^(-1) = (-H_log_lik)^(-1) (positive definite)
- Negating Rust's output: -(-H_log_lik)^(-1) = H_log_lik^(-1) → matches Pascal

---

## Implementation Order

1. **Fix #2 (SE Sentinel)** - Quick win, easy to implement and test
2. **Fix #1 (Life Histories)** - Most critical for correctness
3. **Fix #3 (Hessian Sign)** - Already partially addressed in Fix #2
4. **Fix #4 (Step Size)** - Simple change
5. **Fix #6 (Invalid Flag)** - Ensure output consistency
6. **Fix #7 (Inverse Hessian Sign)** - Critical for R variance computation
7. **Fix #5 (Formula)** - Only if testing shows issues

---

## Testing Strategy

### Unit Tests

Add tests in each modified file:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiple_life_histories() {
        // Create individual with 2 life histories
        // Verify: ln(exp(ll1) + exp(ll2)) != ll1 + ll2
    }

    #[test]
    fn test_se_sentinel() {
        let vi = VarInfo::new("test".to_string(), 0.0, 10.0);
        assert_eq!(vi.se, -1.0);
    }
}
```

### Integration Tests

1. Run `cargo test --all-features`
2. Run `devtools::test()` in R
3. Compare output files from Rust vs Pascal for same input:
   - Use seeds `10, 20, 30, 40` to match existing Pascal output
   - Compare likelihood values (should match within tolerance)
   - Compare SE values (should be -1.0 for unfitted, positive for fitted)

### Manual Testing

Run `dev/demo.R` and verify:
- No NAs in predictions
- SE values make sense (no zeros for fitted parameters)
- Likelihood values are reasonable

---

## Verification Checklist

After implementation, verify:

- [ ] `cargo build --release` succeeds
- [ ] `cargo test --all-features` passes
- [ ] `devtools::test()` passes (94 tests)
- [ ] `dev/demo.R` runs without NAs
- [ ] Output file format matches Pascal (esp. SE = -1.0 for unfitted)
- [ ] Likelihood values are comparable to Pascal for same input
- [ ] Hessian matrix has `(!INVALID)` when singular

---

## Notes on Pascal Behavior

Some Pascal behaviors may not be optimal:

1. **Step size `range/1000`** - This is relatively large; smaller steps often give better numerical derivatives. However, consistency with Pascal is more important for migration.

2. **6-point formula for mixed partials** - More complex but not necessarily better than the standard 4-point formula.

3. **Handling of impossible events** - Pascal uses `10 * ln(minus)` as penalty. This is arbitrary but works. Rust should match this for consistency.

The goal is first to match Pascal behavior, then optionally improve numerical methods in future versions.

---

*Implementation Plan Created: 2026-01-21*
