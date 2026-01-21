# Investigation Report: Pascal-to-Rust Migration Discrepancies

This document identifies the root causes of discrepancies between the Pascal backend and its Rust rewrite for the lifelihood package.

## Summary

The investigation revealed **multiple critical issues** that explain why estimations (especially variance estimations) are "off" and why NAs appear in the R output. These issues fall into three categories:

1. **Likelihood computation differences** - fundamentally different handling of multiple life histories
2. **Standard error computation differences** - sign conventions and sentinel values
3. **Hessian computation differences** - formula and step size differences

---

## Critical Issue #1: Multiple Life Histories Handling (SEVERITY: HIGH)

### Pascal Behavior (source-pascal/Unit2.pas:1019-1036)

```pascal
ans2 := 0;
for m := 0 to group[j].gi[I].nb_hv - 1 do  { loop over life histories }
begin
  ans3 := 1;
  for n := 0 to ... do                     { loop over events }
    ans3 := ans3 * probevent(...);         { MULTIPLY probabilities }
  ans2 := ans2 + ans3;                     { SUM life history probabilities }
end;
if ans2 > 0 then ans1 := ans1 + ln(ans2)   { LOG of SUM }
```

**Pascal computes**: `ln(p_hv1 + p_hv2 + ...)` for each individual

### Rust Behavior (src/model/likelihood.rs:43-50)

```rust
for hv in &ind.life_histories {
    let ll_hv = compute_life_history_likelihood(hv, group, state);
    total_ll += ll_hv;  // SUM of log-probabilities
}
```

Where `compute_life_history_likelihood` returns log probabilities (sum of log event probabilities).

**Rust computes**: `ln(p_hv1) + ln(p_hv2) + ... = ln(p_hv1 * p_hv2 * ...)` for each individual

### Impact

These are **mathematically different**:
- Pascal: `ln(Σ p_i)` (log of sum of probabilities)
- Rust: `Σ ln(p_i)` (sum of log probabilities = log of product)

For single life history per individual: equivalent.
For multiple life histories: **completely different results**.

The likelihood difference explains the massive discrepancy (~19x) observed between Pascal and Rust outputs.

---

## Critical Issue #2: SE Sentinel Value (SEVERITY: HIGH)

### Pascal Behavior (source-pascal/Unit2.pas:1504-1518)

```pascal
if flag = 1 then
begin
  for i := 1 to fd.number_of_variables do
    if H[i - 1, i - 1] < 0 then
      fd.var_info[i].SE := Sqrt(-H[i - 1, i - 1])  // Note: NEGATIVE diagonal!
    else
      fd.var_info[i].SE := -1;                     // Sentinel: -1
end;
if flag = 0 then
begin
  for i := 1 to fd.number_of_variables do fd.var_info[i].SE := -1;  // All -1 on failure
end;
```

Pascal uses **-1.00000000** as a sentinel to indicate:
- Parameters that weren't fitted
- SE computation failed
- Hessian matrix was singular

### Rust Behavior (src/optim/hessian.rs:150-166)

```rust
let var = hessian[i][i];
if var > 0.0 {
    fd.var_info[i].se = var.sqrt();
} else {
    fd.var_info[i].se = 0.0;  // Uses 0.0 instead of -1
}
// On Hessian inversion failure:
for vi in fd.var_info.iter_mut() {
    vi.se = 0.0;  // Uses 0.0 instead of -1
}
```

Rust uses **0.00000000** as the default, which:
- Is indistinguishable from "computed SE that happens to be zero"
- Causes R's parser to treat it as a valid SE value
- Leads to incorrect variance estimates and NAs in downstream analyses

### Evidence from Output Files

**Pascal** (lifelihood_10_20_30_40/):
```
int_expt_death -0.14732804 -1.00000000
eff_expt_death_type1 -2.80262976 -1.00000000
```
All unfitted/failed parameters show `-1.00000000`.

**Rust** (lifelihood_1_2_3_4/):
```
int_expt_death -1.09861229 0.06246378
eff_expt_death_par1 0.00000000 0.00000000
eff_expt_death_par2 -0.88374412 0.00000000
```
Mix of computed values and `0.00000000` - impossible to distinguish.

---

## Critical Issue #3: Hessian Diagonal Sign Convention (SEVERITY: HIGH)

### Pascal Behavior

Pascal takes the square root of the **NEGATIVE** diagonal:
```pascal
if H[i - 1, i - 1] < 0 then
  fd.var_info[i].SE := Sqrt(-H[i - 1, i - 1])
```

This is correct because when **maximizing** the log-likelihood, the Hessian at the maximum should be **negative definite**. The Fisher Information matrix (used for SE) is the negative of this Hessian.

### Rust Behavior

Rust takes the square root of the **POSITIVE** diagonal:
```rust
if var > 0.0 {
    fd.var_info[i].se = var.sqrt();
}
```

This is mathematically incorrect for a maximization problem. If the optimization returns a Hessian with negative diagonal elements (as expected), Rust would set SE = 0.0, losing the valid standard error.

---

## Critical Issue #4: Hessian Mixed Partial Derivative Formula (SEVERITY: MEDIUM)

### Pascal Formula (source-pascal/Unit2.pas:1336-1341)

```pascal
H[i, j] := (auxD2[i, 1] + auxD2[i, 2] + auxD2[j, 1] + auxD2[j, 2] - 2 * max
           - auxH1[i, j] - auxH2[i, j])
           / (2 * fd.var_info[i + 1].step * fd.var_info[j + 1].step);
```

Uses a 6-point stencil involving:
- auxD2[i,1] = f(x_i + h_i)
- auxD2[i,2] = f(x_i - h_i)
- auxD2[j,1] = f(x_j + h_j)
- auxD2[j,2] = f(x_j - h_j)
- auxH1[i,j] = f(x_i + h_i, x_j - h_j)
- auxH2[i,j] = f(x_i - h_i, x_j + h_j)

### Rust Formula (src/optim/hessian.rs:107)

```rust
let d2 = (f_pp - f_pm - f_mp + f_mm) / (4.0 * hi * hj);
```

Uses a standard 4-point stencil:
- f_pp = f(x_i + h_i, x_j + h_j)
- f_pm = f(x_i + h_i, x_j - h_j)
- f_mp = f(x_i - h_i, x_j + h_j)
- f_mm = f(x_i - h_i, x_j - h_j)

These formulas are **different** and may produce different results, especially near non-smooth regions of the likelihood surface.

---

## Critical Issue #5: Hessian Step Size (SEVERITY: LOW)

### Pascal (source-pascal/Unit2.pas:1291-1292)

```pascal
fd.var_info[i].step := (fd.var_info[i].maxBound - fd.var_info[i].minBound) / 1000;
```

Step size = `range / 1000 = range * 0.001`

### Rust (src/optim/hessian.rs:48-51)

```rust
let h: Vec<f64> = fd.var_info.iter().map(|vi| {
    let range = vi.max_bound - vi.min_bound;
    (range * 1e-4).max(1e-8)
}).collect();
```

Step size = `range * 0.0001` (with minimum of 1e-8)

The Rust step is **10x smaller** than Pascal, which could affect numerical stability and precision.

---

## Other Observations

### Parameter Count Discrepancy

The Pascal output shows 14 parameters while Rust shows 18, even for similar models. This suggests:
- Rust may be creating variables for unfitted parameters (where Pascal skips them entirely)
- Different handling of the model specification during initialization

### VarInfo Default SE

In `src/model/types.rs:383`, SE is initialized to 0.0:
```rust
se: 0.0,
```

This means unfitted parameters start with SE=0.0 and never get updated, making them indistinguishable from failed SE computations.

### Hessian Invalid Flag

Pascal writes `(!INVALID)` when the Hessian is singular:
```pascal
if flag_hessian = 0 then
  writeln(outfile, 'inverse of Hessian Matrix (!INVALID)')
```

Rust only writes the invalid marker when hessian is None (src/io/output.rs:95), but this distinction may not propagate correctly from the computation.

---

## Recommended Fixes (Priority Order)

1. **Fix multiple life histories handling** (Critical)
   - Rust should sum probabilities (not log-probabilities) across life histories before taking the log
   - This requires restructuring `compute_life_history_likelihood` to return raw probabilities

2. **Use -1 sentinel for SE** (Critical)
   - Change default SE initialization to -1.0
   - Change failure case SE to -1.0
   - Update output.rs to handle this sentinel correctly

3. **Fix Hessian diagonal sign convention** (Critical)
   - Check if diagonal is negative and take sqrt of negated value
   - Match Pascal's `Sqrt(-H[i-1, i-1])` pattern

4. **Consider Hessian formula alignment** (Medium)
   - Either align with Pascal's 6-point formula or verify 4-point gives equivalent results

5. **Align step sizes** (Low)
   - Use `range / 1000` instead of `range * 1e-4` for consistency

---

## Files to Investigate Further

- `src/model/likelihood.rs` - Life history likelihood aggregation
- `src/optim/hessian.rs` - Hessian computation and SE extraction
- `src/model/types.rs` - VarInfo default values
- `src/io/output.rs` - Output formatting
- `source-pascal/Unit2.pas` - Reference implementation

---

*Generated: 2026-01-21*
