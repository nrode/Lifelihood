# Plan: fix the `optional-dist` issues

This plan covers issues 1 to 7 of `issue.md` (Part 1, the issues introduced by the current changes). Part 2 (issues 8 and 9) is out of scope, except that fixing issue 1 also requires the engine change described in issue 9.

The steps are ordered so that each one can be tested on its own, with the cheap R-only fixes first and the engine change last.

## Step 1: Fix the `matclutch` guards (issue 2)

**File:** `R/lifelihoodData.R`, `as_lifelihoodData()`

- In the three guards (the `matclutch_size` NULL check, the `matclutch_size` validation, and the warning), and in the line that resets `matclutch <- FALSE`, replace `"maturity" %in% names(dist)` with `"maturity" %in% required_events`.
- Effect: `matclutch` is kept whenever maturity data is used (maturity *or* reproduction fitted). It is only reset, with a warning, when the fit uses neither.
- Update the warning so it explains why the argument is ignored. For example: "`matclutch` and `matclutch_size` are ignored because neither maturity nor reproduction is fitted."

**Tests** (`tests/testthat/test-lifelihoodData.R`):
- `dist = data.frame(mortality = "wei", reproduction = "wei")` with `matclutch = TRUE` and a valid `matclutch_size` keeps `matclutch = TRUE` and raises no warning.
- The same call with `matclutch_size = NULL` errors.
- `dist = data.frame(mortality = "wei")` with `matclutch = TRUE` warns and stores `FALSE`.

## Step 2: Fix the error order in `compute_fitted_event_rate()` (issue 5)

**File:** `R/compute_event_rate.R`

- Move the `if (!event %in% names(lifelihoodResults$dist)) stop(...)` block above `require_event_data(...)`.

**Test:** on a mortality-only fit, `compute_fitted_event_rate(res, event = "maturity")` errors with "was not fitted".

## Step 3: Move the omitted-event config rule into `validate_config_input()` and warn (issue 4)

**Files:** `R/config_file.R` and `R/lifelihood.R`

- Add an internal helper, `neutralize_omitted_events(config, dist)`, in `R/config_file.R` next to `validate_param2_for_exponential()`. For each event that is in `c("mortality", "maturity", "reproduction")` but not in `names(dist)`:
  - treat `expt_death` and `survival_param2` as mortality core parameters; `expt_maturity` and `maturity_param2` as maturity core parameters; and `expt_reproduction`, `reproduction_param2`, and `n_offspring` as reproduction core parameters;
  - collect only non-core parameters in that section whose value is not `"not_fitted"`;
  - if any non-core parameters are fitted, raise one warning naming the event and those dropped parameters. For example: "`mortality` is not in `dist`, so these parameters are not fitted: sex_ratio.";
  - set the whole section to `"not_fitted"`.
- Call this helper from `validate_config_input()` when `dist` is not `NULL`, before the `validate_param2_for_exponential()` calls.
- Remove the `omitted_events` loop from `lifelihood()`. It is now covered by the existing `validate_config_input(model_config, temp_lifelihoodData$dist)` call.
- Check that `create_simulation_input()` and any other callers of `validate_config_input(config, dist)` still behave as expected.

**Tests** (config tests):
- A maturity-only `dist` with `mortality$sex_ratio = "geno"` warns, and `sex_ratio` ends up `not_fitted`.
- A maturity-only `dist` with only mortality core parameters configured does not warn about those parameters being dropped.
- A full `dist` raises no warning.

## Step 4: Reject trade-off parameters when mortality is not fitted (issue 3)

**File:** `R/config_file.R` (inside the same `validate_config_input(config, dist)` path as step 3)

- If `"mortality"` is not in `names(dist)` and any of `increase_death_hazard`, `tof_decay` or `increase_death_hazard_n_offspring` is not `"not_fitted"`, stop with: "Trade-off parameters (...) act on the mortality hazard and require `mortality` in `dist`."
- This check must run on the user's config, *before* step 3's helper sets the sections to `not_fitted`. Step 3 only touches omitted sections, and these parameters are in the `reproduction` section, so the order is already safe. Add a comment saying so.

**Test:** a reproduction-only `dist` with `increase_death_hazard = "1"` errors, and with a full `dist` it does not.

## Step 5: Make `simulate_life_history()` work on partial fits (issue 6)

**File:** `R/simulation.R`, `simulate_life_history()`

- Add a simulation-specific helper in `R/simulation.R` to select fitted events; do not reuse `infer_simulation_events()`, whose return value is tailored to goodness-of-fit simulation.
- When `event = "all"`, simulate the fitted mortality and maturity events, and include reproduction only when reproduction, maturity, and mortality are all fitted. If no requested event can be simulated, stop with a clear error.
- When the user explicitly asks for an event that is not in `names(object$lifelihoodData$dist)`, stop with: "The `<event>` event was not fitted in these results."
- Keep the reproduction dependency check: explicitly simulating reproduction requires a fit that includes reproduction, maturity, and mortality. Update its error message to name all three required models.
- Check that `simulate_life_history_tradeoff()` and `simulate_event()` are only reached with fitted events, so that `dist[[ev]]` is never `NULL`.

**Tests:**
- On a mortality-only fit, `simulate_life_history(res)` returns mortality columns only.
- `simulate_life_history(res, event = "maturity")` errors with "not fitted".
- On a reproduction-only fit, `simulate_life_history(res, event = "reproduction")` gives a clear error.
- On a full fit, `simulate_life_history(res)` still returns maturity, reproduction, and mortality outputs.

## Step 6: Deduplicate the omitted-event fallbacks (issue 7)

**Files:** `R/default_boundaries.R`, `R/lifelihood.R`, `R/write_input.R`

- `default_bounds_df()`: replace the three `if ("<event>" %in% names(dist)) ... else "exp"` blocks with one `dist <- complete_dist(dist)` at the top, then use `dist[["mortality"]]` and so on directly.
- `summary.all_models()`: add a small helper, for example `dist_or_na(dist, event)`, and use it for the three `dist_*` columns.
- `format_dataframe_to_txt()`: replace the four `NULL` placeholder blocks with a loop over a named list of `(argument, placeholder column)` pairs.
- This step changes no behavior. Check it with the existing tests.

## Step 7: Remove the log-likelihood padding in the engine (issue 1, via issue 9)

**Files:** `source/Unit2.pas`, and rebuilt binaries in `inst/bin/`

- In the engine, work out which processes are fitted from whether their mean parameter is used:
  - mortality: `expt_death`, index 0;
  - maturity: `expt_maturity`, index 5;
  - reproduction: `expt_reproduction`, index 8.

  A parameter is not used when `param[k].po[0] = -1` for every group, or equivalently when the model description marks it as `not_fitted`. Store this in three global booleans, set once after the model is interpreted (`interpretation()`).
- In `probevent()`:
  - `mor`: if mortality is not fitted, return `1`.
  - `mat`: if maturity is not fitted, replace the `censored_mat(...)` factor with `1`. If `matclutch = 1` and reproduction is fitted, keep the `ppoissontrunc(...)` clutch-size factor, since it belongs to reproduction.
  - `pon` / `nop`: if reproduction is not fitted, return `1`.
- Rebuild the macOS binaries with `just macos` and Linux binaries with `just linux` (Docker). Windows is intentionally skipped.
- Keep the R-side placeholder intervals in `format_dataframe_to_txt()`. The engine still expects `mat`/`mor` records, and they no longer contribute to the likelihood.

**Checks** (rerun the issue 1 reprex):
- A mortality-only fit gives the same logLik with and without the maturity and clutch columns.
- A maturity-only fit gives the same logLik with and without the death columns.
- The logLik of a partial fit is no longer close to a multiple of `ln(1e-35)`.
- A full three-event fit gives exactly the same logLik and estimates as before the change, which shows there is no regression.
- Add a test that compares logLik for the same partial `dist` with and without columns for the omitted events.

## Wrap-up

- Run `devtools::document()` (the roxygen text changes in steps 1 and 5), `air format . --check` and `jarl check .`.
- Run `devtools::test()` only on the affected files: `test-lifelihoodData.R`, the config tests, the simulation tests, and the event-rate tests.
- Update `issue.md` to mark issues 1 to 7 as addressed.
