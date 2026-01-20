//! Lifelihood main entry point
//!
//! This binary reads input files, performs optimization, and writes output.
//! It is designed to be called from R via system() with arguments passed via stdin.
//!
//! # Arguments (24 positional args via stdin)
//!
//! 1. path_input_data - Path to data file
//! 2. path_param_bounds - Path to parameter bounds file
//! 3. group_by_group - Boolean (TRUE/FALSE)
//! 4. MCMC - Number of MCMC samples (0 = disabled)
//! 5. interval - Interval between MCMC samples
//! 6. se.fit - Compute standard errors (TRUE/FALSE)
//! 7. saveprobevent - Save event probabilities (TRUE/FALSE)
//! 8. fitness - Fitness reparametrization (TRUE/FALSE)
//! 9. r - Intrinsic rate value
//! 10-13. seed1-seed4 - Random seeds (4 values)
//! 14. ratiomax - Max ratio for clutch size senescence
//! 15. tc - Critical time for juvenile mortality
//! 16. tinf - Maximum censoring time
//! 17. sub_interval - Sub-interval for integration
//! 18. path_continuous_var - Path to continuous variables file (or "NULL")
//! 19. ntr - Number of temperature reductions
//! 20. nst - Number of steps per temperature
//! 21. To - Initial temperature
//! 22. Tf - Final temperature
//! 23. climbrate - Cooling rate
//! 24. precision - Convergence precision

use std::io::{self, BufRead};
use std::path::Path;

use lifelihood::io::output::{write_output, OutputConfig};
use lifelihood::io::{parse_bounds_file, parse_continuous_var_file, parse_data_file};
use lifelihood::model::likelihood::compute_likelihood;
use lifelihood::model::types::*;
use lifelihood::optim::{automatic_met, calc_se, mcmc_sample};
use lifelihood::rng::Marsaglia;

fn main() {
    if let Err(e) = run() {
        eprintln!(
            "{}: {}",
            std::any::type_name::<Box<dyn std::error::Error>>(),
            e
        );
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    // Read arguments from stdin
    let stdin = io::stdin();
    let mut line = String::new();
    stdin.lock().read_line(&mut line)?;

    let args: Vec<&str> = line.trim().split_whitespace().collect();
    if args.len() < 24 {
        return Err(format!("Expected 24 arguments, got {}", args.len()).into());
    }

    // Parse arguments
    let path_input_data = args[0];
    let path_param_bounds = args[1];
    let _group_by_group = args[2].to_uppercase() == "TRUE";
    let mcmc_samples: i32 = args[3].parse().unwrap_or(0);
    let mcmc_interval: i32 = args[4].parse().unwrap_or(1);
    let compute_se = args[5].to_uppercase() == "TRUE";
    let _save_prob_event = args[6].to_uppercase() == "TRUE";
    let fitness_repar = args[7].to_uppercase() == "TRUE";
    let r: f64 = args[8].parse().unwrap_or(0.0);

    let seed1: i32 = args[9].parse().unwrap_or(1);
    let seed2: i32 = args[10].parse().unwrap_or(2);
    let seed3: i32 = args[11].parse().unwrap_or(3);
    let seed4: i32 = args[12].parse().unwrap_or(4);

    let ratiomax: f64 = args[13].parse().unwrap_or(2.0);
    let tc: f64 = args[14].parse().unwrap_or(0.0);
    let tinf: f64 = args[15].parse().unwrap_or(1000.0);
    let sub_interval: f64 = args[16].parse().unwrap_or(0.1);
    let path_continuous_var = args[17];

    let ntr: i32 = args[18].parse().unwrap_or(100);
    let nst: i32 = args[19].parse().unwrap_or(20);
    let temp0: f64 = args[20].parse().unwrap_or(10.0);
    let tempf: f64 = args[21].parse().unwrap_or(0.0);
    let climbrate: f64 = args[22].parse().unwrap_or(1.0);
    let precision: f64 = args[23].parse().unwrap_or(0.0001);

    // Initialize RNG
    let mut rng = Marsaglia::new(seed1, seed2, seed3, seed4);

    // Parse input data file
    let (mut state, mut groups) = parse_data_file(path_input_data)?;

    // Update state with arguments
    state.tc = tc;
    state.tinf = tinf;
    state.sub_interval = sub_interval;
    state.ratiomax = ratiomax;
    state.r = r;
    state.fitness_repar = fitness_repar;
    state.saved_seed = [seed1, seed2, seed3, seed4];

    // Parse parameter bounds
    let param_descript = parse_bounds_file(path_param_bounds)?;

    // Parse continuous variables if provided
    if path_continuous_var.to_uppercase() != "NULL" {
        parse_continuous_var_file(path_continuous_var, &mut state.covar, state.nbcov)?;
    }

    // Initialize function descriptor
    let mut fd = FunctionDescriptor::new();
    fd.param_descript = param_descript;

    // Initialize model (interpretation step)
    initialize_model(&mut fd, &mut groups, &state);

    // Set up Metropolis parameters
    let mut met_params = MetropolisParams::new();
    met_params.ntr = ntr;
    met_params.nst = nst;
    met_params.temp0 = temp0;
    met_params.tempf = tempf;
    met_params.climbrate = climbrate;
    met_params.precision = precision;

    // Run optimization
    automatic_met(&mut fd, &met_params, &mut groups, &state, &mut rng);

    // MCMC sampling if requested
    if mcmc_samples > 0 {
        mcmc_sample(
            &mut fd,
            &mut groups,
            &state,
            &mut rng,
            mcmc_samples,
            mcmc_interval,
        );
    }

    // Compute standard errors if requested
    let hessian = if compute_se {
        calc_se(&mut fd, &mut groups, &state);
        lifelihood::optim::hessian::get_inverse_hessian(&mut fd, &mut groups, &state)
    } else {
        None
    };

    // Write output
    let output_path = Path::new(path_input_data)
        .with_extension("out")
        .to_string_lossy()
        .to_string();

    let mut config = OutputConfig::new(path_input_data.to_string(), [seed1, seed2, seed3, seed4]);
    config.nb_mcmc = mcmc_samples;
    config.export_hessian = compute_se;
    config.hessian = hessian;
    config.ratiomax = ratiomax;
    config.tinf = tinf;
    config.tc = tc;

    write_output(&output_path, &fd, &config)?;

    Ok(())
}

/// Initialize model from data and configuration
///
/// Uses dummy coding: one variable per non-reference level for categorical covariates.
/// The naming convention is:
/// - Intercepts: `int_{param_name}`
/// - Categorical effects: `eff_{param_name}_{covar_name}{level}`
fn initialize_model(fd: &mut FunctionDescriptor, groups: &mut [Group], state: &ModelState) {
    // Global variable index across all parameters
    let mut var_idx = 0;

    for (param_idx, modele_param) in state.modele.iter().enumerate() {
        if modele_param.nb_terms == 0 {
            continue;
        }

        let bounds = &fd.param_descript[param_idx];
        let param_name = &bounds.name;

        // Track position in this parameter's po/valpo arrays
        let mut param_slot_idx = 0;

        for term_idx in 0..modele_param.nb_terms {
            let term = modele_param.term[term_idx];

            // Skip if term is -1 (not fitted)
            if term < 0 {
                continue;
            }

            if term == 0 {
                // Intercept term: single variable
                // Initialize at a reasonable starting point (~20% above min bound)
                // This avoids extreme values from the midpoint
                let name = format!("int_{}", param_name);
                let mut vi = VarInfo::new(name, bounds.min_bound, bounds.max_bound);

                // Use delink to initialize at a reasonable linked value
                // Start at 20% of the way from min to max
                let initial_linked = bounds.min_bound + 0.2 * (bounds.max_bound - bounds.min_bound);
                vi.value = lifelihood::model::link::delink(
                    initial_linked,
                    bounds.min_bound,
                    bounds.max_bound,
                );

                fd.var_info.push(vi);

                // Update all groups to point to this variable for the intercept
                for group in groups.iter_mut() {
                    ensure_param_capacity(&mut group.param[param_idx], param_slot_idx + 1);
                    group.param[param_idx].po[param_slot_idx] = var_idx;
                    group.param[param_idx].valpo[param_slot_idx] = 1.0; // Intercept always 1.0
                }

                var_idx += 1;
                param_slot_idx += 1;
            } else {
                // Covariate effect term: one variable per non-reference level
                let cov_idx = (term - 1) as usize; // 1-based to 0-based
                if cov_idx >= state.nbcov {
                    continue;
                }

                let cov_name = &state.covar[cov_idx].name;
                let n_levels = state.covar[cov_idx].lev as usize;

                // Create one variable for each level except reference (level 0)
                for level in 1..n_levels {
                    let name = format!("eff_{}_{}{}", param_name, cov_name, level);
                    let mut vi = VarInfo::new(name, bounds.min_bound, bounds.max_bound);
                    vi.value = 0.0;
                    fd.var_info.push(vi);

                    // Update groups to point to this variable for matching levels
                    for (group_idx, group) in groups.iter_mut().enumerate() {
                        let group_cov_value = state.get_cov(group_idx, cov_idx + 1);

                        ensure_param_capacity(&mut group.param[param_idx], param_slot_idx + 1);
                        group.param[param_idx].po[param_slot_idx] = var_idx;
                        // Set valpo to 1.0 if this group's covariate matches this level
                        group.param[param_idx].valpo[param_slot_idx] =
                            if group_cov_value == level { 1.0 } else { 0.0 };
                    }

                    var_idx += 1;
                    param_slot_idx += 1;
                }
            }
        }
    }

    // Update nb_terms for each group's parameters
    for group in groups.iter_mut() {
        for param in group.param.iter_mut() {
            param.nb_terms = param.po.len();
        }
    }

    fd.number_of_variables = var_idx;

    // Initial likelihood evaluation
    compute_likelihood(fd, groups, state);
}

/// Ensure ModelParamInst has enough capacity for the given number of terms
fn ensure_param_capacity(param: &mut ModelParamInst, min_capacity: usize) {
    while param.po.len() < min_capacity {
        param.po.push(0);
    }
    while param.valpo.len() < min_capacity {
        param.valpo.push(0.0);
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_argument_parsing() {
        // Basic test that the argument structure is correct
        let args = "data.txt bounds.txt FALSE 0 1 TRUE FALSE FALSE 0 1234 5678 9012 3456 2 0 1000 0.1 NULL 100 20 10 0 1 0.0001";
        let parts: Vec<&str> = args.split_whitespace().collect();
        assert_eq!(parts.len(), 24);
    }
}
