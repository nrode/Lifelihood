//! Parameter bounds configuration parsing
//!
//! Port of read_custom from Unit1.pas

use crate::math::constants::NBPARPOSS;
use crate::model::types::ParamDescriptor;
use std::fs::File;
use std::io::{BufRead, BufReader};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Parse error at line {line}: {message}")]
    Parse { line: usize, message: String },
}

/// Parse the parameter bounds configuration file
///
/// # File Format
///
/// ```text
/// param_name min_bound max_bound
/// expt_death 1 200
/// survival_param2 0.001 30
/// ...
/// ```
pub fn parse_bounds_file(path: &str) -> Result<Vec<ParamDescriptor>, ConfigError> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut params = Vec::with_capacity(NBPARPOSS);

    for (line_num, line_result) in reader.lines().enumerate() {
        let line = line_result?;
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.len() < 3 {
            continue;
        }

        let name = parts[0].to_string();
        let min_bound: f64 = parts[1].parse().map_err(|_| ConfigError::Parse {
            line: line_num + 1,
            message: format!("Invalid min bound: {}", parts[1]),
        })?;
        let max_bound: f64 = parts[2].parse().map_err(|_| ConfigError::Parse {
            line: line_num + 1,
            message: format!("Invalid max bound: {}", parts[2]),
        })?;

        params.push(ParamDescriptor::new(name, min_bound, max_bound));
    }

    // Pad with defaults if needed
    while params.len() < NBPARPOSS {
        params.push(ParamDescriptor::new(
            format!("param_{}", params.len()),
            0.0,
            1.0,
        ));
    }

    Ok(params)
}

/// Get default parameter bounds
pub fn default_bounds() -> Vec<ParamDescriptor> {
    vec![
        ParamDescriptor::new("expt_death".to_string(), 1.0, 200.0),
        ParamDescriptor::new("survival_param2".to_string(), 0.001, 30.0),
        ParamDescriptor::new("ratio_expt_death".to_string(), 0.001, 1000.0),
        ParamDescriptor::new("increase_death_hazard_juvenile".to_string(), 0.001, 100.0),
        ParamDescriptor::new("prop_male".to_string(), 0.001, 0.999),
        ParamDescriptor::new("expt_maturity".to_string(), 1.0, 200.0),
        ParamDescriptor::new("maturity_param2".to_string(), 0.001, 30.0),
        ParamDescriptor::new("ratio_expt_maturity".to_string(), 0.001, 1000.0),
        ParamDescriptor::new("expt_reproduction".to_string(), 1.0, 200.0),
        ParamDescriptor::new("reproduction_param2".to_string(), 0.001, 30.0),
        ParamDescriptor::new("n_offspring".to_string(), 0.001, 100.0),
        ParamDescriptor::new("increase_death_hazard".to_string(), 0.001, 10.0),
        ParamDescriptor::new("decrease_death_hazard".to_string(), 0.001, 10.0),
        ParamDescriptor::new("increase_death_hazard_offspring".to_string(), 0.001, 10.0),
        ParamDescriptor::new("senescence_repro_rate_linear".to_string(), -1.0, 1.0),
        ParamDescriptor::new("senescence_repro_rate_quadratic".to_string(), -1.0, 1.0),
        ParamDescriptor::new("senescence_offspring_linear".to_string(), -1.0, 1.0),
        ParamDescriptor::new("senescence_offspring_quadratic".to_string(), -1.0, 1.0),
        ParamDescriptor::new("tradeoff_interval_offspring".to_string(), -10.0, 10.0),
        ParamDescriptor::new("fitness".to_string(), 0.001, 1000.0),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_bounds() {
        let bounds = default_bounds();
        assert_eq!(bounds.len(), NBPARPOSS);
        assert_eq!(bounds[0].name, "expt_death");
        assert!(bounds[0].min_bound < bounds[0].max_bound);
    }
}
