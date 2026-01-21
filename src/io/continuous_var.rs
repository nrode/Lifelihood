//! Continuous variable file parsing
//!
//! Port of read_custom_continuous_var from Unit1.pas

use crate::model::types::CovarInfo;
use std::fs::File;
use std::io::{BufRead, BufReader};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ContinuousVarError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Parse error at line {line}: {message}")]
    Parse { line: usize, message: String },
}

/// Parse the continuous variables file
///
/// # File Format
///
/// One line per covariate, space-separated values for each level.
///
/// ```text
/// 0.0 1.0 2.0
/// 0.5 1.5 2.5
/// ```
pub fn parse_continuous_var_file(
    path: &str,
    covar: &mut [CovarInfo],
    nbcov: usize,
) -> Result<(), ContinuousVarError> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    for (line_num, line_result) in reader.lines().enumerate() {
        if line_num >= nbcov {
            break;
        }

        let line = line_result?;
        let values: Vec<f64> = line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();

        // Update the continuous version of the covariate (at index nbcov + line_num)
        let cont_idx = nbcov + line_num;
        if cont_idx < covar.len() {
            covar[cont_idx].valcont = values;
        }
    }

    Ok(())
}

/// Initialize continuous variable values as ordinals
///
/// Used when no continuous variable file is provided.
pub fn init_continuous_var_ordinal(covar: &mut [CovarInfo], nbcov: usize) {
    for i in 0..nbcov {
        let cont_idx = nbcov + i;
        if cont_idx < covar.len() {
            let lev = covar[cont_idx].lev;
            covar[cont_idx].valcont = (0..lev).map(|j| j as f64).collect();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init_ordinal() {
        let mut covar = vec![
            CovarInfo::new("cov1".to_string(), 0, 3),
            CovarInfo::new("cov1".to_string(), 1, 3),
        ];

        init_continuous_var_ordinal(&mut covar, 1);

        assert_eq!(covar[1].valcont, vec![0.0, 1.0, 2.0]);
    }
}
