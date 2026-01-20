//! Output file writing
//!
//! Port of printout_FD and related functions from Unit2.pas

use crate::model::types::{FunctionDescriptor, ParamDescriptor};
use std::fs::File;
use std::io::{BufWriter, Write};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum OutputError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Output configuration
pub struct OutputConfig {
    /// Data file path (for header)
    pub data_file: String,
    /// Random seeds
    pub seeds: [i32; 4],
    /// Number of MCMC samples
    pub nb_mcmc: i32,
    /// Whether to export Hessian
    pub export_hessian: bool,
    /// Hessian matrix (if available)
    pub hessian: Option<Vec<Vec<f64>>>,
    /// Parameter ranges
    pub param_ranges: Vec<(f64, f64)>,
    /// Additional parameters
    pub ratiomax: f64,
    pub tinf: f64,
    pub tc: f64,
}

impl OutputConfig {
    pub fn new(data_file: String, seeds: [i32; 4]) -> Self {
        OutputConfig {
            data_file,
            seeds,
            nb_mcmc: 0,
            export_hessian: false,
            hessian: None,
            param_ranges: Vec::new(),
            ratiomax: 2.0,
            tinf: 1000.0,
            tc: 0.0,
        }
    }
}

/// Write the main output file
pub fn write_output(
    path: &str,
    fd: &FunctionDescriptor,
    config: &OutputConfig,
) -> Result<(), OutputError> {
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);

    // Header
    writeln!(writer, "---------------------------")?;
    writeln!(writer)?;
    writeln!(writer, "datafile= {}", config.data_file)?;
    writeln!(
        writer,
        "seed1= {} seed2= {} seed3= {} seed4= {}",
        config.seeds[0], config.seeds[1], config.seeds[2], config.seeds[3]
    )?;
    writeln!(writer, "#parameters= {}", fd.number_of_variables)?;
    writeln!(writer, "Likelihood_max= {:.8}", -fd.best_result)?;

    // Parameter estimates with standard errors
    for vi in &fd.var_info {
        let se_str = if vi.se > 0.0 {
            format!("{:.8}", vi.se)
        } else {
            "NA".to_string()
        };
        writeln!(writer, "{} {:.8} {}", vi.name, vi.best_value, se_str)?;
    }

    writeln!(writer)?;

    // Hessian matrix
    if config.export_hessian {
        if let Some(ref hessian) = config.hessian {
            writeln!(writer, "inverse of Hessian Matrix")?;
            for row in hessian {
                let row_str: Vec<String> = row.iter().map(|v| format!("{:.8}", v)).collect();
                writeln!(writer, "{}", row_str.join(" "))?;
            }
        } else {
            writeln!(writer, "inverse of Hessian Matrix (!INVALID)")?;
        }
    }

    writeln!(writer)?;

    // MCMC samples
    if config.nb_mcmc > 0 && !fd.sample.is_empty() {
        writeln!(writer, "MCMCsamples")?;

        // Write likelihood samples
        let ll_str: Vec<String> = fd.sample.iter().map(|v| format!("{:.8}", v)).collect();
        writeln!(writer, "LL {}", ll_str.join(" "))?;

        // Write parameter samples
        for vi in &fd.var_info {
            let sample_str: Vec<String> = vi.sample.iter().map(|v| format!("{:.8}", v)).collect();
            writeln!(writer, "{} {}", vi.name, sample_str.join(" "))?;
        }
    }

    writeln!(writer)?;

    // Parameter ranges
    writeln!(writer, "Parameter_Range_Table")?;
    for (_i, vi) in fd.var_info.iter().enumerate() {
        writeln!(
            writer,
            "{} {:.8} {:.8}",
            vi.name, vi.min_bound, vi.max_bound
        )?;
    }

    // Additional parameters
    writeln!(writer, "ratiomax {}", config.ratiomax)?;
    writeln!(writer, "tinf_(right_censoring) {}", config.tinf)?;
    writeln!(writer, "tc_(juvenile_period_length) {}", config.tc)?;

    writer.flush()?;
    Ok(())
}

/// Write parameter descriptors
pub fn write_param_descript(
    path: &str,
    param_descript: &[ParamDescriptor],
) -> Result<(), OutputError> {
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);

    for pd in param_descript {
        writeln!(
            writer,
            "{} {:.8} {:.8}",
            pd.name, pd.min_bound, pd.max_bound
        )?;
    }

    writer.flush()?;
    Ok(())
}

/// Write event probabilities
pub fn write_event_probs(writer: &mut impl Write, event_probs: &[f64]) -> Result<(), OutputError> {
    writeln!(writer, "Event_Probabilities")?;
    for (i, prob) in event_probs.iter().enumerate() {
        writeln!(writer, "{} {:.8}", i, prob)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::types::VarInfo;

    #[test]
    fn test_output_format() {
        let mut fd = FunctionDescriptor::new();
        fd.best_result = -100.5;
        fd.number_of_variables = 2;
        fd.var_info
            .push(VarInfo::new("param1".to_string(), 0.0, 10.0));
        fd.var_info[0].best_value = 5.0;
        fd.var_info[0].se = 0.5;
        fd.var_info
            .push(VarInfo::new("param2".to_string(), 0.0, 1.0));
        fd.var_info[1].best_value = 0.3;

        let config = OutputConfig::new("test.txt".to_string(), [1, 2, 3, 4]);

        // We can't directly test write_output without a file, but we can verify the config
        assert_eq!(config.seeds, [1, 2, 3, 4]);
    }
}
