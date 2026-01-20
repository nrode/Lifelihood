//! Input file parsing
//!
//! Port of readata function from Unit1.pas

use crate::math::constants::NBPARPOSS;
use crate::model::types::*;
use std::fs::File;
use std::io::{BufRead, BufReader};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Parse error at line {line}: {message}")]
    Parse { line: usize, message: String },
    #[error("Invalid format: {0}")]
    Format(String),
}

/// Helper function to get next line from iterator
fn next_line(
    lines: &mut impl Iterator<Item = (usize, Result<String, std::io::Error>)>,
    description: &str,
) -> Result<(usize, String), ParseError> {
    match lines.next() {
        Some((num, Ok(line))) => Ok((num, line)),
        Some((num, Err(e))) => Err(ParseError::Parse {
            line: num,
            message: format!("IO error: {}", e),
        }),
        None => Err(ParseError::Format(format!("Missing {}", description))),
    }
}

/// Parse the main data input file
///
/// # File Format
///
/// ```text
/// *******data struct****
/// matclutch true|false
/// covar1 covar2 ...           # covariate names (or "none")
/// n1 n2 ...                   # number of levels per covariate
/// ****modele******
/// wei|exp|gam|lgn wei|exp|gam|lgn wei|exp|gam|lgn   # distributions
/// 0|-1 0|-1 ...               # model terms for each of 20 parameters
/// *******data*********
/// <individual life histories>
/// ```
pub fn parse_data_file(path: &str) -> Result<(ModelState, Vec<Group>), ParseError> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines().enumerate();

    let mut state = ModelState::new();

    // Skip header line "*******data struct****"
    let _ = lines.next();

    // Parse matclutch line
    let (_, matclutch_line) = next_line(&mut lines, "matclutch line")?;
    let parts: Vec<&str> = matclutch_line.split_whitespace().collect();
    if parts.len() >= 2 {
        state.matclutch = parts[1].to_lowercase() == "true";
    }

    // Parse covariate names
    let (_, cov_names_line) = next_line(&mut lines, "covariate names line")?;
    let cov_names: Vec<&str> = cov_names_line.split_whitespace().collect();

    if cov_names.is_empty() || cov_names[0].to_lowercase() == "none" {
        state.nbcov = 0;
    } else {
        state.nbcov = cov_names.len();

        // Parse covariate levels
        let (_, cov_levels_line) = next_line(&mut lines, "covariate levels line")?;
        let cov_levels: Vec<i32> = cov_levels_line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();

        // Create covariate info (factor and continuous versions)
        for (i, name) in cov_names.iter().enumerate() {
            let lev = cov_levels.get(i).copied().unwrap_or(2);

            // Factor version
            state.covar.push(CovarInfo {
                name: name.to_string(),
                typ: 0, // factor
                lev,
                valcont: Vec::new(),
            });
        }

        // Add continuous versions
        for (i, name) in cov_names.iter().enumerate() {
            let lev = cov_levels.get(i).copied().unwrap_or(2);

            state.covar.push(CovarInfo {
                name: name.to_string(),
                typ: 1, // continuous
                lev,
                valcont: (0..lev).map(|j| j as f64).collect(),
            });
        }
    }

    // Calculate number of groups
    state.nb_group = 1;
    for i in 0..state.nbcov {
        state.nb_group *= state.covar[i].lev as usize;
    }

    // Skip "****modele******" line
    let _ = lines.next();

    // Parse distribution names
    let (_, dist_line) = next_line(&mut lines, "distribution line")?;
    let dist_names: Vec<&str> = dist_line.split_whitespace().collect();

    let mort_dist = DistributionType::from_str(dist_names.get(0).unwrap_or(&"exp"))
        .unwrap_or(DistributionType::Exp);
    let mat_dist = DistributionType::from_str(dist_names.get(1).unwrap_or(&"exp"))
        .unwrap_or(DistributionType::Exp);
    let pon_dist = DistributionType::from_str(dist_names.get(2).unwrap_or(&"exp"))
        .unwrap_or(DistributionType::Exp);

    // Create groups
    let mut groups: Vec<Group> = (0..state.nb_group)
        .map(|_| Group::new(mort_dist, mat_dist, pon_dist))
        .collect();

    // Parse model parameters (20 lines)
    for param_idx in 0..NBPARPOSS {
        let (_, param_line) = next_line(&mut lines, &format!("parameter line {}", param_idx))?;
        let parts: Vec<i32> = param_line
            .split_whitespace()
            .skip(1) // Skip parameter name
            .filter_map(|s| s.parse().ok())
            .collect();

        state.modele[param_idx].nb_terms = parts.len();
        state.modele[param_idx].term = parts.clone();
        state.modele[param_idx].term_cov0 = vec![0; parts.len()];
        state.modele[param_idx].term_cov1 = vec![0; parts.len()];
        state.modele[param_idx].first_vi = vec![0; parts.len()];
        state.modele[param_idx].name_term = vec![String::new(); parts.len()];

        // Initialize parameter instances for each group
        for group in groups.iter_mut() {
            group.param[param_idx].nb_terms = parts.len();
            group.param[param_idx].po = vec![0; parts.len()];
            group.param[param_idx].valpo = vec![0.0; parts.len()];
        }
    }

    // Skip "*******data*********" line
    let _ = lines.next();

    // Parse individual life histories
    for (_line_num, line_result) in lines {
        let line = match line_result {
            Ok(l) => l,
            Err(_) => continue,
        };
        if line.trim().is_empty() {
            continue;
        }

        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.is_empty() {
            continue;
        }

        // Parse covariates to determine group
        let mut cov_values: Vec<i32> = Vec::new();
        for i in 0..state.nbcov {
            if let Some(val) = parts.get(i) {
                cov_values.push(val.parse().unwrap_or(0));
            }
        }

        let group_idx = state.get_group(&cov_values);
        if group_idx >= groups.len() {
            continue;
        }

        // Parse events from the line
        let life_history = parse_life_history(&parts, state.nbcov, state.matclutch, state.tinf)?;

        // Create individual
        let mut ind = Individual::new();
        ind.covariates = cov_values.iter().map(|&v| v as f64).collect();
        ind.life_histories.push(life_history);

        groups[group_idx].individuals.push(ind);
        groups[group_idx].use_flag = true;
    }

    // Post-processing: generate non-reproduction events and calculate calendars
    for group in groups.iter_mut() {
        for ind in group.individuals.iter_mut() {
            for hv in ind.life_histories.iter_mut() {
                generate_nop_events(hv, state.tinf);
                calculate_nb_ponte(hv);
                calculate_calendar(hv);
            }
        }
    }

    Ok((state, groups))
}

/// Parse a single life history from a data line
fn parse_life_history(
    parts: &[&str],
    nbcov: usize,
    matclutch: bool,
    tinf: f64,
) -> Result<LifeHistory, ParseError> {
    let mut hv = LifeHistory::new();
    let mut i = nbcov;

    while i < parts.len() {
        let event_name = parts[i].to_lowercase();

        match event_name.as_str() {
            "sex" => {
                if i + 3 < parts.len() {
                    let t1: f64 = parts[i + 1].parse().unwrap_or(0.0);
                    let t2: f64 = parts[i + 2].parse().unwrap_or(tinf);
                    let tp: i32 = parts[i + 3].parse().unwrap_or(0);

                    hv.events
                        .push(Event::new(EventType::Sex, t1, t2).with_tp(tp));
                    i += 4;
                } else {
                    i += 1;
                }
            }
            "mat" => {
                if i + 2 < parts.len() {
                    let t1: f64 = parts[i + 1].parse().unwrap_or(0.0);
                    let t2: f64 = parts[i + 2].parse().unwrap_or(tinf);
                    let mut event = Event::new(EventType::Mat, t1, t2);

                    if matclutch && i + 3 < parts.len() {
                        event.tp = parts[i + 3].parse().unwrap_or(0);
                        i += 4;
                    } else {
                        i += 3;
                    }

                    hv.events.push(event);
                } else {
                    i += 1;
                }
            }
            "pon" => {
                if i + 3 < parts.len() {
                    let t1: f64 = parts[i + 1].parse().unwrap_or(0.0);
                    let t2: f64 = parts[i + 2].parse().unwrap_or(tinf);
                    let tp: i32 = parts[i + 3].parse().unwrap_or(0);

                    hv.events
                        .push(Event::new(EventType::Pon, t1, t2).with_tp(tp));
                    i += 4;
                } else {
                    i += 1;
                }
            }
            "mor" => {
                if i + 2 < parts.len() {
                    let t1: f64 = parts[i + 1].parse().unwrap_or(0.0);
                    let t2: f64 = parts[i + 2].parse().unwrap_or(tinf);

                    hv.events.push(Event::new(EventType::Mor, t1, t2));
                    i += 3;
                } else {
                    i += 1;
                }
            }
            _ => {
                i += 1;
            }
        }
    }

    Ok(hv)
}

/// Generate non-reproduction events (nop)
fn generate_nop_events(hv: &mut LifeHistory, tinf: f64) {
    // Check if this is a female that matured
    let sex = hv
        .events
        .iter()
        .find(|e| e.event_type == EventType::Sex)
        .map(|e| e.tp)
        .unwrap_or(0);

    if sex != 0 {
        return; // Male, no nop events
    }

    // Check if matured
    let has_matured = hv
        .events
        .iter()
        .any(|e| e.event_type == EventType::Mat && e.t2 < tinf);

    if !has_matured {
        return;
    }

    // Find mortality event
    let mort_t1 = hv
        .events
        .iter()
        .find(|e| e.event_type == EventType::Mor)
        .map(|e| e.t1)
        .unwrap_or(tinf);

    // Add nop event
    let nop = Event::new(EventType::Nop, mort_t1, tinf);
    hv.events.push(nop);
}

/// Calculate number of reproduction events
fn calculate_nb_ponte(hv: &mut LifeHistory) {
    hv.nb_ponte = hv
        .events
        .iter()
        .filter(|e| e.event_type == EventType::Pon)
        .count() as i32;
}

/// Calculate debut/fin times for each event
fn calculate_calendar(hv: &mut LifeHistory) {
    for event in hv.events.iter_mut() {
        event.debut = event.t1;
        event.fin = event.t2;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_life_history() {
        let parts = vec![
            "0", "sex", "0", "1", "0", "mat", "5", "6", "mor", "100", "1000",
        ];
        let hv = parse_life_history(&parts, 1, false, 1000.0).unwrap();

        assert_eq!(hv.events.len(), 3);
        assert_eq!(hv.events[0].event_type, EventType::Sex);
        assert_eq!(hv.events[1].event_type, EventType::Mat);
        assert_eq!(hv.events[2].event_type, EventType::Mor);
    }
}
