#' @title Validate group-by-group configuration
#'
#' @description
#' Checks that all fitted parameters in the config use the same
#' covariate formula, as required for group-by-group fitting.
#'
#' @param config Configuration list loaded from YAML file.
#'
#' @return The single common formula string (e.g., `"par"` or `"par * geno"`).
#'
#' @keywords internal
validate_group_by_group_config <- function(config) {
  sections <- list(
    mortality = c(
      "expt_death",
      "survival_param2",
      "ratio_expt_death",
      "prob_death",
      "sex_ratio"
    ),
    maturity = c("expt_maturity", "maturity_param2", "ratio_expt_maturity"),
    reproduction = c(
      "expt_reproduction",
      "reproduction_param2",
      "n_offspring",
      "increase_death_hazard",
      "tof_reduction_date",
      "increase_tof_n_offspring",
      "lin_decrease_hazard",
      "quad_decrease_hazard",
      "lin_change_n_offspring",
      "quad_change_n_offspring",
      "tof_n_offspring",
      "fitness"
    )
  )

  non_trivial_formulas <- c()
  for (section in names(sections)) {
    for (param in sections[[section]]) {
      val <- config[[section]][[param]]
      if (!is.null(val) && val != "not_fitted") {
        non_trivial_formulas <- c(non_trivial_formulas, val)
      }
    }
  }

  if (length(non_trivial_formulas) == 0) {
    stop(
      "`group_by_group` argument requires at least one fitted parameter with covariates ",
      "(i.e., not '1' or 'not_fitted'). No covariate formulas found in config."
    )
  }

  unique_formulas <- unique(non_trivial_formulas)
  if (length(unique_formulas) > 1) {
    stop(
      "`group_by_group` argument requires the same covariate(s) for all fitted parameters ",
      "across mortality, maturity and reproduction. Found different formulas ",
      "across mortality, maturity and reproduction: ",
      paste(unique_formulas, collapse = ", ")
    )
  }

  return(unique_formulas)
}

#' @title Extract covariate names from a formula string
#'
#' @description
#' Parses a formula string (e.g., `"par * geno"`) to extract
#' the individual covariate names.
#'
#' @param common_formula A character string with the formula.
#'
#' @return A character vector of unique covariate names.
#'
#' @keywords internal
extract_group_covariates <- function(common_formula) {
  parts <- trimws(unlist(strsplit(common_formula, "\\+")))
  covariates <- c()
  for (part in parts) {
    sub_parts <- trimws(unlist(strsplit(part, "\\*")))
    covariates <- c(covariates, sub_parts)
  }
  return(unique(covariates))
}

#' @title Split data by covariate groups
#'
#' @description
#' Creates sub-datasets by splitting the data according to
#' the unique combinations of group covariates.
#'
#' @param lifelihoodData A `lifelihoodData` object.
#' @param group_covariates Character vector of covariate names.
#'
#' @return A named list of `lifelihoodData` objects, one per group.
#'
#' @keywords internal
split_data_by_groups <- function(lifelihoodData, group_covariates) {
  df <- lifelihoodData$df

  if (length(group_covariates) == 1) {
    group_col <- as.factor(df[[group_covariates]])
  } else {
    group_col <- interaction(df[group_covariates], drop = TRUE)
  }

  group_levels <- levels(group_col)
  sub_datasets <- list()

  for (lvl in group_levels) {
    idx <- which(group_col == lvl)
    if (length(idx) == 0) {
      next
    }

    if (length(idx) < 10) {
      warning(
        "Group '",
        lvl,
        "' has only ",
        length(idx),
        " observations. Results may be unreliable."
      )
    }

    sub_df <- df[idx, , drop = FALSE]
    rownames(sub_df) <- NULL

    # Drop unused factor levels in sub-dataset
    sub_df <- droplevels(sub_df)

    sub_data <- lifelihoodData
    sub_data$df <- sub_df
    sub_datasets[[lvl]] <- sub_data
  }

  return(sub_datasets)
}

#' @title Create intercept-only config
#'
#' @description
#' Takes the original config and replaces every non-`"not_fitted"`
#' formula with `"1"` (intercept only). Writes the result to a
#' temporary YAML file.
#'
#' @param original_config Configuration list loaded from YAML.
#' @param temp_dir Directory where the temp config should be written.
#'
#' @return The file path to the temporary YAML config.
#'
#' @keywords internal
create_intercept_only_config <- function(original_config, temp_dir) {
  new_config <- original_config

  sections <- list(
    mortality = c(
      "expt_death",
      "survival_param2",
      "ratio_expt_death",
      "prob_death",
      "sex_ratio"
    ),
    maturity = c("expt_maturity", "maturity_param2", "ratio_expt_maturity"),
    reproduction = c(
      "expt_reproduction",
      "reproduction_param2",
      "n_offspring",
      "increase_death_hazard",
      "tof_reduction_date",
      "increase_tof_n_offspring",
      "lin_decrease_hazard",
      "quad_decrease_hazard",
      "lin_change_n_offspring",
      "quad_change_n_offspring",
      "tof_n_offspring",
      "fitness"
    )
  )

  for (section in names(sections)) {
    for (param in sections[[section]]) {
      val <- new_config[[section]][[param]]
      if (!is.null(val) && val != "not_fitted") {
        if (!is.numeric(val)) {
          new_config[[section]][[param]] <- "1"
        }
      }
    }
  }

  temp_config_path <- file.path(temp_dir, "config_intercept_only.yaml")
  yaml::write_yaml(new_config, temp_config_path)
  return(temp_config_path)
}

#' @title Merge group-by-group results
#'
#' @description
#' Combines the results from individual group fits into a single
#' `lifelihoodResults` object.
#'
#' @param group_results Named list of `lifelihoodResults` objects.
#' @param group_names Character vector of group labels.
#' @param original_config Original config list from YAML.
#' @param original_lifelihoodData Original `lifelihoodData` object.
#'
#' @return A `lifelihoodResults` object with merged results.
#'
#' @keywords internal
merge_group_results <- function(
  group_results,
  group_names,
  original_config,
  original_lifelihoodData
) {
  results <- list()

  # Sum likelihoods
  group_likelihoods <- sapply(group_results, function(x) x$likelihood)
  results$likelihood <- sum(group_likelihoods)

  # Merge effects, renaming names to include group label
  all_effects <- list()
  for (i in seq_along(group_results)) {
    grp <- group_names[i]
    eff <- group_results[[i]]$effects
    eff$name <- paste0(eff$name, "_", grp)
    eff$group <- grp
    all_effects[[i]] <- eff
  }
  results$effects <- do.call(rbind, all_effects)
  rownames(results$effects) <- NULL

  # Build formula list (same as read_output.R does)
  get_event_covariates <- function(str_formula) {
    if (trimws(as.character(str_formula)) == "1") {
      return(c("intercept"))
    } else if (str_formula == "not_fitted") {
      return(c())
    } else {
      return(trimws(unlist(strsplit(str_formula, split = "\\+"))))
    }
  }

  sections <- list(
    mortality = c(
      "expt_death",
      "survival_param2",
      "ratio_expt_death",
      "prob_death",
      "sex_ratio"
    ),
    maturity = c("expt_maturity", "maturity_param2", "ratio_expt_maturity"),
    reproduction = c(
      "expt_reproduction",
      "reproduction_param2",
      "n_offspring",
      "increase_death_hazard",
      "tof_reduction_date",
      "increase_tof_n_offspring",
      "lin_decrease_hazard",
      "quad_decrease_hazard",
      "lin_change_n_offspring",
      "quad_change_n_offspring",
      "tof_n_offspring",
      "fitness"
    )
  )

  for (section in names(sections)) {
    for (var in sections[[section]]) {
      val <- original_config[[section]][[var]]
      if (!is.null(val)) {
        results$formula[[var]] <- get_event_covariates(val)
      }
    }
  }

  results$config <- original_config
  results$covariates <- original_lifelihoodData$covariates
  results$lifelihoodData <- original_lifelihoodData
  results$sample_size <- nrow(original_lifelihoodData$df)
  results$seeds <- group_results[[1]]$seeds
  results$parameter_ranges <- group_results[[1]]$parameter_ranges
  results$vcov <- NULL
  results$MCMC <- 0
  results$se.fit <- FALSE

  results$group_by_group <- TRUE
  results$group_names <- group_names
  results$group_likelihoods <- group_likelihoods

  class(results) <- "lifelihoodResults"
  return(results)
}

#' @title Fit lifelihood group by group
#'
#' @description
#' Orchestrator function that splits data by covariate groups,
#' fits intercept-only models for each group, and merges results.
#'
#' @inheritParams lifelihood
#'
#' @return A `lifelihoodResults` object with group-by-group results.
#'
#' @keywords internal
lifelihood_fit_group_by_group <- function(
  lifelihoodData,
  path_config,
  path_to_Lifelihood = NULL,
  param_bounds_df = NULL,
  MCMC = 0,
  interval = 25,
  se.fit = FALSE,
  saveprobevent = 0,
  r = 0,
  seeds = NULL,
  ntr = 2,
  nst = 2,
  To = 50,
  Tf = 1,
  climbrate = 1,
  precision = 0.001,
  ratiomax = 10,
  tc = 20,
  tinf = 1000,
  sub_interval = 0.3,
  delete_temp_files = TRUE
) {
  config <- yaml::yaml.load_file(path_config, readLines.warn = FALSE)

  common_formula <- validate_group_by_group_config(config)
  group_covariates <- extract_group_covariates(common_formula)
  sub_datasets <- split_data_by_groups(lifelihoodData, group_covariates)
  group_names <- names(sub_datasets)

  # Create a temp dir for the intercept-only config
  gbg_temp_dir <- file.path(
    here::here(),
    paste0("lifelihood_gbg_", paste(sample(1:10000, 4), collapse = "_"))
  )
  dir.create(gbg_temp_dir, showWarnings = FALSE)
  intercept_config_path <- create_intercept_only_config(config, gbg_temp_dir)

  if (is.null(seeds)) {
    seeds <- sample(1:10000, 4, replace = TRUE)
  }

  group_results <- list()
  group_temp_dirs <- c()
  for (i in seq_along(group_names)) {
    grp <- group_names[i]
    message("Fitting group: ", grp)
    sub_data <- sub_datasets[[grp]]

    # Generate unique seeds per group to ensure unique temp dirs
    group_seeds <- seeds + i

    result <- tryCatch(
      {
        lifelihood_fit(
          lifelihoodData = sub_data,
          path_config = intercept_config_path,
          path_to_Lifelihood = path_to_Lifelihood,
          param_bounds_df = param_bounds_df,
          group_by_group = FALSE,
          MCMC = MCMC,
          interval = interval,
          se.fit = se.fit,
          saveprobevent = saveprobevent,
          r = r,
          seeds = group_seeds,
          ntr = ntr,
          nst = nst,
          To = To,
          Tf = Tf,
          climbrate = climbrate,
          precision = precision,
          ratiomax = ratiomax,
          tc = tc,
          tinf = tinf,
          sub_interval = sub_interval,
          raise_estimation_warning = FALSE,
          delete_temp_files = FALSE
        )
      },
      error = function(e) {
        warning("Failed to fit group '", grp, "': ", conditionMessage(e))
        return(NULL)
      }
    )

    # Track temp dir for cleanup
    grp_temp_dir <- file.path(
      here::here(),
      paste0("lifelihood_", paste(group_seeds, collapse = "_"))
    )
    group_temp_dirs <- c(group_temp_dirs, grp_temp_dir)

    if (!is.null(result)) {
      group_results[[grp]] <- result
    }
  }

  if (length(group_results) == 0) {
    stop("All group fits failed. Cannot produce group_by_group results.")
  }

  merged <- merge_group_results(
    group_results = group_results,
    group_names = names(group_results),
    original_config = config,
    original_lifelihoodData = lifelihoodData
  )

  # Clean up temp files
  if (delete_temp_files) {
    unlink(gbg_temp_dir, recursive = TRUE)
    for (td in group_temp_dirs) {
      unlink(td, recursive = TRUE)
    }
  }

  return(merged)
}
