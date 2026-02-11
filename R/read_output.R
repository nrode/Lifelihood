#' @title Read and format the output file of the program
#'
#' @description
#' Takes the file path of the output file and read the
#' results using parsers from [lifelihood::parse_output()].
#'
#' @keywords internal
#'
#' @param file_path Location of the output file of the program.
#' @param covariates Vector containing the names of the covariates.
#' @param path_config A character string specifying the file path
#' to the YAML configuration file.
#' @param MCMC Perform MCMC sampling of the parameter after
#' convergence to estimate their 95% confidence interval.
#'
#' @importFrom tidyr starts_with
#'
#' @return An object of class `lifelihoodResults` with all results
#' from the output file.
#'
#' @export
read_output_from_file <- function(
  file_path,
  covariates = NULL,
  path_config,
  MCMC
) {
  lines <- readLines(file_path)
  results <- list()

  seeds <- parse_output(lines, "seeds")
  likelihood <- parse_output(lines, "likelihood")
  effects <- parse_output(lines, "effects")
  parameter_ranges <- parse_output(lines, "parameter_ranges")
  ratiomax <- parse_output(lines, "ratio_max")
  inverse_hessian <- parse_output(lines, "hessian")

  if (MCMC > 0) {
    mcmc <- parse_output(lines, "mcmc")
  }

  get_event_covariates <- function(str_formula) {
    if (trimws(as.character(str_formula)) == "1") {
      covar_names <- c("intercept")
    } else if (str_formula == "not_fitted") {
      covar_names <- c()
    } else {
      covar_names <- strsplit(
        str_formula,
        split = "\\+"
      ) |>
        unlist() |>
        trimws()
    }
    return(covar_names)
  }

  results$config <- yaml::yaml.load_file(path_config, readLines.warn = FALSE)

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
      results$formula[[var]] <- get_event_covariates(results$config[[section]][[
        var
      ]])
    }
  }

  results$covariates <- covariates
  results$seeds <- seeds
  results$likelihood <- likelihood
  results$effects <- effects
  results$effects$parameter <- sapply(results$effects$name, map_parameter_name)
  results$effects$kind <- sapply(results$effects$name, find_parameter_kind)
  results$effects$event <- sapply(results$effects$parameter, find_event_type)
  results$parameter_ranges <- parameter_ranges
  results$ratiomax <- ratiomax

  if (!is.null(inverse_hessian)) {
    results$vcov <- -inverse_hessian
  }

  if (MCMC > 0) {
    results$mcmc_loglikelihood <- as.data.frame(t(mcmc))$LL
    mcmc_sample <- as.data.frame(t(mcmc)) |> dplyr::select(-LL)
    rownames(mcmc_sample) <- NULL
    results$mcmc_sample <- mcmc_sample
    mcmc <- lifelihood_mcmcse(results$mcmc_sample)
    results$mcmc_vcov <- mcmc$vcov
    mcmc$vcov <- NULL

    mcmc_se_df <- mcmc |> as.data.frame()
    mcmc_se_df$name <- rownames(mcmc_se_df)
    rownames(mcmc_se_df) <- NULL
    colnames(mcmc_se_df) <- c("mcmc_estimation", "mcmc_stderror", "name")

    results$mcmc_se <- mcmc_se_df

    results$effects <- results$effects |>
      left_join(
        results$mcmc_se,
        by = "name"
      ) |>
      relocate(mcmc_estimation, mcmc_stderror, .before = parameter)
  }

  class(results) <- "lifelihoodResults"
  return(results)
}
