#' @title Read and format the output file of the program
#'
#' @description
#' Takes the file path of the output file and read the
#' results using parsers from [lifelihood::parse_output()].
#'
#' @keywords internal
#'
#' @param file_path Location of the output file of the program
#' @inheritParams lifelihood
#'
#' @importFrom tidyr starts_with
#'
#' @return An object of class `lifelihoodResults` with all results from the output file
#'
#' @export
read_output_from_file <- function(
  file_path,
  group_by_group = FALSE,
  covariates = NULL,
  path_config
) {
  lines <- readLines(file_path)
  results <- list()

  seeds <- parse_output(lines, "seeds", group_by_group)
  likelihood <- parse_output(lines, "likelihood", group_by_group)
  effects <- parse_output(lines, "effects", group_by_group)
  parameter_ranges <- parse_output(lines, "parameter_ranges", group_by_group)
  ratiomax <- parse_output(lines, "ratio_max", group_by_group)
  mcmc_raw <- parse_output(lines, "mcmc")

  if (!is.null(mcmc_raw)) {
    mcmc_long <- tidyr::pivot_longer(
      data = mcmc_raw,
      cols = starts_with("Sample_"),
      names_to = "Sample",
      values_to = "Value"
    )

    mcmc_pivoted <- tidyr::pivot_wider(
      data = mcmc_long,
      names_from = Parameter,
      values_from = Value
    )

    mcmc_pivoted <- mcmc_pivoted[, -1]
    results$mcmc <- coda::mcmc(mcmc_pivoted)
    results$vcov <- mcmc_pivoted |> dplyr::select(-LL)
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
      "survival_shape",
      "ratio_expt_death",
      "prob_death",
      "sex_ratio"
    ),
    maturity = c("expt_maturity", "maturity_shape", "ratio_expt_maturity"),
    reproduction = c(
      "expt_reproduction",
      "reproduction_shape",
      "n_offspring",
      "increase_death_hazard",
      "tof_reduction_date",
      "increase_tof_n_offspring",
      "lin_decrease_hazard",
      "quad_decrease_hazard",
      "lin_change_n_offspring",
      "quad_change_n_offspring",
      "tof_n_offspring"
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
  results$group_by_group <- group_by_group
  class(results) <- "lifelihoodResults"
  return(results)
}
