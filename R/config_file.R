#' @title safe access
#'
#' @description
#' Safely access elements in config file. This function
#' exists because yaml.load_file() returns NULL when a
#' value is not found instead of raising an error.
#'
#' @keywords internal
#'
#' @param config configuration object
#' @param path event and parameter to read
#'
#' @return the read value
safe_access <- function(config, path) {
  result <- tryCatch(
    {
      Reduce(`[[`, path, config)
    },
    error = function(e) {
      stop(paste(
        "Missing configuration element:",
        paste(path, collapse = " -> ")
      ))
    }
  )
  if (is.null(result)) {
    stop(paste(
      "Missing configuration element:",
      paste(path, collapse = " -> ")
    ))
  }
  result
}

#' @title Read and parse the configuration file (YAML).
#'
#' @description
#' Safely access the configuration file to use for lifelihood.
#' This function is used in [lifelihood()] when creating the
#' input text file.
#'
#' @param path_config A character string specifying the file path
#' to the YAML configuration file.
#' @param covariates Vector containing the names of the covariates.
#' @param covar_types Vector containing the types of the covariates
#' (either "cat" for categorical or "num" for numerical).
#'
#' @keywords internal
#'
#' @return A character vector that will be used under the model tag in the input text file.
format_config <- function(path_config, covariates, covar_types) {
  if (!file.exists(path_config)) {
    stop(paste(
      "Configuration file",
      path_config,
      "not found. Current working directory: ",
      getwd()
    ))
  }
  config <- yaml::yaml.load_file(path_config, readLines.warn = FALSE)

  formatted_config <- c(
    paste(
      "expt_death",
      R_to_lifelihood(
        safe_access(config, c("mortality", "expt_death")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "survival_param2",
      R_to_lifelihood(
        safe_access(config, c("mortality", "survival_param2")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "ratio_expt_death",
      R_to_lifelihood(
        safe_access(config, c("mortality", "ratio_expt_death")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "prob_death",
      R_to_lifelihood(
        safe_access(config, c("mortality", "prob_death")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "sex_ratio",
      R_to_lifelihood(
        safe_access(config, c("mortality", "sex_ratio")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "expt_maturity",
      R_to_lifelihood(
        safe_access(config, c("maturity", "expt_maturity")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "maturity_param2",
      R_to_lifelihood(
        safe_access(config, c("maturity", "maturity_param2")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "ratio_expt_maturity",
      R_to_lifelihood(
        safe_access(config, c("maturity", "ratio_expt_maturity")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "expt_reproduction",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "expt_reproduction")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "reproduction_param2",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "reproduction_param2")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "n_offspring",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "n_offspring")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "increase_death_hazard",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "increase_death_hazard")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "tof_reduction_date",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "tof_reduction_date")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "increase_tof_n_offspring",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "increase_tof_n_offspring")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "lin_decrease_hazard",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "lin_decrease_hazard")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "quad_decrease_hazard",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "quad_decrease_hazard")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "lin_change_n_offspring",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "lin_change_n_offspring")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "quad_change_n_offspring",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "quad_change_n_offspring")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "tof_n_offspring",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "tof_n_offspring")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "fitness",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "fitness")),
        covariates,
        covar_types
      )[1]
    )
  )

  return(formatted_config)
}

#' @title Read formula from config file
#'
#' @keywords internal
#'
#' @param config Configuration object loaded from YAML file.
#' @param parameter Name of the parameter to read formula for.
#'
#' @return Formula
read_formula <- function(config, parameter) {
  event <- find_event_type(parameter_name = parameter)
  formula <- safe_access(config, c(event, parameter))
  return(formula)
}


#' @title Convert R formula to lifelihood formula
#'
#' @description
#' Transforms a character string describing the covariates to be
#' included into a format which the compiled program can understand.
#' For example, `"geno + type"` will become `1 2` if `"geno"` is the
#' first element of `covariables` and `"type"` is the second. This
#' function is used to create the model part of the input text file.
#'
#' @param R_format String representing the covariates to be adjusted.
#' For example, "geno + type" will use the covariates geno and type.
#' @param covariates Vector containing the names of the covariates.
#' @param covar_types Vector containing the types of the covariates
#' (either "cat" for categorical or "num" for numerical).
#'
#' @keywords internal
#'
#' @return The formatted format for lifelihood to understand which
#' parameter to fit.
R_to_lifelihood <- function(R_format, covariates, covar_types) {
  # ensure input is a string
  R_format <- as.character(R_format)

  if (R_format == "not_fitted") {
    return("-1")
  } else if (R_format == "1") {
    return("0")
  } else {
    # get a list of each covariable (separated by '+')
    used_covariables <- trimws(unlist(strsplit(R_format, split = "\\+")))
    n_element_parameter <- length(used_covariables)

    # initiate a list of all covariables
    all_covariables <- c()

    # create a list of all covariables used
    for (cov in used_covariables) {
      if (grepl("*", cov, fixed = TRUE)) {
        interaction_covs <- trimws(unlist(strsplit(cov, split = "\\*")))
        all_covariables <- c(all_covariables, interaction_covs)
      } else {
        all_covariables <- c(all_covariables, cov)
      }
    }
    all_covariables <- unique(all_covariables)

    # ensure that all provided covariables are valid ones
    for (cov in all_covariables) {
      if (!(cov %in% covariates)) {
        stop("Unknown covariate: `", cov, "`")
      }
    }

    # create the lifelihood format output
    lifelihood_format <- "0"
    for (cov in used_covariables) {
      if (grepl("*", cov, fixed = TRUE)) {
        interaction_terms <- strsplit(cov, split = "\\*")
        first_term <- which(covariates == interaction_terms[[1]][1])
        second_term <- which(covariates == interaction_terms[[1]][2])
        position <- paste(first_term, second_term, sep = "")
      } else {
        position <- which(covariates == cov)
        if (covar_types[which(cov == covariates)] == "num") {
          position <- position + length(covariates)
        }
      }
      lifelihood_format <- paste(lifelihood_format, position)
    }
  }

  return(c(lifelihood_format, n_element_parameter))
}
