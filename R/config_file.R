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

#' @keywords internal
config_parameter_sections <- function() {
  list(
    mortality = c(
      "expt_death",
      "survival_param2",
      "ratio_expt_death",
      "prob_death",
      "sex_ratio"
    ),
    maturity = c(
      "expt_maturity",
      "maturity_param2",
      "ratio_expt_maturity"
    ),
    reproduction = c(
      "expt_reproduction",
      "reproduction_param2",
      "n_offspring",
      "increase_death_hazard",
      "tof_decay",
      "increase_death_hazard_n_offspring",
      "lin_decrease_hazard",
      "quad_decrease_hazard",
      "lin_change_n_offspring",
      "quad_change_n_offspring",
      "tof_n_offspring",
      "fitness"
    )
  )
}

#' @keywords internal
validate_config_input <- function(config) {
  if (is.character(config) && length(config) == 1) {
    if (!file.exists(config)) {
      stop("Configuration file not found: ", config, call. = FALSE)
    }
    config <- yaml::yaml.load_file(config, readLines.warn = FALSE)
  }

  if (!is.list(config) || (length(config) > 0 && is.null(names(config)))) {
    stop(
      "`config` must be an existing YAML file path or a named configuration list.",
      call. = FALSE
    )
  }

  sections <- config_parameter_sections()
  if (
    length(config) > 0 &&
      (anyDuplicated(names(config)) > 0 || any(names(config) == ""))
  ) {
    stop(
      "Configuration sections must have unique, non-empty names.",
      call. = FALSE
    )
  }

  unknown_sections <- setdiff(names(config), names(sections))
  if (length(unknown_sections) > 0) {
    stop(
      "Unknown configuration section(s): ",
      paste(unknown_sections, collapse = ", "),
      call. = FALSE
    )
  }

  validated_config <- lapply(
    sections,
    function(parameters) {
      setNames(as.list(rep("not_fitted", length(parameters))), parameters)
    }
  )

  for (section in names(config)) {
    section_config <- config[[section]]
    if (
      !is.list(section_config) ||
        (length(section_config) > 0 && is.null(names(section_config)))
    ) {
      stop(
        "Configuration section `",
        section,
        "` must be a named list.",
        call. = FALSE
      )
    }
    if (
      length(section_config) > 0 &&
        (anyDuplicated(names(section_config)) > 0 ||
          any(names(section_config) == ""))
    ) {
      stop(
        "Parameters in configuration section `",
        section,
        "` must have unique, non-empty names.",
        call. = FALSE
      )
    }

    unknown_parameters <- setdiff(names(section_config), sections[[section]])
    if (length(unknown_parameters) > 0) {
      stop(
        "Unknown parameter(s) in configuration section `",
        section,
        "`: ",
        paste(unknown_parameters, collapse = ", "),
        call. = FALSE
      )
    }

    for (parameter in names(section_config)) {
      value <- section_config[[parameter]]
      if (!is.atomic(value) || length(value) != 1 || is.na(value)) {
        stop(
          "Configuration value `",
          section,
          ".",
          parameter,
          "` must be a single non-missing value.",
          call. = FALSE
        )
      }
      validated_config[[section]][[parameter]] <- value
    }
  }

  validated_config
}

#' @title Read and parse the configuration file (YAML).
#'
#' @description
#' Safely access the configuration file to use for lifelihood.
#' This function is used in [lifelihood()] when creating the
#' input text file.
#'
#' @param config A complete configuration list.
#' @param covariates Vector containing the names of the covariates.
#' @param covar_types Vector containing the types of the covariates
#' (either "cat" for categorical or "num" for numerical).
#'
#' @keywords internal
#'
#' @return A character vector that will be used under the model tag in the input text file.
format_config <- function(config, covariates, covar_types) {
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
      "tof_decay",
      R_to_lifelihood(
        safe_access(config, c("reproduction", "tof_decay")),
        covariates,
        covar_types
      )[1]
    ),
    paste(
      "increase_death_hazard_n_offspring",
      R_to_lifelihood(
        safe_access(
          config,
          c("reproduction", "increase_death_hazard_n_offspring")
        ),
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

#' @keywords internal
expand_formula_terms <- function(formula) {
  terms <- trimws(unlist(strsplit(formula, split = "\\+")))
  terms <- terms[nzchar(terms)]
  expanded_terms <- c()

  for (term in terms) {
    if (grepl("*", term, fixed = TRUE)) {
      interaction_terms <- trimws(unlist(strsplit(term, split = "\\*")))
      interaction_terms <- interaction_terms[nzchar(interaction_terms)]

      if (length(interaction_terms) != 2) {
        stop(
          "Interaction terms must involve exactly two covariates. Invalid term: `",
          term,
          "`"
        )
      }

      expanded_terms <- c(
        expanded_terms,
        interaction_terms,
        # rev() is here to make sure we have the same
        # order of levels in interactions and be consistent with R
        paste(rev(interaction_terms), collapse = ":")
      )
    } else if (grepl(":", term, fixed = TRUE)) {
      interaction_terms <- trimws(unlist(strsplit(term, split = ":")))
      interaction_terms <- interaction_terms[nzchar(interaction_terms)]

      if (length(interaction_terms) != 2) {
        stop(
          "Interaction terms must involve exactly two covariates. Invalid term: `",
          term,
          "`"
        )
      }

      expanded_terms <- c(
        expanded_terms,
        paste(interaction_terms, collapse = ":")
      )
    } else {
      expanded_terms <- c(expanded_terms, term)
    }
  }

  unique(expanded_terms)
}

extract_formula_covariates <- function(formula_terms) {
  all_covariates <- c()

  for (term in formula_terms) {
    if (grepl(":", term, fixed = TRUE)) {
      interaction_terms <- trimws(unlist(strsplit(term, split = ":")))
      all_covariates <- c(all_covariates, interaction_terms)
    } else {
      all_covariates <- c(all_covariates, term)
    }
  }

  unique(all_covariates)
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
    # expand interactions so `a * b` behaves as `a + b + a:b`
    used_covariables <- expand_formula_terms(R_format)
    n_element_parameter <- length(used_covariables)

    # list all covariates needed by both main effects and interactions
    all_covariables <- extract_formula_covariates(used_covariables)

    # ensure that all provided covariables are valid ones
    for (cov in all_covariables) {
      if (!(cov %in% covariates)) {
        stop("Unknown covariate: `", cov, "`")
      }
    }

    # create the lifelihood format output
    lifelihood_format <- "0"
    for (cov in used_covariables) {
      if (grepl(":", cov, fixed = TRUE)) {
        interaction_terms <- trimws(unlist(strsplit(cov, split = ":")))
        first_term <- which(covariates == interaction_terms[1])
        second_term <- which(covariates == interaction_terms[2])
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
