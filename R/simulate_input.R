#' @title Create a lifelihood-like object for simulation
#'
#' @description
#' Builds a `lifelihoodResults`-like object from manually supplied parameter
#' effects. The returned object can be passed to [simulate_life_history()] in
#' the same way as the output of [lifelihood()].
#'
#' The `effects` list must contain one entry for every parameter fitted in
#' `config`. For intercept-only parameters, use a scalar or `list(1)`. For
#' categorical covariates, use one vector per covariate. When no explicit
#' `intercept` is supplied, the first value of each covariate vector is used as
#' the shared intercept and the remaining values are used as non-reference level
#' effects.
#'
#' @param effects Named list of parameter effects on the lifelihood link scale.
#' @param data Data frame containing one row per simulated individual, or one
#'   row per combination when `n_per_combination` is supplied. It must contain
#'   the columns named in `covariates` and `sex`.
#' @param covariates Character vector with covariate column names in `data`.
#' @param sex Name of the sex column in `data`.
#' @param config Path to a YAML configuration file or an already-loaded
#'   configuration list.
#' @param model Character vector with one model family to reuse for mortality,
#'   maturity and reproduction, or three model families in that order. Values
#'   must be `"wei"`, `"exp"`, `"gam"` or `"lgn"`.
#' @param n_per_combination Optional name of a column in `data` containing the
#'   number of individuals to create for each row.
#' @param param_bounds_df Optional data frame with columns `param`, `min` and
#'   `max`. If `NULL`, defaults are created with [default_bounds_df()].
#' @param right_censoring_date Numeric right censoring date stored in the
#'   internal `lifelihoodData` object.
#'
#' @return A `lifelihoodResults` object suitable for [simulate_life_history()].
#'
#' @export
create_simulation_input <- function(
  effects,
  data,
  covariates,
  sex,
  config,
  model,
  n_per_combination = NULL,
  param_bounds_df = NULL,
  right_censoring_date = 1000
) {
  if (missing(data) || is.null(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  config <- load_simulation_config(config)
  model_specs <- normalize_simulation_model(model)
  covariates <- validate_simulation_covariates(covariates)
  sex <- validate_simulation_sex_column(sex)
  validate_simulation_effects(effects)

  df <- build_simulation_data(
    data = data,
    covariates = covariates,
    sex = sex,
    n_per_combination = n_per_combination
  )
  n <- nrow(df)

  fitted_parameters <- get_simulation_fitted_parameters(config)
  missing_effects <- setdiff(fitted_parameters, names(effects))
  if (length(missing_effects) > 0) {
    stop(
      "`effects` is missing fitted parameter",
      if (length(missing_effects) == 1) "" else "s",
      ": ",
      paste(missing_effects, collapse = ", "),
      call. = FALSE
    )
  }

  extra_effects <- setdiff(names(effects), fitted_parameters)
  if (length(extra_effects) > 0) {
    stop(
      "`effects` contains parameter",
      if (length(extra_effects) == 1) "" else "s",
      " not fitted in `config`: ",
      paste(extra_effects, collapse = ", "),
      call. = FALSE
    )
  }

  formula_covariates <- get_simulation_formula_covariates(config)
  unknown_covariates <- setdiff(formula_covariates, covariates)
  if (length(unknown_covariates) > 0) {
    stop(
      "Formula covariate",
      if (length(unknown_covariates) == 1) "" else "s",
      " must be listed in `covariates`: ",
      paste(unknown_covariates, collapse = ", "),
      call. = FALSE
    )
  }

  formula <- build_simulation_formula_list(config)
  effects_df <- build_simulation_effects_df(
    effects = effects,
    config = config,
    fitted_parameters = fitted_parameters,
    df = df,
    covariates = covariates
  )

  lifelihoodData <- build_simulation_lifelihood_data(
    df = df,
    covariates = covariates,
    sex = sex,
    model_specs = model_specs,
    right_censoring_date = right_censoring_date
  )

  if (is.null(param_bounds_df)) {
    param_bounds_df <- default_bounds_df(lifelihoodData)
  }
  param_bounds_df <- normalize_simulation_bounds_df(param_bounds_df)

  missing_bounds <- setdiff(fitted_parameters, param_bounds_df$param)
  if (length(missing_bounds) > 0) {
    stop(
      "`param_bounds_df` is missing fitted parameter",
      if (length(missing_bounds) == 1) "" else "s",
      ": ",
      paste(missing_bounds, collapse = ", "),
      call. = FALSE
    )
  }

  results <- list(
    config = config,
    covariates = covariates,
    formula = formula,
    effects = effects_df,
    likelihood = NA_real_,
    seeds = NA_real_,
    parameter_ranges = data.frame(
      name = param_bounds_df$param,
      min = param_bounds_df$min,
      max = param_bounds_df$max
    ),
    ratiomax = lifelihoodData$ratiomax,
    vcov = NULL,
    MCMC = 0,
    se.fit = FALSE,
    lifelihoodData = lifelihoodData,
    sample_size = n,
    param_bounds_df = param_bounds_df,
    group_by_group = FALSE
  )

  class(results) <- "lifelihoodResults"
  results
}

#' @keywords internal
simulation_parameter_sections <- function() {
  list(
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
load_simulation_config <- function(config) {
  if (is.character(config) && length(config) == 1) {
    if (!file.exists(config)) {
      stop("Configuration file not found: ", config, call. = FALSE)
    }
    return(yaml::yaml.load_file(config, readLines.warn = FALSE))
  }

  if (is.list(config)) {
    return(config)
  }

  stop(
    "`config` must be a YAML file path or a configuration list.",
    call. = FALSE
  )
}

#' @keywords internal
normalize_simulation_model <- function(model) {
  valid_models <- c("wei", "gam", "lgn", "exp")
  model <- as.character(model)

  if (length(model) == 1) {
    model <- rep(model, 3)
  }

  if (length(model) != 3 || !all(model %in% valid_models)) {
    stop(
      "`model` must be length 1 or 3 and contain only: ",
      paste(valid_models, collapse = ", "),
      call. = FALSE
    )
  }

  model
}

#' @keywords internal
validate_simulation_covariates <- function(covariates) {
  covariates <- as.character(covariates)
  if (anyNA(covariates) || !all(nzchar(covariates))) {
    stop("`covariates` must contain non-empty names.", call. = FALSE)
  }
  unique(covariates)
}

#' @keywords internal
validate_simulation_sex_column <- function(sex) {
  sex <- as.character(sex)
  if (length(sex) != 1 || is.na(sex) || !nzchar(sex)) {
    stop("`sex` must be a single non-empty column name.", call. = FALSE)
  }
  sex
}

#' @keywords internal
validate_simulation_effects <- function(effects) {
  if (!is.list(effects) || is.null(names(effects))) {
    stop("`effects` must be a named list.", call. = FALSE)
  }

  if (!all(nzchar(names(effects)))) {
    stop("Every entry in `effects` must be named.", call. = FALSE)
  }

  invisible(TRUE)
}

#' @keywords internal
is_simulation_formula_fitted <- function(value) {
  !is.null(value) && trimws(as.character(value)) != "not_fitted"
}

#' @keywords internal
get_simulation_fitted_parameters <- function(config) {
  sections <- simulation_parameter_sections()
  fitted <- character()

  for (section in names(sections)) {
    for (parameter in sections[[section]]) {
      value <- config[[section]][[parameter]]
      if (is_simulation_formula_fitted(value)) {
        fitted <- c(fitted, parameter)
      }
    }
  }

  fitted
}

#' @keywords internal
get_simulation_formula_covariates <- function(config) {
  fitted_parameters <- get_simulation_fitted_parameters(config)
  covariates <- character()

  for (parameter in fitted_parameters) {
    value <- read_formula(config, parameter)
    if (trimws(as.character(value)) != "1") {
      covariates <- c(covariates, expand_formula_terms(as.character(value)))
    }
  }

  unique(extract_formula_covariates(covariates))
}

#' @keywords internal
build_simulation_data <- function(
  data,
  covariates,
  sex,
  n_per_combination
) {
  df <- as.data.frame(data)
  if (nrow(df) < 1) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  if (sex %in% covariates) {
    stop("`sex` cannot also be listed in `covariates`.", call. = FALSE)
  }

  required_columns <- c(covariates, sex)
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(
      "`data` is missing column",
      if (length(missing_columns) == 1) "" else "s",
      ": ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(n_per_combination)) {
    count_column <- validate_simulation_count_column(
      n_per_combination = n_per_combination,
      data_names = names(df),
      reserved_names = required_columns
    )
    counts <- normalize_simulation_count_values(df[[count_column]])
    df <- df[rep(seq_len(nrow(df)), counts), , drop = FALSE]
    df[[count_column]] <- NULL
  }

  rownames(df) <- NULL
  df
}

#' @keywords internal
validate_simulation_count_column <- function(
  n_per_combination,
  data_names,
  reserved_names
) {
  n_per_combination <- as.character(n_per_combination)
  if (
    length(n_per_combination) != 1 ||
      is.na(n_per_combination) ||
      !nzchar(n_per_combination)
  ) {
    stop(
      "`n_per_combination` must be a single non-empty column name.",
      call. = FALSE
    )
  }

  if (!(n_per_combination %in% data_names)) {
    stop(
      "`data` is missing `n_per_combination` column: ",
      n_per_combination,
      call. = FALSE
    )
  }

  if (n_per_combination %in% reserved_names) {
    stop(
      "`n_per_combination` cannot name a covariate or sex column.",
      call. = FALSE
    )
  }

  n_per_combination
}

#' @keywords internal
normalize_simulation_count_values <- function(counts) {
  if (is.factor(counts)) {
    counts <- as.character(counts)
  }
  counts <- suppressWarnings(as.numeric(counts))

  invalid_counts <- is.na(counts) |
    !is.finite(counts) |
    counts < 1 |
    counts != as.integer(counts)

  if (any(invalid_counts)) {
    stop(
      "`n_per_combination` values must be positive whole numbers.",
      call. = FALSE
    )
  }

  as.integer(counts)
}

#' @keywords internal
add_simulation_life_history_placeholders <- function(
  df,
  right_censoring_date
) {
  base_placeholder_names <- c(
    sex_start = ".simulation_sex_start",
    sex_end = ".simulation_sex_end",
    maturity_start = ".simulation_maturity_start",
    maturity_end = ".simulation_maturity_end",
    clutch_start1 = ".simulation_clutch_start1",
    clutch_end1 = ".simulation_clutch_end1",
    clutch_size1 = ".simulation_clutch_size1",
    death_start = ".simulation_death_start",
    death_end = ".simulation_death_end"
  )
  all_names <- make.unique(c(names(df), base_placeholder_names), sep = "_")
  placeholder_names <- all_names[
    (length(names(df)) + 1):length(all_names)
  ]
  names(placeholder_names) <- names(base_placeholder_names)

  time_value <- right_censoring_date / 2

  df[[placeholder_names[["sex_start"]]]] <- NA_real_
  df[[placeholder_names[["sex_end"]]]] <- NA_real_
  df[[placeholder_names[["maturity_start"]]]] <- time_value
  df[[placeholder_names[["maturity_end"]]]] <- time_value
  df[[placeholder_names[["clutch_start1"]]]] <- time_value
  df[[placeholder_names[["clutch_end1"]]]] <- time_value
  df[[placeholder_names[["clutch_size1"]]]] <- 25
  df[[placeholder_names[["death_start"]]]] <- time_value
  df[[placeholder_names[["death_end"]]]] <- time_value

  list(df = df, columns = placeholder_names)
}

#' @keywords internal
normalize_simulation_bounds_df <- function(param_bounds_df) {
  param_bounds_df <- as.data.frame(param_bounds_df)

  if (
    !("param" %in% names(param_bounds_df)) && "name" %in% names(param_bounds_df)
  ) {
    names(param_bounds_df)[names(param_bounds_df) == "name"] <- "param"
  }

  required_columns <- c("param", "min", "max")
  missing_columns <- setdiff(required_columns, names(param_bounds_df))
  if (length(missing_columns) > 0) {
    stop(
      "`param_bounds_df` must contain column",
      if (length(missing_columns) == 1) "" else "s",
      ": ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  param_bounds_df <- param_bounds_df[, required_columns]
  param_bounds_df$param <- as.character(param_bounds_df$param)
  param_bounds_df$min <- as.numeric(param_bounds_df$min)
  param_bounds_df$max <- as.numeric(param_bounds_df$max)

  invalid_bounds <- !is.finite(param_bounds_df$min) |
    !is.finite(param_bounds_df$max) |
    param_bounds_df$min >= param_bounds_df$max

  if (any(invalid_bounds)) {
    stop(
      "`param_bounds_df` contains invalid bounds for: ",
      paste(param_bounds_df$param[invalid_bounds], collapse = ", "),
      call. = FALSE
    )
  }

  param_bounds_df
}

#' @keywords internal
build_simulation_formula_list <- function(config) {
  fitted_parameters <- get_simulation_fitted_parameters(config)
  formula <- list()

  for (parameter in fitted_parameters) {
    value <- read_formula(config, parameter)
    if (trimws(as.character(value)) == "1") {
      formula[[parameter]] <- c("intercept")
    } else {
      formula[[parameter]] <- expand_formula_terms(as.character(value))
    }
  }

  formula
}

#' @keywords internal
build_simulation_effects_df <- function(
  effects,
  config,
  fitted_parameters,
  df,
  covariates
) {
  all_effects <- list()

  for (parameter in fitted_parameters) {
    formula_string <- as.character(read_formula(config, parameter))
    design_matrix <- simulation_model_matrix(formula_string, df)
    design_columns <- colnames(design_matrix)
    effect_names <- make_simulation_effect_names(
      parameter = parameter,
      design_columns = design_columns,
      df = df,
      covariates = covariates
    )

    estimates <- normalize_parameter_effects(
      parameter = parameter,
      parameter_effects = effects[[parameter]],
      design_columns = design_columns,
      effect_names = effect_names,
      covariates = covariates
    )

    parameter_effects_df <- data.frame(
      name = effect_names,
      estimation = as.numeric(estimates),
      stderror = NA_real_,
      stringsAsFactors = FALSE
    )
    all_effects[[parameter]] <- parameter_effects_df
  }

  effects_df <- do.call(rbind, all_effects)
  rownames(effects_df) <- NULL
  effects_df$parameter <- vapply(
    effects_df$name,
    map_parameter_name,
    character(1)
  )
  effects_df$kind <- vapply(effects_df$name, find_parameter_kind, character(1))
  effects_df$event <- vapply(
    effects_df$parameter,
    find_event_type,
    character(1)
  )

  effects_df
}

#' @keywords internal
simulation_model_matrix <- function(formula_string, df) {
  formula <- stats::as.formula(paste("~", formula_string))
  model_frame <- stats::model.frame(formula, data = df)
  stats::model.matrix(stats::terms(model_frame), model_frame)
}

#' @keywords internal
make_simulation_effect_names <- function(
  parameter,
  design_columns,
  df,
  covariates
) {
  vapply(
    design_columns,
    function(column) {
      if (column == "(Intercept)") {
        return(paste0("int_", parameter))
      }

      normalized_column <- normalize_design_column_name(column, covariates)
      prefix <- if (column %in% covariates && is.numeric(df[[column]])) {
        "slo_"
      } else {
        "eff_"
      }
      paste0(prefix, parameter, "_", normalized_column)
    },
    character(1)
  )
}

#' @keywords internal
normalize_design_column_name <- function(column, covariates) {
  parts <- strsplit(column, ":", fixed = TRUE)[[1]]
  parts <- vapply(
    parts,
    function(part) {
      matches <- covariates[startsWith(part, covariates)]
      if (length(matches) == 0) {
        return(part)
      }

      covariate <- matches[which.max(nchar(matches))]
      suffix <- substr(part, nchar(covariate) + 1, nchar(part))
      if (nzchar(suffix)) {
        paste0(covariate, "_", suffix)
      } else {
        covariate
      }
    },
    character(1)
  )

  paste(parts, collapse = ":")
}

#' @keywords internal
normalize_parameter_effects <- function(
  parameter,
  parameter_effects,
  design_columns,
  effect_names,
  covariates
) {
  if (is.atomic(parameter_effects) && is.numeric(parameter_effects)) {
    return(normalize_atomic_parameter_effects(
      parameter = parameter,
      parameter_effects = parameter_effects,
      design_columns = design_columns,
      effect_names = effect_names
    ))
  }

  if (!is.list(parameter_effects) || is.data.frame(parameter_effects)) {
    stop(
      "`effects$",
      parameter,
      "` must be a numeric vector or a list.",
      call. = FALSE
    )
  }

  normalize_list_parameter_effects(
    parameter = parameter,
    parameter_effects = parameter_effects,
    design_columns = design_columns,
    effect_names = effect_names,
    covariates = covariates
  )
}

#' @keywords internal
normalize_atomic_parameter_effects <- function(
  parameter,
  parameter_effects,
  design_columns,
  effect_names
) {
  effect_values <- as.numeric(parameter_effects)
  names(effect_values) <- names(parameter_effects)

  if (
    !is.null(names(effect_values)) &&
      all(effect_names %in% names(effect_values))
  ) {
    return(unname(effect_values[effect_names]))
  }

  if (
    !is.null(names(effect_values)) &&
      all(design_columns %in% names(effect_values))
  ) {
    return(unname(effect_values[design_columns]))
  }

  if (length(effect_values) == length(design_columns)) {
    return(unname(effect_values))
  }

  stop(
    "`effects$",
    parameter,
    "` must have ",
    length(design_columns),
    " value",
    if (length(design_columns) == 1) "" else "s",
    ".",
    call. = FALSE
  )
}

#' @keywords internal
normalize_list_parameter_effects <- function(
  parameter,
  parameter_effects,
  design_columns,
  effect_names,
  covariates
) {
  list_names <- names(parameter_effects)
  if (is.null(list_names)) {
    list_names <- rep("", length(parameter_effects))
  }

  direct_values <- extract_direct_simulation_effects(
    parameter_effects = parameter_effects,
    list_names = list_names,
    covariates = covariates
  )

  intercept <- extract_simulation_intercept(
    parameter_effects = parameter_effects,
    list_names = list_names,
    covariates = covariates
  )

  has_explicit_intercept <- any(list_names %in% simulation_intercept_names())
  use_first_value_as_intercept <- is.null(intercept) &&
    "(Intercept)" %in% design_columns &&
    any(list_names %in% covariates)

  if (use_first_value_as_intercept) {
    intercept <- infer_intercept_from_covariate_effects(
      parameter = parameter,
      parameter_effects = parameter_effects,
      list_names = list_names,
      covariates = covariates
    )
  }

  estimates <- rep(NA_real_, length(design_columns))

  for (i in seq_along(design_columns)) {
    column <- design_columns[[i]]
    effect_name <- effect_names[[i]]

    if (column == "(Intercept)") {
      if (is.null(intercept)) {
        stop("Missing intercept for `effects$", parameter, "`.", call. = FALSE)
      }
      estimates[[i]] <- intercept
      next
    }

    if (column %in% names(direct_values)) {
      estimates[[i]] <- direct_values[[column]]
      next
    }

    if (effect_name %in% names(direct_values)) {
      estimates[[i]] <- direct_values[[effect_name]]
      next
    }

    covariate <- design_column_covariate(column, covariates)
    if (!is.null(covariate) && covariate %in% list_names) {
      covariate_columns <- vapply(
        design_columns,
        function(x) {
          identical(design_column_covariate(x, covariates), covariate)
        },
        logical(1)
      )
      position <- match(column, design_columns[covariate_columns])
      covariate_effects <- as.numeric(parameter_effects[[covariate]])
      if (!has_explicit_intercept && length(covariate_effects) > 0) {
        covariate_effects <- covariate_effects[-1]
      }

      if (length(covariate_effects) != sum(covariate_columns)) {
        stop(
          "`effects$",
          parameter,
          "$",
          covariate,
          "` must contain ",
          sum(covariate_columns) + ifelse(has_explicit_intercept, 0, 1),
          " value",
          if (sum(covariate_columns) == 1) "" else "s",
          ".",
          call. = FALSE
        )
      }

      estimates[[i]] <- covariate_effects[[position]]
      next
    }

    stop(
      "Missing effect for design column `",
      column,
      "` in `effects$",
      parameter,
      "`.",
      call. = FALSE
    )
  }

  estimates
}

#' @keywords internal
simulation_intercept_names <- function() {
  c("intercept", "(Intercept)", "int")
}

#' @keywords internal
extract_direct_simulation_effects <- function(
  parameter_effects,
  list_names,
  covariates
) {
  direct <- list()
  reserved_names <- c(covariates, simulation_intercept_names(), "")
  direct_indices <- which(!(list_names %in% reserved_names))

  for (i in direct_indices) {
    value <- as.numeric(parameter_effects[[i]])
    if (length(value) != 1) {
      stop(
        "Direct effect `",
        list_names[[i]],
        "` must be a scalar.",
        call. = FALSE
      )
    }
    direct[[list_names[[i]]]] <- value
  }

  direct
}

#' @keywords internal
extract_simulation_intercept <- function(
  parameter_effects,
  list_names,
  covariates
) {
  intercept_indices <- which(list_names %in% simulation_intercept_names())
  if (length(intercept_indices) > 0) {
    value <- as.numeric(parameter_effects[[intercept_indices[[1]]]])
    if (length(value) != 1) {
      stop("Explicit intercept must be a scalar.", call. = FALSE)
    }
    return(value)
  }

  unnamed_indices <- which(list_names == "")
  if (length(unnamed_indices) == 1 && length(parameter_effects) == 1) {
    value <- as.numeric(parameter_effects[[unnamed_indices]])
    if (length(value) == 1) {
      return(value)
    }
  }

  NULL
}

#' @keywords internal
infer_intercept_from_covariate_effects <- function(
  parameter,
  parameter_effects,
  list_names,
  covariates
) {
  covariate_names <- intersect(list_names, covariates)
  intercepts <- vapply(
    covariate_names,
    function(covariate) {
      values <- as.numeric(parameter_effects[[covariate]])
      if (length(values) < 2) {
        stop(
          "`effects$",
          parameter,
          "$",
          covariate,
          "` must include an intercept followed by at least one effect.",
          call. = FALSE
        )
      }
      values[[1]]
    },
    numeric(1)
  )

  if (length(unique(intercepts)) != 1) {
    stop(
      "The first value of each covariate vector in `effects$",
      parameter,
      "` must be the same shared intercept.",
      call. = FALSE
    )
  }

  unname(intercepts[[1]])
}

#' @keywords internal
design_column_covariate <- function(column, covariates) {
  if (grepl(":", column, fixed = TRUE)) {
    return(NULL)
  }

  matches <- covariates[startsWith(column, covariates)]
  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[which.max(nchar(matches))]]
}

#' @keywords internal
build_simulation_lifelihood_data <- function(
  df,
  covariates,
  sex,
  model_specs,
  right_censoring_date
) {
  placeholders <- add_simulation_life_history_placeholders(
    df = df,
    right_censoring_date = right_censoring_date
  )

  lifelihoodData <- list(
    df = placeholders$df,
    sex = sex,
    sex_start = placeholders$columns[["sex_start"]],
    sex_end = placeholders$columns[["sex_end"]],
    maturity_start = placeholders$columns[["maturity_start"]],
    maturity_end = placeholders$columns[["maturity_end"]],
    clutchs = placeholders$columns[c(
      "clutch_start1",
      "clutch_end1",
      "clutch_size1"
    )],
    death_start = placeholders$columns[["death_start"]],
    death_end = placeholders$columns[["death_end"]],
    model_specs = model_specs,
    covariates = covariates,
    block = NULL,
    matclutch = FALSE,
    matclutch_size = NULL,
    right_censoring_date = right_censoring_date,
    critical_age = 20,
    ratiomax = 10
  )
  class(lifelihoodData) <- "lifelihoodData"
  lifelihoodData
}
