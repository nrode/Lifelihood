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
#' @param covariates Character vector with covariate names.
#' @param config Path to a YAML configuration file or an already-loaded
#'   configuration list.
#' @param model Character vector with one model family to reuse for mortality,
#'   maturity and reproduction, or three model families in that order. Values
#'   must be `"wei"`, `"exp"`, `"gam"` or `"lgn"`.
#' @param n Number of simulated individuals represented by the internal data.
#' @param sex Scalar or vector of length `n` with sex values. Defaults to `NA`.
#' @param data Optional data frame containing the covariate values to use for
#'   simulation. If supplied, it must have `n` rows and contain all `covariates`.
#' @param param_bounds_df Optional data frame with columns `param`, `min` and
#'   `max`. These bounds are used by [prediction()] to transform effects from
#'   the link scale to the response scale.
#' @param right_censoring_date Numeric right censoring date stored in the
#'   internal `lifelihoodData` object.
#'
#' @return A `lifelihoodResults` object suitable for [simulate_life_history()].
#'
#' @export
create_simulation_input <- function(
  effects,
  covariates,
  config,
  model,
  n,
  sex = NA,
  data = NULL,
  param_bounds_df = NULL,
  right_censoring_date = 1000
) {
  config <- load_simulation_config(config)
  model_specs <- normalize_simulation_model(model)
  n <- validate_simulation_n(n)
  covariates <- validate_simulation_covariates(covariates)
  validate_simulation_effects(effects)

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

  df <- build_simulation_data(
    effects = effects,
    covariates = covariates,
    config = config,
    n = n,
    sex = sex,
    data = data
  )

  if (is.null(param_bounds_df)) {
    param_bounds_df <- default_simulation_bounds_df(
      model_specs = model_specs,
      right_censoring_date = right_censoring_date
    )
  } else {
    param_bounds_df <- normalize_simulation_bounds_df(param_bounds_df)
  }

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
    model_specs = model_specs,
    right_censoring_date = right_censoring_date
  )

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
      "tof_reduction_rate",
      "increase_tof_n_offspring",
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
validate_simulation_n <- function(n) {
  if (length(n) != 1 || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }
  as.integer(n)
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
  effects,
  covariates,
  config,
  n,
  sex,
  data
) {
  if (!is.null(data)) {
    df <- as.data.frame(data)
    if (nrow(df) != n) {
      stop("`data` must have exactly `n` rows.", call. = FALSE)
    }

    missing_covariates <- setdiff(covariates, names(df))
    if (length(missing_covariates) > 0) {
      stop(
        "`data` is missing covariate",
        if (length(missing_covariates) == 1) "" else "s",
        ": ",
        paste(missing_covariates, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    df <- build_inferred_simulation_data(
      effects = effects,
      covariates = covariates,
      config = config,
      n = n
    )
  }

  if ("sex" %in% covariates) {
    stop("`sex` is reserved for the internal sex column.", call. = FALSE)
  }

  df$sex <- normalize_simulation_sex(sex, n)
  rownames(df) <- NULL
  df
}

#' @keywords internal
build_inferred_simulation_data <- function(
  effects,
  covariates,
  config,
  n
) {
  used_covariates <- get_simulation_formula_covariates(config)

  if (length(covariates) == 0) {
    return(data.frame(.row_id = seq_len(n)))
  }

  level_counts <- infer_simulation_level_counts(
    effects = effects,
    covariates = covariates
  )

  missing_levels <- used_covariates[
    is.na(level_counts[used_covariates]) | level_counts[used_covariates] < 2
  ]
  if (length(missing_levels) > 0) {
    stop(
      "Cannot infer factor levels for covariate",
      if (length(missing_levels) == 1) "" else "s",
      ": ",
      paste(missing_levels, collapse = ", "),
      ". Provide `data` or include covariate effects.",
      call. = FALSE
    )
  }

  level_counts[is.na(level_counts)] <- 1L
  values <- lapply(level_counts, function(count) {
    factor(seq_len(count) - 1, levels = seq_len(count) - 1)
  })
  names(values) <- covariates

  combinations <- expand.grid(
    values,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = TRUE
  )
  combinations[rep(seq_len(nrow(combinations)), length.out = n), , drop = FALSE]
}

#' @keywords internal
infer_simulation_level_counts <- function(effects, covariates) {
  level_counts <- rep(NA_integer_, length(covariates))
  names(level_counts) <- covariates

  for (parameter in names(effects)) {
    parameter_effects <- effects[[parameter]]
    if (!is.list(parameter_effects) || is.data.frame(parameter_effects)) {
      next
    }

    effect_names <- names(parameter_effects)
    if (is.null(effect_names)) {
      effect_names <- rep("", length(parameter_effects))
    }

    has_explicit_intercept <- any(
      effect_names %in% simulation_intercept_names()
    )

    for (covariate in intersect(effect_names, covariates)) {
      values <- as.numeric(parameter_effects[[covariate]])
      if (length(values) == 0) {
        next
      }

      level_count <- if (has_explicit_intercept) {
        length(values) + 1L
      } else {
        length(values)
      }

      level_counts[covariate] <- max(
        level_counts[covariate],
        level_count,
        na.rm = TRUE
      )
    }
  }

  level_counts
}

#' @keywords internal
normalize_simulation_sex <- function(sex, n) {
  if (length(sex) == 1) {
    return(rep(sex, n))
  }

  if (length(sex) != n) {
    stop("`sex` must be a scalar or a vector of length `n`.", call. = FALSE)
  }

  sex
}

#' @keywords internal
default_simulation_bounds_df <- function(model_specs, right_censoring_date) {
  model_bounds <- data.frame(
    model = c("wei", "gam", "lgn", "exp"),
    min = c(0.05, 0.005, 0.0025, 0.05),
    max = c(500, 600, 10, 1000),
    stringsAsFactors = FALSE
  )

  shape_bounds <- function(model) {
    row <- model_bounds[model_bounds$model == model, , drop = FALSE]
    c(min = row$min, max = row$max)
  }

  mortality_shape <- shape_bounds(model_specs[[1]])
  maturity_shape <- shape_bounds(model_specs[[2]])
  reproduction_shape <- shape_bounds(model_specs[[3]])

  normalize_simulation_bounds_df(data.frame(
    param = c(
      "expt_death",
      "survival_param2",
      "ratio_expt_death",
      "prob_death",
      "sex_ratio",
      "expt_maturity",
      "maturity_param2",
      "ratio_expt_maturity",
      "expt_reproduction",
      "reproduction_param2",
      "n_offspring",
      "increase_death_hazard",
      "tof_reduction_rate",
      "increase_tof_n_offspring",
      "lin_decrease_hazard",
      "quad_decrease_hazard",
      "lin_change_n_offspring",
      "quad_change_n_offspring",
      "tof_n_offspring",
      "fitness"
    ),
    min = c(
      0.001,
      mortality_shape[["min"]],
      0.01,
      1e-05,
      1e-05,
      0.001,
      maturity_shape[["min"]],
      0.01,
      0.001,
      reproduction_shape[["min"]],
      1,
      1e-05,
      1e-07,
      1e-07,
      -20,
      -10,
      -10,
      -10,
      -10,
      0.001
    ),
    max = c(
      right_censoring_date,
      mortality_shape[["max"]],
      100,
      1 - 1e-05,
      1 - 1e-05,
      right_censoring_date,
      maturity_shape[["max"]],
      100,
      right_censoring_date,
      reproduction_shape[["max"]],
      50,
      10,
      10,
      10,
      20,
      10,
      10,
      10,
      10,
      right_censoring_date
    ),
    stringsAsFactors = FALSE
  ))
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
  model_specs,
  right_censoring_date
) {
  lifelihoodData <- list(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "maturity_start",
    maturity_end = "maturity_end",
    clutchs = c("clutch_start1", "clutch_end1", "clutch_size1"),
    death_start = "death_start",
    death_end = "death_end",
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
