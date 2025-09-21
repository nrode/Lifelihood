#' @title Get dataframe with default parameter boundaries
#'
#' @description
#' Once you have created your `lifelihoodData` object with
#' [lifelihoodData()], you can call the `default_bounds_df()`
#' function to generate (and load) a dataframe with default
#' parameter bounds.
#' This is useful when you want to customise these bounds and
#' then pass this dataframe to the [lifelihood()] function via
#' the `param_bounds_df` argument (if not, it will automatically
#' generate it and keep the default values).
#'
#' @inheritParams lifelihood
#'
#' @return A dataframe with the default parameter boundaries.
#'
#' @export
#'
#' @examples
#' library(lifelihood)
#' library(tidyverse)
#'
#' df <- fakesample |>
#'   mutate(
#'     geno = as.factor(geno),
#'     type = as.factor(type)
#'   )
#' head(df)
#'
#' clutchs <- c(
#'   "clutch_start1", "clutch_end1", "clutch_size1",
#'   "clutch_start2", "clutch_end2", "clutch_size2"
#' )
#'
#' dataLFH <- lifelihoodData(
#'   df = df,
#'   sex = "sex",
#'   sex_start = "sex_start",
#'   sex_end = "sex_end",
#'   maturity_start = "mat_start",
#'   maturity_end = "mat_end",
#'   clutchs = clutchs,
#'   death_start = "death_end",
#'   death_end = "death_end",
#'   covariates = c("geno", "type"),
#'   model_specs = c("gam", "lgn", "wei")
#' )
#'
#' bounds_df <- default_bounds_df(dataLFH)
#' head(bounds_df)
#'
#' # for example, we want to change this value
#' bounds_df[bounds_df$name == "increase_death_hazard", "max"] <- 80
#'
#' # then we pass it to lifelihood()
#' results <- lifelihood(
#'   lifelihoodData = dataLFH,
#'   path_config = get_config_path("config"),
#'   param_bounds_df = bounds_df,
#'   raise_estimation_warning = FALSE
#' )
default_bounds_df <- function(
  lifelihoodData
) {
  if (!inherits(lifelihoodData, "lifelihoodData")) {
    stop("lifelihoodData must be of class lifelihoodData")
  }

  df <- lifelihoodData$df
  death_end <- lifelihoodData$death_end
  maturity_end <- lifelihoodData$maturity_end
  clutchs <- lifelihoodData$clutchs
  model_specs <- lifelihoodData$model_specs
  right_censoring_date <- lifelihoodData$right_censoring_date

  max_death <- max(
    df[df[[death_end]] < right_censoring_date, death_end],
    na.rm = TRUE
  ) *
    2
  max_maturity <- max(
    df[df[[maturity_end]] < right_censoring_date, maturity_end],
    na.rm = TRUE
  ) *
    2
  clutchs_size_cols <- clutchs[seq(3, length(clutchs), 3)]
  max_clutch <- max(
    suppressWarnings(as.numeric(trimws(unlist(df[clutchs_size_cols])))),
    na.rm = TRUE
  ) *
    2

  models_bounds <- data.frame(
    name = c("wei", "gam", "lgn", "exp"),
    min_default = c(0.05, 0.005, 0.0025, 0.05),
    max_default = c(500, 600, 10, 1000)
  )

  maturity_model <- model_specs[1]
  maturity_specs <- subset(models_bounds, name == maturity_model)
  maturity_shape_min <- maturity_specs$min_default
  maturity_shape_max <- maturity_specs$max_default

  clutch_model <- model_specs[2]
  clutch_specs <- subset(models_bounds, name == clutch_model)
  clutch_shape_min <- clutch_specs$min_default
  clutch_shape_max <- clutch_specs$max_default

  death_model <- model_specs[3]
  death_specs <- subset(models_bounds, name == death_model)
  death_shape_min <- death_specs$min_default
  death_shape_max <- death_specs$max_default

  boundaries <- list(
    expt_death = c(name = "expt_death", min = 0.001, max = max_death),
    survival_shape = c(
      name = "survival_shape",
      min = death_shape_min,
      max = death_shape_max
    ),
    ratio_expt_death = c(name = "ratio_expt_death", min = 0.01, max = 100),
    prob_death = c(name = "prob_death", min = 0 + 0.00001, max = 1 - 0.00001),
    sex_ratio = c(name = "sex_ratio", min = 0 + 0.00001, max = 1 - 0.00001),
    expt_maturity = c(name = "expt_maturity", min = 0.001, max = max_maturity),
    maturity_shape = c(
      name = "maturity_shape",
      min = maturity_shape_min,
      max = maturity_shape_max
    ),
    ratio_expt_maturity = c(
      name = "ratio_expt_maturity",
      min = 0.01,
      max = 100
    ),
    expt_reproduction = c(
      name = "expt_reproduction",
      min = 0.001,
      max = max_clutch
    ),
    reproduction_shape = c(
      name = "reproduction_shape",
      min = clutch_shape_min,
      max = clutch_shape_max
    ),
    n_offspring = c(name = "n_offspring", min = 1, max = 50),
    increase_death_hazard = c(
      name = "increase_death_hazard",
      min = 1e-05,
      max = 10
    ),
    tof_reduction_date = c(name = "tof_reduction_date", min = 1e-07, max = 10),
    increase_tof_n_offspring = c(
      name = "increase_tof_n_offspring",
      min = 1e-07,
      max = 10
    ),
    lin_decrease_hazard = c(name = "lin_decrease_hazard", min = -20, max = 20),
    quad_senescence = c(name = "quad_senescence", min = -20, max = 20),
    quad_decrease_hazard = c(
      name = "quad_decrease_hazard",
      min = -10,
      max = 10
    ),
    quad_change_n_offspring = c(
      name = "quad_change_n_offspring",
      min = -10,
      max = 10
    ),
    tof_n_offspring = c(name = "tof_n_offspring", min = -10, max = 10),
    fitness = c(name = "fitness", min = 0.001, max = 1000) # chnager max
  )
  boundaries_df <- data.frame(
    param = sapply(boundaries, function(x) x["name"]),
    min = sapply(boundaries, function(x) x["min"]),
    max = sapply(boundaries, function(x) x["max"]),
    row.names = NULL
  )
  return(boundaries_df)
}
