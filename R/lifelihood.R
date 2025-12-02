#' @title Individual life history modelling
#'
#' @description
#' Computes the joined likelihood of all the events in
#' an individual life-history (time of maturity, reproductive
#' events, death) and estimates the parameters of the model
#' using maximum likelihood.
#'
#' @param lifelihoodData `lifelihoodData` object created with [lifelihoodData()].
#' @param path_config A character string specifying the file path to the YAML configuration file.
#' @param path_to_Lifelihood A character string specifying the file path to the compile Lifelihood program (default is NULL)
#' @param n_fit Number of times to fit.
#' @param param_bounds_df Dataframe with the parameter ranges/boundaries/boundaries
#' @param group_by_group Boolean option to fit the full factorial model with all the interactions between each of the factors
#' @param MCMC Perform MCMC sampling of the parameter after convergence to estimate their 95% confidence interval
#' @param interval TBD - Check the actual meaning
#' @param se If TRUE, Lifelihood computes the standard error of each parameter using the Hessian matrix (output with value of -1 if standard error cannot be computed due to singularity of the Hessian matrix)
#' @param saveprobevent TBD - Check the actual meaning
#' @param r Reparametrize the model with one parameter as the intrinsic rate of increase
#' @param seeds Vector of length for with seed numbers used to reproduce results (same seeds = same results). This argument should be `NULL` (default) when `n_fit` > 1.
#' @param ratiomax Maximum multiplicative factor for clutch size in models with reproductive senescence (cf CalculRatioEspPoissonTronque function in Lifelihood)
#' @param tc critical age (after the juvenile mortality peak) at which the survival model starts to be fitted
#' @param tinf maximum censoring time (should be greater than the age of the oldest individual observed dead in the dataset)
#' @param sub_interval sub-interval used to integrate the left and right censoring dates of each event
#' @param ntr Number of thread for the paralelisation ?
#' @param nst TBD - Check the actual meaning
#' @param To Initial temperature for the simulated annealing
#' @param Tf Initial temperature for the simulated annealing
#' @param climbrate Rate for the simulated annealing ?
#' @param precision TBD - Check the actual meaning
#' @param raise_estimation_warning Whether or not to raise a warning when the estimate of a parameter is too close to its minimum or maximum bound. Default is TRUE.
#' @param delete_temp_files Indicates whether temporary files should be deleted. TRUE by default and recommended.
#'
#' @return A `lifelihoodResults` object
#'
#' @importFrom utils write.table
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
#'   death_start = "death_start",
#'   death_end = "death_end",
#'   covariates = c("geno", "type"),
#'   model_specs = c("gam", "lgn", "wei")
#' )
#'
#' results <- lifelihood(
#'   lifelihoodData = dataLFH,
#'   path_config = get_config_path("config"),
#'   seeds = c(1, 2, 3, 4),
#'   raise_estimation_warning = FALSE
#' )
#' summary(results)
lifelihood <- function(
  lifelihoodData,
  path_config,
  path_to_Lifelihood = NULL,
  n_fit = 1,
  param_bounds_df = NULL,
  group_by_group = FALSE,
  MCMC = 0,
  interval = 25,
  se = FALSE,
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
  raise_estimation_warning = TRUE,
  delete_temp_files = TRUE,
  temp_dir = NULL
) {
  check_lifelihoodData(lifelihoodData)

  # we force generate seeds here because it would not make sense
  # to use n times the same seeds.
  if (!is.null(seeds) & n_fit > 1) {
    stop("Can't set `seeds` with `n_fit` > 1.")
  }

  all_results <- list()
  for (i in 1:n_fit) {
    if (n_fit != 1 & is.null(seeds)) {
      seeds <- sample(1:10000, 4, replace = T)
    }

    temp_dir <- file.path(
      here::here(),
      paste0(paste0("lifelihood_", paste(seeds, collapse = "_")))
    )

    results <- lifelihood_fit(
      lifelihoodData = lifelihoodData,
      path_config = path_config,
      path_to_Lifelihood = path_to_Lifelihood,
      param_bounds_df = param_bounds_df,
      group_by_group = group_by_group,
      MCMC = MCMC,
      interval = interval,
      se = se,
      saveprobevent = saveprobevent,
      r = r,
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
      raise_estimation_warning = raise_estimation_warning,
      delete_temp_files = delete_temp_files,
      seeds = seeds,
      temp_dir = temp_dir
    )
    all_results[[glue::glue("lifelihood_fit_{i}")]] <- results
  }

  likelihoods <- sapply(all_results, function(x) x$likelihood)
  ord <- order(likelihoods, decreasing = TRUE)

  best_fit <- all_results[[ord[1]]]

  # we want to make sure that we couldn't easily find a better
  # solution (https://github.com/nrode/Lifelihood/issues/111)
  if (length(likelihoods) > 1) {
    diff_best <- likelihoods[ord[1]] - likelihoods[ord[2]]
    if (diff_best > 0.1) {
      warning(glue::glue(
        "Best and second-best likelihoods differ by {round(diff_best, 3)} (> 0.1). ",
        "Consider increasing n_fit (currently {n_fit}) for more stable optimization."
      ))
    }
  }

  return(best_fit)
}

#' @title Fit lifelihood one time.
#'
#' @description
#' This function fits [`lifelihood()`] 1 time. It's used
#' internally to fit multiple times lifelihood.
#'
#' @inheritParams lifelihood
#'
#' @import glue
#'
#' @keywords internal
#'
#' @return `lifelihoodResults`
lifelihood_fit <- function(
  lifelihoodData,
  path_config,
  path_to_Lifelihood = NULL,
  param_bounds_df = NULL,
  group_by_group = FALSE,
  MCMC = 0,
  interval = 25,
  se = FALSE,
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
  raise_estimation_warning = TRUE,
  delete_temp_files = TRUE,
  temp_dir = NULL
) {
  if ((length(seeds) != 4) & !is.null(seeds)) {
    stop("`seeds` must be an integer vector of length 4.")
  }
  if (is.null(seeds)) {
    seeds <- sample(1:10000, 4, replace = T)
  }

  set.seed(sum(seeds))
  if (is.null(temp_dir)) {
    temp_dir <- file.path(
      here::here(),
      paste0(paste0("lifelihood_", paste(seeds, collapse = "_")))
    )
  }
  dir.create(temp_dir, showWarnings = FALSE)

  if (is.null(param_bounds_df)) {
    param_bounds_df <- default_bounds_df(lifelihoodData)
  }

  path_param_range <- file.path(temp_dir, "temp_param_range_path.txt")
  write.table(
    param_bounds_df,
    file = path_param_range,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )

  # encode covariates to integers
  df <- lifelihoodData$df
  translator <- create_translator(df, cols = lifelihoodData$covariates)
  df_encoded <- encode(translator, df)

  # check is any numeric variable
  numeric_vars <- df |>
    select(any_of(lifelihoodData$covariates)) |>
    select(where(is.numeric))
  if (ncol(numeric_vars) > 0) {
    numeric_vec <- c()
    for (col in colnames(df |> select(lifelihoodData$covariates))) {
      unique_values <- df |>
        dplyr::pull(col) |>
        unique() |>
        sort() |>
        paste0(collapse = " ")
      numeric_vec <- c(numeric_vec, unique_values)
    }
    path_continuous_var <- file.path(temp_dir, "temp_continuous_variables.txt")
    writeLines(numeric_vec, con = path_continuous_var)
  } else {
    path_continuous_var = "NULL"
  }

  ## Convert numeric variables so that they start at 0
  convertTonum <- function(x) {
    as.numeric(as.factor(x)) - 1
  }

  df_encoded <- df_encoded |>
    mutate(across(all_of(colnames(numeric_vars)), convertTonum))

  data_path <- format_dataframe_to_txt(
    df = df_encoded,
    sex = lifelihoodData$sex,
    sex_start = lifelihoodData$sex_start,
    sex_end = lifelihoodData$sex_end,
    maturity_start = lifelihoodData$maturity_start,
    maturity_end = lifelihoodData$maturity_end,
    clutchs = lifelihoodData$clutchs,
    death_start = lifelihoodData$death_start,
    death_end = lifelihoodData$death_end,
    covariates = lifelihoodData$covariates,
    matclutch = lifelihoodData$matclutch,
    model_specs = lifelihoodData$model_specs,
    path_config = path_config,
    temp_dir = temp_dir
  )

  # we deduce fitness from the configuration file
  config_yaml <- yaml::yaml.load_file(path_config, readLines.warn = FALSE)
  if (read_formula(config_yaml, "fitness") != "not_fitted") {
    if (read_formula(config_yaml, "n_offspring") != "not_fitted") {
      stop(
        "Model in configuration file (",
        path_config,
        ") is not identifiable: you should either fit 'fitness' or 'n_offpsring' in your model, not both."
      )
    }
    fitness <- TRUE
  } else {
    fitness <- FALSE
  }

  execute_bin(
    path_to_Lifelihood = path_to_Lifelihood,
    path_input_data = data_path,
    path_param_bounds = path_param_range,
    group_by_group = group_by_group,
    MCMC = MCMC,
    interval = interval,
    se = se,
    saveprobevent = saveprobevent,
    fitness = fitness,
    r = r,
    seed1 = seeds[1],
    seed2 = seeds[2],
    seed3 = seeds[3],
    seed4 = seeds[4],
    ratiomax = ratiomax,
    tc = tc,
    tinf = tinf,
    sub_interval = sub_interval,
    path_continuous_var = path_continuous_var,
    ntr = ntr,
    nst = nst,
    To = To,
    Tf = Tf,
    climbrate = climbrate,
    precision = precision
  )

  output_path <- file.path(
    temp_dir,
    paste0(
      sub("\\.txt$", "", basename(data_path)),
      ".out"
    )
  )

  # decode the encoded factor levels in output file
  output_path <- decode_file_with_translator(output_path, translator)

  results <- read_output_from_file(
    output_path,
    group_by_group = group_by_group,
    covariates = lifelihoodData$covariates,
    path_config = path_config,
    MCMC = MCMC
  )

  results$lifelihoodData <- lifelihoodData
  results$sample_size <- nrow(lifelihoodData$df)
  results$param_bounds_df <- param_bounds_df

  if (delete_temp_files) {
    unlink(temp_dir, recursive = TRUE)
  } else {
    print(paste("Intermediate files are stored at:", temp_dir))
  }

  if (raise_estimation_warning) {
    check_estimation(results)
  }

  return(results)
}


#' @title Coefficient estimates
#'
#' @name coef
#'
#' @description
#' `coef()` retrieve all coefficients from the output of [lifelihood()]
#'
#' @param object output of [lifelihood()]
#' @param ... Ignored
#'
#' @return A nested list of coefficient estimates
#'
#' @export
#'
#' @importFrom stats coef
coef.lifelihoodResults <- function(object, ...) {
  check_lifelihoodResults(object)

  coefs <- object$effects$estimation
  names(coefs) <- object$effects$name

  return(coefs)
}

#' @title Coefficient estimates
#'
#' @name coef
#'
#' @description
#' `coeff()` retrieve coefficients of one parameter
#' from the output of [lifelihood()]
#'
#' @param object output of [lifelihood()]
#' @param parameter_name
#' Name of the parameters to extract the estimate from to extract
#' all parameter estimates). All parameters#' can be found
#' [here](/articles/setting-up-the-configuration-file.html#parameters).
#'
#' @return A list of coefficient estimates
#'
#' @export
coeff <- function(object, parameter_name) {
  check_lifelihoodResults(object)

  effects <- object$effects
  parameter_data <- which(effects$parameter == parameter_name)
  range <- parameter_data[1]:parameter_data[length(parameter_data)]
  coefs <- effects$estimation[range]
  names(coefs) <- effects$name[range]

  return(coefs)
}

#' @title Likelihood
#'
#' @description
#' S3 method to retrieve likelihood from the output of [lifelihood()]
#'
#' @param object output of [lifelihood()]
#' @param ... Ignored
#'
#' @return A number with the value of maximum likelihood found.
#'
#' @export
logLik.lifelihoodResults <- function(object, ...) {
  check_lifelihoodResults(object)
  return(object$likelihood)
}

#' @title Covariance matrix
#'
#' @description
#' S3 method to retrieve the covariance matrix
#' from the output of [lifelihood()]
#'
#' @param object output of [lifelihood()]
#' @param ... Ignored
#'
#' @return A covariance matrix
#'
#' @export
vcov.lifelihoodResults <- function(object, ...) {
  check_lifelihoodResults(object)
  return(object$vcov)
}

#' @title MCMC
#'
#' @description
#' Retrieve the mcmc from the output of [lifelihood()]
#'
#' @param object output of [lifelihood()]
#'
#' @return A covariance matrix
#'
#' @export
mcmc <- function(object) {
  check_lifelihoodResults(object)
  return(object$mcmc)
}

#' @title Akaike Information Criterion
#'
#' @description
#' S3 method to compute AIC (Akaike Information Criterion).
#'
#' @param object output of [lifelihood()]
#' @param ... Ignored
#' @param k Number of estimated parameter of the modèle. Default to `length(coef(object))`.
#'
#' @return The AIC
#'
#' @seealso [AICc()], [BIC()]
#'
#' @export
AIC.lifelihoodResults <- function(object, ..., k = length(coef(object))) {
  L <- object$likelihood
  AIC <- -2 * L + 2 * k
  return(AIC)
}

#' @title Akaike Information Criterion for small sample size
#'
#' @description
#' S3 method to compute AICc (Akaike Information Criterion
#' corrected for small sample size, see Hurvich and Tsai 1989).
#'
#' @inheritParams AIC.lifelihoodResults
#'
#' @return The AICc
#'
#' @seealso [AIC()], [BIC()]
#'
#' @export
AICc <- function(object, ..., k = length(coef(object))) {
  check_lifelihoodResults(object)

  L <- object$likelihood
  n <- object$sample_size
  AICc <- -2 * L + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
  return(AICc)
}

#' @title Bayesian Information Criterion
#'
#' @description
#' S3 method to compute BIC (Bayesian Information Criterion).
#'
#' @param object output of [lifelihood()]
#' @param ... Ignored
#'
#' @return The BIC
#'
#' @seealso [AIC()], [AICc()]
#'
#' @importFrom stats BIC
#'
#' @export
BIC.lifelihoodResults <- function(object, ...) {
  k <- length(coef(object))
  L <- object$likelihood
  n <- object$sample_size
  BIC <- k * log(n) - 2 * L
  return(BIC)
}

#' @title Summary function to be used with the output of [lifelihood()]
#'
#' @description
#' S3 method to display main results of the lifelihood program.
#'
#' @param object output of [lifelihood()]
#' @param ... Ignored
#'
#' @export
summary.lifelihoodResults <- function(object, ...) {
  cat("LIFELIHOOD RESULTS\n\n")

  cat("Likelihood:\n")
  print(object$likelihood)
  cat("\n")

  cat("Effects:\n")
  print(object$effects)
  cat("\n")
}
