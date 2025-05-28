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
#' @param param_bounds_df Dataframe with the parameter ranges/boundaries/boundaries
#' @param group_by_group Boolean option to fit the full factorail model with all the interactions between each of the factors
#' @param MCMC Perform MCMC sampling of the parameter after convergence to estimate their 95% confidence interval
#' @param interval TBD - Check the actual meaning
#' @param SEcal If TRUE, Lifelihood computes the standard error of each parameter using the Hessian matrix (output with value of -1 if standard error cannot be computed due to singularity of the Hessian matrix)
#' @param saveprobevent TBD - Check the actual meaning
#' @param fitness Reparametrize the model with one parameter as the lifetime reproductive success
#' @param r Reparametrize the model with one parameter as the intrinsic rate of increase
#' @param seeds Numbers used to reproduce results (same seeds = same results). This must be a vector of length 4.
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
  param_bounds_df = NULL,
  group_by_group = FALSE,
  MCMC = 0,
  interval = 25,
  SEcal = FALSE,
  saveprobevent = 0,
  fitness = 0,
  r = 0,
  seeds = NULL,
  ntr = 2,
  nst = 2,
  To = 50,
  Tf = 1,
  climbrate = 1,
  precision = 0.001,
  raise_estimation_warning = TRUE,
  delete_temp_files = TRUE
) {
  if ((length(seeds) != 4) & !is.null(seeds)) {
    stop("`seeds` must be an integer vector of length 4.")
  }
  if (is.null(seeds)) {
    seeds <- sample(1:10000, 4, replace = T)
  }

  set.seed(sum(seeds))
  temp_dir <- file.path(
    here::here(),
    paste0(paste0("lifelihood_", paste(seeds, collapse = "_")))
  )
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

  data_path <- format_dataframe_to_txt(
    df = lifelihoodData$df,
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

  group_by_group_int <- as.integer(group_by_group)
  execute_bin(
    path_to_Lifelihood = path_to_Lifelihood,
    path_input_data = data_path,
    path_param_range,
    group_by_group_int,
    MCMC,
    interval,
    SEcal,
    saveprobevent,
    fitness,
    r,
    seeds[1],
    seeds[2],
    seeds[3],
    seeds[4],
    ntr,
    nst,
    To,
    Tf,
    climbrate,
    precision
  )

  output_path <- file.path(
    temp_dir,
    paste0(
      sub("\\.txt$", "", basename(data_path)),
      ".out"
    )
  )

  results <- read_output_from_file(
    output_path,
    group_by_group = group_by_group,
    covariates = lifelihoodData$covariates
  )

  results$lifelihoodData <- lifelihoodData
  results$sample_size <- nrow(lifelihoodData$df)
  results$param_bounds_df <- param_bounds_df
  results$config <- yaml::yaml.load_file(path_config, readLines.warn = FALSE)

  if (delete_temp_files) {
    unlink(temp_dir, recursive = TRUE)
  } else {
    print(paste("Intermediate files are stored at:", temp_dir))
  }

  if (raise_estimation_warning) {
    check_valid_estimation(results)
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
#'
#' @return A nested list of coefficient estimates
#'
#' @export
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#' coef(results)
coef.lifelihoodResults <- function(object, ...) {
  check_valid_lifelihoodResults(object)

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
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#'
#' coeff(results, "expt_death")
coeff <- function(object, parameter_name) {
  check_valid_lifelihoodResults(object)

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
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#' logLik(results)
logLik.lifelihoodResults <- function(object, ...) {
  check_valid_lifelihoodResults(object)
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
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#' vcov(results)
vcov.lifelihoodResults <- function(object, ...) {
  check_valid_lifelihoodResults(object)
  return(object$vcov)
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
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#' AIC(results)
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
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#' AICc(results)
AICc.lifelihoodResults <- function(object, ..., k = length(coef(object))) {
  check_valid_lifelihoodResults(object)

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
#'
#' @examples
#' df <- lifelihood::fakesample |>
#'   mutate(
#'     type = as.factor(type),
#'     geno = as.factor(geno)
#'   )
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
#' BIC(results)
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
#'
#' @examples
#' library(lifelihood)
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
summary.lifelihoodResults <- function(object, ...) {
  cat("LIFELIHOOD RESULTS\n\n")

  cat("Likelihood:\n")
  print(object$likelihood)
  cat("\n")

  cat("Effects:\n")
  print(object$effects)
  cat("\n")
}
