#' @title Individual life history modelling
#' @description Computes the joined likelihood of all the events in an individual life-history (time of maturity, reproductive events, death) and estimates the parameters of the model using maximum likelihood.
#' @name lifelihood
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
#' @return `lifelihoodResults` object
#' @export
#' @examples
#' df <- read.csv(here::here("data/fake_sample.csv"))
#' head(df)
#' df$type <- as.factor(df$type)
#' df$geno <- as.factor(df$geno)
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
#'   path_config = here::here("config.yaml"),
#'   seeds = c(1, 2, 3, 4),
#'   raise_estimation_warning = FALSE
#' )
#' summary(results)
lifelihood <- function(
  lifelihoodData,
  path_config,
  path_to_Lifelihood=NULL,
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

  # ajouter check code dans testing_things_out

  set.seed(sum(seeds))
  run_id <- paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  temp_dir <- file.path(
    here::here(),
    paste0(paste0("lifelihood_", paste(seeds, collapse = "_"), "_id=", run_id))
  )
  dir.create(temp_dir)

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

  filename_output <- sub("\\.txt$", "", basename(data_path))
  output_path <- file.path(temp_dir, paste0(filename_output, ".out"))

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
    check_valid_estimation(lifelihoodResults = results)
  }

  return(results)
}

#' @title Coefficient estimates
#' @name coef
#' @description S3 method to retrieve coefficients from the output of [lifelihood()]
#' @param parameter_name Name of the parameter. All parameters can be found [here](/articles/2-setting-up-the-configuration-file.html#parameters).
#' @inheritParams summary
#' @return A list of coefficient estimates
#' @export
#' @examples
#' df <- read.csv(here::here("data/fake_sample.csv"))
#' df$type <- as.factor(df$type)
#' df$geno <- as.factor(df$geno)
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
#'   path_config = here::here("config.yaml"),
#'   seeds = c(1, 2, 3, 4),
#'   raise_estimation_warning = FALSE
#' )
#' coef(results, "expt_death")
coef.lifelihoodResults <- function(object, parameter_name) {
  effects <- object$effects
  parameter_data <- which(effects$parameter == parameter_name)
  range <- parameter_data[1]:parameter_data[length(parameter_data)]
  coefs <- effects$estimation[range]
  names(coefs) <- effects$name[range]
  return(coefs)
}

#' @title Likelihood
#' @name logLik
#' @description S3 method to retrieve likelihood from the output of [lifelihood()]
#' @inheritParams summary
#' @return A number with the value of maximum likelihood found.
#' @export
#' @examples
#' df <- read.csv(here::here("data/fake_sample.csv"))
#' df$type <- as.factor(df$type)
#' df$geno <- as.factor(df$geno)
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
#'   path_config = here::here("config.yaml"),
#'   seeds = c(1, 2, 3, 4),
#'   raise_estimation_warning = FALSE
#' )
#' logLik(results)
logLik.lifelihoodResults <- function(object, ...) {
  return(object$likelihood)
}

#' @title AIC
#' @name AIC
#' @description S3 method to compute AIC (Akaike information criterion).
#' @inheritParams summary
#' @inheritParams coef
#' @return The AIC.
#' @seealso \code{\link{BIC}}
#' @export
AIC.lifelihoodResults <- function(object, parameter_name) {
  k <- length(coef(object, parameter_name)) + 1
  L <- object$likelihood
  AIC <- 2 * k - 2 * L
  return(AIC)
}

#' @title BIC
#' @name BIC
#' @description S3 method to compute BIC (Akaike information criterion).
#' @inheritParams summary
#' @inheritParams coef
#' @return The BIC.
#' @seealso \code{\link{AIC}}
#' @export
BIC.lifelihoodResults <- function(object, parameter_name) {
  k <- length(coef(object, parameter_name)) + 1
  L <- object$likelihood
  n <- object$sample_size
  BIC <- k * log(n) - 2 * L
  return(BIC)
}

#' @title Summary function to be used with the output of [lifelihood()]
#' @name summary
#' @description S3 method to display main results of the lifelihood program.
#' @param object `lifelihoodResults` object from [lifelihood()]
#' @return NULL
#' @export
#' @examples
#' df <- read.csv(here::here("data/fake_sample.csv"))
#' df$type <- as.factor(df$type)
#' df$geno <- as.factor(df$geno)
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
#'   path_config = here::here("config.yaml"),
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
