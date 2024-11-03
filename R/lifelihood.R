#' @title Data object for lifelihood
#' @description Creates a `lifelihoodData` object, which is a list containing all the information needed to run the lifelihood program of a given dataset of individual life history.
#' @name lifelihoodData
#' @param df Dataframe with the data of life history. It should have one row per life history / observation.
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param clutchs Vector containing the names of the clutch columns. The order should be: first clutch first date, first clutch second date, first clutch clutch size, second clutch first date, first clutch second date, second clutch clutch size, and so on. If the observation with the most clutches is, for example, 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param model_specs Vector of characters with the name of the statistical law to use. Must be of length 3 and each element must be in "wei" (Weibull law), "exp" (Exponential law), "gam" (Gamma law) or "lgn" (Log-normal law). The first one is used for maturity, the second one is used for clutchs and the third one for death.
#' @param covariates Vector containing the names of the covariates.
#' @param matclutch Whether the maturity event (designated by `maturity_start` and `maturity_end`) is a clutch event or not. If `TRUE`, must specify the `matclutch_size` argument. Default is `FALSE`.
#' @param matclutch_size Column name containing the size of the clutch for the maturity event. Only used (and required) if `matclutch` is `TRUE`.
#' @param right_censoring_date Time (integer) point at which a subject’s data is censored. This means that for subjects who do not experience the event of interest (e.g., death, failure) by this date, their data is considered censored. In practice, choose a value much larger than the maximum longevity seen in the data. (CURRENTLY IGNORED)
#' @param critical_age Critical age (integer) below which life histories are not followed individually. (CURRENTLY IGNORED)
#' @param ratiomax Maximum ratio (integer) between number of offspring of last and first reproduction events. Cannot be greater than ratiomax. (CURRENTLY IGNORED)
#' @return `lifelihoodData` object
#' @export
lifelihoodData <- function(
   df,
   sex,
   sex_start,
   sex_end,
   maturity_start,
   maturity_end,
   clutchs,
   death_start,
   death_end,
   model_specs,
   covariates,
   matclutch = FALSE,
   matclutch_size = NULL,
   right_censoring_date = 1000,
   critical_age = 20,
   ratiomax = 10
) {

   valid_model_specs <- c("wei", "gam", "lgn", "exp")
   if (length(model_specs) != 3 || !all(model_specs %in% valid_model_specs)) {
      stop("'model_specs' must be a character vector of length 3 containing only 'wei', 'exp', 'gam', or 'lgn'")
   }

   if (isTRUE(matclutch) & is.null(matclutch_size)) {
      stop("`matclutch_size` argument cannot be NULL when `matclutch` is TRUE.")
   }

   dataObject <- list(
      df = df,
      sex = sex,
      sex_start = sex_start,
      sex_end = sex_end,
      maturity_start = maturity_start,
      maturity_end = maturity_end,
      clutchs = clutchs,
      death_start = death_start,
      death_end = death_end,
      model_specs = model_specs,
      covariates = covariates,
      matclutch = matclutch,
      matclutch_size = matclutch_size,
      right_censoring_date = right_censoring_date,
      critical_age = critical_age,
      ratiomax = ratiomax
   )
   class(dataObject) <- "lifelihoodData"
   return(dataObject)
}

#' @name summary
#' @title Custom summary function to be used with the output of [lifelihoodData()]
#' @description Display main information of the `lifelihoodData` object:
#' - number of observations
#' - number of covariates
#' - number of clutches
#' - number of events (maturity, reproductive, death)
#' @param object `lifelihoodData` object from [lifelihoodData()]
#' @return NULL
#' @export
summary.lifelihoodData <- function(object, ...) {
   cat("LIFELIHOOD DATA\n\n")
   cat("Number of observations:", nrow(object$df), "\n")
   cat("Number of covariates:", length(object$covariates), "\n")
   cat("Number of clutches:", length(object$clutchs), "\n")
}


#' @title Individual life history modelling
#' @description Computes the joined likelihood of all the events in an individual life-history (time of maturity, reproductive events, death) and estimates the parameters of the model using maximum likelihood.
#' @name lifelihood
#' @param lifelihoodData `lifelihoodData` object created with [lifelihoodData()].
#' @param path_config A character string specifying the file path to the YAML configuration file.
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
#' @param ... Additional arguments (currently not used)
#' @return `LifelihoodResults` object
#' @export
lifelihood <- function(
   lifelihoodData,
   path_config,
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
   if (is.null(seeds)) {seeds <- sample(1:10000, 4, replace = T)}

   set.seed(sum(seeds))
   run_id <- paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
   temp_dir <- file.path(getwd(), paste0(paste0("lifelihood_", paste(seeds, collapse = "_"), "_id=", run_id)))
   dir.create(temp_dir)

   if (is.null(param_bounds_df)) {param_bounds_df <- default_bounds_df(lifelihoodData)}

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
      data_path, path_param_range, group_by_group_int, MCMC, interval, SEcal, saveprobevent,
      fitness, r, seeds[1], seeds[2], seeds[3], seeds[4], ntr, nst, To, Tf, climbrate, precision
   )

   filename_output <- sub("\\.txt$", "", basename(data_path))
   output_path <- file.path(temp_dir, paste0(filename_output, ".out"))

   results <- read_output_from_file(
      output_path,
      group_by_group = group_by_group,
      covariates = lifelihoodData$covariates
   )

   if (delete_temp_files) {
      unlink(temp_dir, recursive = TRUE)
   }
   else {
      print(paste("Intermediate files are stored at:", temp_dir))
   }

   if (raise_estimation_warning){
      check_valid_estimation(results_lifelihood = results)
   }

   return(results)
}

#' @name summary
#' @title Custom summary function to be used with the output of [lifelihood()]
#' @description Display main results of the lifelihood program:
#' - seeds
#' - likelihood
#' - effects (estimation)
#' - parameter ranges
#' @param object `LifelihoodResults` object from [lifelihood()]
#' @return NULL
#' @export
summary.LifelihoodResults <- function(object, ...) {
   cat("LIFELIHOOD RESULTS\n\n")

   cat("Likelihood:\n")
   print(object$likelihood)
   cat("\n")

   cat("Effects:\n")
   print(object$effects)
   cat("\n")
}