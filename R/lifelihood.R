#' @title Individual life history modelling
#' @description Main function of the lifelihood program. Provides the joined likelihood of all the events in an individual life-history (time of maturity, reproductive events, death).
#' @name lifelihood
#' @param df Dataframe with the data of life history. It should have one row per life history / observation.
#' @param path_config Path to the configuration file (YAML format).
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param clutchs Vector containing the names of the clutch columns. The order should be: first clutch first date, first clutch second date, first clutch clutch size, second clutch first date, first clutch second date, second clutch clutch size, and so on. If the observation with the most clutches is, for example, 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param model_specs Vector of characters with the name of the statistical law to use. Must be of length 3 and each element must be in "wei", "exp", "gam" or "lgn". The first one is used for maturity, the second one is used for clutchs and the third one for death.
#' @param covariates Vector containing the names of the covariates.
#' @param matclutch Whether the maturity event (designated by `maturity_start` and `maturity_end`) is a clutch event or not. If `TRUE`, must specify the `matclutch_size` argument.
#' @param matclutch_size Column name containing the size of the clutch for the maturity event. Only used (and required) if `matclutch` is `TRUE`.
#' @param param_range_df Dataframe with the parameter ranges/boundaries/boundaries
#' @param group_by_group Option to fit the full factorail model with all the interactions between each of the factors
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
#' @param right_censoring_date Time (integer) point at which a subject’s data is censored. This means that for subjects who do not experience the event of interest (e.g., death, failure) by this date, their data is considered censored. In practice, choose a value much larger than the maximum longevity seen in the data. (CURRENTLY IGNORED)
#' @param critical_age Critical age (integer) below which life histories are not followed individually. (CURRENTLY IGNORED)
#' @param ratiomax Maximum ratio (integer) between number of offspring of last and first reproduction events. Cannot be greater than ratiomax. (CURRENTLY IGNORED)
#' @param delete_temp_files Indicates whether temporary files should be deleted. TRUE by default and recommended.
#' @export
lifelihood <- function(
    df,
    path_config,
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
    param_range_df = NULL,
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
    right_censoring_date = 1000,
    critical_age = 20,
    ratiomax = 10,
    delete_temp_files = TRUE
) {
   
   # ensure `model_specs` has the right format and values
   valid_model_specs <- c("wei", "gam", "lgn", "exp")
   if (length(model_specs) != 3 || !all(model_specs %in% valid_model_specs)) {
      stop("'model_specs' must be a character vector of length 3 containing only 'wei', 'exp', 'gam', or 'lgn'")
   }

   # ensure that `matclutch_size` is defined when `matclutch` is `TRUE`
   if (isTRUE(matclutch) & is.null(matclutch_size)) {
      stop("`matclutch_size` argument cannot be NULL when `matclutch` is TRUE.")
   }

   # generate seeds
   if (is.null(seeds)) {
      seeds <- sample(1:100, 4, replace = T)
   }

   # Create temporary directory
   temp_dir <- file.path(getwd(), paste0("lifelihood_", paste(seeds, collapse = "_")))
   dir.create(temp_dir)

   # if param_range_df is NULL, use default values
   if (is.null(param_range_df)) {
      max_death <- max(df[death_end], na.rm = TRUE) * 2
      max_maturity <- max(df[maturity_end], na.rm = TRUE) * 2
      max_clutch <- max(suppressWarnings(as.numeric(trimws(unlist(df[clutchs])))), na.rm = TRUE) * 2

      param_range_df <- create_default_boundaries(
         model_specs = model_specs,
         max_death = max_death,
         max_maturity = max_maturity,
         max_clutch = max_clutch
      )
   }

   # create parameters range file
   param_range_path <- file.path(temp_dir, "temp_param_range_path.txt")
   write.table(
      param_range_df,
      file = param_range_path,
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
   )
   path_param_range <- param_range_path

   # create data file
   data_path <- format_dataframe_to_txt(
      df = df,
      sex = sex,
      sex_start = sex_start,
      sex_end = sex_end,
      maturity_start = maturity_start,
      maturity_end = maturity_end,
      clutchs = clutchs,
      death_start = death_start,
      death_end = death_end,
      covariates = covariates,
      matclutch = matclutch,
      model_specs = model_specs,
      path_config = path_config,
      temp_dir = temp_dir
   )

   # create output file
   group_by_group_int <- as.integer(group_by_group)
   execute_bin(
      data_path, path_param_range, group_by_group_int, MCMC, interval, SEcal, saveprobevent,
      fitness, r, seeds[1], seeds[2], seeds[3], seeds[4], ntr, nst, To, Tf, climbrate, precision
   )

   # get path to output file
   filename_output <- sub("\\.txt$", "", basename(data_path))
   output_path <- file.path(temp_dir, paste0(filename_output, ".out"))

   # read output file
   results <- read_output_from_file(output_path, group_by_group = group_by_group)

   # delete intermediate files and temp directory after execution
   if (delete_temp_files) {
      unlink(temp_dir, recursive = TRUE)
   }
   else {
      print(paste("Intermediate files are stored at:", temp_dir))
   }

   # check if estimation are too close from boundaries
   check_valid_estimation(results_lifelihood = results)

   # give output to user
   return(results)
}

#' @name summary
#' @title Custom summary function to be used with the output of [lifelihood::lifelihood()]
#' @description Display main results of the lifelihood program:
#' - seeds
#' - likelihood
#' - effects (estimation)
#' - parameter ranges
#' @param object `LifelihoodResults` object from [lifelihood::lifelihood()]
#' @return NULL
#' @export
summary.LifelihoodResults <- function(object, ...) {
   cat("LIFELIHOODIZATION\n\n")

   is_gbg <- object$group_by_group

   cat("likelihood:\n")
   print(object$likelihood)
   cat("\n")

   cat("effects:\n")
   print(object$effects)
   cat("\n")
}