#' @title Execution of the compiled files
#'
#' @keywords internal
#'
#' @description
#' Run lifelihood program in console mode.
#'
#' @param path_to_Lifelihood A character string specifying the file
#' path to the compile Lifelihood program (default is NULL).
#' @param path_input_data Path to the input text file with the model
#' and data to be fitted.
#' @param path_param_bounds Path to the parameter boundaries text file
#' with the min and max boudaries for each parameter.
#' @param seed1 First seed number used to reproduce results (same seed
#' = same results).
#' @param seed2 Second seed number used to reproduce results (same seed
#' = same results).
#' @param seed3 Third seed number used to reproduce results (same seed
#' = same results).
#' @param seed4 Fourth seed number used to reproduce results (same seed
#' = same results).
#' @param group_by_group Boolean option to fit the full factorial model
#' with all the interactions between each of the factors.
#' @param MCMC Perform MCMC sampling of the parameter after convergence
#' to estimate their 95% confidence interval.
#' @param interval TBD - Check the actual meaning.
#' @param se.fit If TRUE, Lifelihood computes the standard error of
#' each parameter using the variance-covariance matrix.
#' @param saveprobevent TBD - Check the actual meaning.
#' @param fitness Reparametrize the model with one parameter as the
#' intrinsic rate of increase.
#' @param r Reparametrize the model with one parameter as the intrinsic
#' rate of increase.
#' @param ratiomax Maximum multiplicative factor for clutch size in
#' models with reproductive senescence.
#' @param tc Critical age (after the juvenile mortality peak) at which
#' the survival model starts to be fitted.
#' @param tinf Maximum censoring time (should be greater than the age
#' of the oldest individual observed dead in the dataset).
#' @param sub_interval Sub-interval used to integrate the left and
#' right censoring dates of each event.
#' @param path_continuous_var Path to the continuous variables file.
#' @param ntr Number of thread for the paralelisation.
#' @param nst TBD - Check the actual meaning.
#' @param To Initial temperature for the simulated annealing.
#' @param Tf Final temperature for the simulated annealing.
#' @param climbrate Rate for the simulated annealing.
#' @param precision Precision parameter for the algorithm.
execute_bin <- function(
  path_to_Lifelihood,
  path_input_data,
  path_param_bounds,
  group_by_group,
  MCMC,
  interval,
  se.fit,
  saveprobevent,
  fitness,
  r,
  seed1,
  seed2,
  seed3,
  seed4,
  ratiomax,
  tc,
  tinf,
  sub_interval,
  path_continuous_var,
  ntr,
  nst,
  To,
  Tf,
  climbrate,
  precision
) {
  arg_string <- paste(
    path_input_data,
    path_param_bounds,
    group_by_group,
    MCMC,
    interval,
    se.fit,
    saveprobevent,
    fitness,
    r,
    seed1,
    seed2,
    seed3,
    seed4,
    ratiomax,
    tc,
    tinf,
    sub_interval,
    path_continuous_var,
    ntr,
    nst,
    To,
    Tf,
    climbrate,
    precision
  )
  if (is.null(path_to_Lifelihood)) {
    os <- detect_os()
    path <- switch(
      os,
      "Windows" = system.file(
        "bin",
        "lifelihood-windows.exe",
        package = "lifelihood",
        mustWork = TRUE
      ),
      "Darwin" = system.file(
        "bin",
        "lifelihood-macos",
        package = "lifelihood",
        mustWork = TRUE
      ),
      "Linux" = system.file(
        "bin",
        "lifelihood-linux",
        package = "lifelihood",
        mustWork = TRUE
      ),
      stop("Unexpect OS: ", os)
    )
  } else {
    path <- path_to_Lifelihood
  }

  print(paste0(path, " ", arg_string))
  system(path, input = arg_string)
}
