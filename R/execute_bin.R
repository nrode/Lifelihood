.normalize_machine <- function(machine) {
  machine <- tolower(machine)
  machine <- gsub("-", "_", machine, fixed = TRUE)
  switch(
    machine,
    "amd64" = "x86_64",
    "x64" = "x86_64",
    "arm64" = "aarch64",
    machine
  )
}

.lifelihood_binary_candidates <- function(os, machine) {
  candidates <- switch(
    os,
    "Windows" = "lifelihood-windows.exe",
    "Darwin" = switch(
      machine,
      "x86_64" = "lifelihood-macos-x86_64",
      "aarch64" = c("lifelihood-macos-aarch64", "lifelihood-macos"),
      character()
    ),
    "Linux" = switch(
      machine,
      "x86_64" = c("lifelihood-linux-x86_64", "lifelihood-linux"),
      "aarch64" = "lifelihood-linux-aarch64",
      character()
    ),
    character()
  )

  if (length(candidates) == 0) {
    stop(
      "No bundled Lifelihood executable for ",
      os,
      "/",
      machine,
      ". ",
      "Build one with `just` or provide `path_to_Lifelihood`."
    )
  }

  candidates
}

.find_lifelihood_binary <- function() {
  os <- detect_os()
  machine <- .normalize_machine(Sys.info()[["machine"]])
  candidates <- .lifelihood_binary_candidates(os, machine)

  paths <- vapply(
    candidates,
    function(candidate) {
      system.file(
        "bin",
        candidate,
        package = "lifelihood",
        mustWork = FALSE
      )
    },
    character(1)
  )
  paths <- paths[nzchar(paths)]

  if (length(paths) > 0) {
    return(unname(paths[[1]]))
  }

  stop(
    "No bundled Lifelihood executable found for ",
    os,
    "/",
    machine,
    ". Expected one of: ",
    paste(candidates, collapse = ", "),
    ". Build one with `just` or provide `path_to_Lifelihood`."
  )
}

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
#' @param nst Simulated annealing tuning parameter increasing nst makes the search more thorough at each temperature (better chance of finding the global maximum, slower fit)
#' @param To Initial temperature for the simulated annealing.
#' @param Tf Final temperature for the simulated annealing.
#' @param climbrate Initial cooling rate of the adaptive annealing schedule: scales how fast the temperature is lowered between annealing runs (self-adjusting during the fit).
#' @param precision Convergence tolerance of the final local search: fitting stops when an additional batch of local-search iterations improves the log-likelihood by less than this amount.
execute_bin <- function(
  path_to_Lifelihood,
  path_input_data,
  path_param_bounds,
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
    path <- .find_lifelihood_binary()
  } else {
    path <- path_to_Lifelihood
  }

  print(paste0(path, " ", arg_string))
  system(path, input = arg_string)
}
