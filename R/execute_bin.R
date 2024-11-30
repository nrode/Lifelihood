#' @title Execution of the compiled files
#' @name execute_bin
#' @keywords internal
#' @description Run lifelihood program in console mode.
#' @param path_input_data Path to the input text file with the model and data to be fitted.
#' @param path_param_bounds Path to the parameter boundaries text file with the min and max boudaries for each parameter.
#' @param seed1 First seed number used to reproduce results (same seed = same results).
#' @param seed2 Second seed number used to reproduce results (same seed = same results).
#' @param seed3 Third seed number used to reproduce results (same seed = same results).
#' @param seed4 4th seed number used to reproduce results (same seed = same results).
#' @inheritParams lifelihood
#' @inheritParams lifelihoodData
#' @return NULL. This function writes an output file, that will then be parsed by [parse_output()].
#' @export
execute_bin <- function(
    path_input_data,
    path_param_bounds,
    group_by_group,
    MCMC,
    interval,
    SEcal,
    saveprobevent,
    fitness,
    r,
    seed1,
    seed2,
    seed3,
    seed4,
    ntr,
    nst,
    To,
    Tf,
    climbrate,
    precision) {
  # concatenate the inputs and other parameters
  arg_string <- paste(
    path_input_data, path_param_bounds, group_by_group, MCMC, interval, SEcal, saveprobevent,
    fitness, r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
  )

  # get the path to the compiled program and execute it
  os <- detect_os()
  path <- switch(os,
    "Windows" = file.path(here::here("src", "bin"), "lifelihoodC2023.exe"),
    "Unix-like" = file.path(here::here("src", "bin"), "lifelihoodC2023"),
    stop("Unknown OS")
  )
  print(arg_string)
  system(path, input = arg_string)
}
