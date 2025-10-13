#' @title Execution of the compiled files
#'
#' @keywords internal
#'
#' @description
#' Run lifelihood program in console mode.
#'
#' @param path_to_Lifelihood A character string specifying the file path to the compile Lifelihood program (default is NULL)
#' @param path_input_data Path to the input text file with the model and data to be fitted.
#' @param path_param_bounds Path to the parameter boundaries text file with the min and max boudaries for each parameter.
#' @param seed1 First seed number used to reproduce results (same seed = same results).
#' @param seed2 Second seed number used to reproduce results (same seed = same results).
#' @param seed3 Third seed number used to reproduce results (same seed = same results).
#' @param seed4 Fourth seed number used to reproduce results (same seed = same results).
#' @inheritParams lifelihood
#' @inheritParams lifelihoodData
execute_bin <- function(
  path_to_Lifelihood,
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
    SEcal,
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
