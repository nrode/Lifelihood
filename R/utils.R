#' @title Make the design matrix
#'
#' @keywords internal
#'
#' @description
#' Create the design matrix for the expected death and
#' the survival shape.
#'
#' @inheritParams lifelihood
#' @inheritParams pred_mortality_rate
#'
#' @export
make_design_matrix <- function(covariates, data) {
  if (!all(covariates %in% names(data))) {
    stop("Some covariate names are not present in the data.")
  }

  factor_levels <- lapply(covariates, function(cov) {
    if (is.factor(data[[cov]])) {
      levels(data[[cov]])
    } else {
      unique(data[[cov]])
    }
  })
  factor_levels

  names(factor_levels) <- covariates
  fitted_data <- do.call(expand.grid, factor_levels)
  formula_str <- paste("~", paste(covariates, collapse = " + "))
  formula <- as.formula(formula_str)

  mat_expt_death <- model.matrix(formula, fitted_data)
  mat_survival_shape <- model.matrix(formula, fitted_data)

  return(list(
    fitted_data = fitted_data,
    mat_expt_death = mat_expt_death,
    mat_survival_shape = mat_survival_shape
  ))
}

#' @title Weibull survival function
#'
#' @description
#' Weibull survival function
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param ExpLong Numeric. The expected longevity
#' @param Shape Numeric. The shape parameter
#'
#' @return The survival probability (numeric)
#'
#' @export
SurvWei <- function(t, ExpLong, Shape) {
  Scale <- ExpLong / gamma(1 + 1 / Shape)
  return(exp(-(t / Scale)^Shape))
}

#' @title Find the operating system of the user
#'
#' @description
#' `detect_os()` finds the operating system name
#'
#' @keywords internal
#'
#' @return String with the name of the operating system, either "Windows" or "Unix-like"
#'
#' @examples
#' detect_os()
#'
#' @export
detect_os <- function() {
  compatible_oses <- c("Windows", "Darwin")
  os <- Sys.info()["sysname"]
  if (os %in% compatible_oses) {
    return(os)
  } else {
    stop(
      "lifelihood only works with MacOS and Windows at the moment, not: ",
      os
    )
  }
}

#' @title Get the path to a built-in configuration file.
#'
#' @description
#' `lifelihood` embeds a few configuration files, and
#' this function is a simple tool to access one of them.
#'
#' It takes the name one of the available configuration
#' and returns the path to it.
#'
#' For more info about configuration files, see
#' \code{vignette("setting-up-the-configuration-file", package = "lifelihood")}
#'
#' @param config_name Configuration name. Currently available options:
#' - config
#' - config2
#' - config_pierrick
#' By default, it will use "config".
#'
#' @return Absolute path to the configuration file
#'
#' @examples
#' get_config_path("config")
#' get_config_path("config2")
#'
#' @export
get_config_path <- function(
  config_name = c("config", "config2", "config_pierrick")
) {
  config_name <- match.arg(config_name)

  config_path <- system.file(
    "configs",
    paste0(config_name, ".yaml"),
    package = "lifelihood"
  )

  return(config_path)
}

load_data <- function(dataset) {
  path <- system.file(
    "data",
    "fakesample.rda",
    package = "lifelihood",
    mustWork = TRUE
  )
  load(path)
}

#' @title Remove all lifelihood temporary files
#'
#' @description
#' By default, [lifelihood()] deletes all the temp files
#' it creates, but users can set `delete_temp_files=FALSE`
#' to keep them.
#'
#' After multiple runs, there can be lots of temp files.
#' This function will just remove them.
#'
#' @param path Where to look for. Default to current dir.
#'
#' @examples
#' remove_lifelihood_tempfiles()
#'
#' @export
remove_lifelihood_tempfiles <- function(path = ".") {
  # regex pattern for the directory names
  pattern <- "^lifelihood_\\d+_\\d+_\\d+_\\d+"

  dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  target_dirs <- dirs[grepl(pattern, basename(dirs))]

  for (dir in target_dirs) {
    # delete directory if it contains 3 or fewer files
    files <- list.files(dir, full.names = TRUE)
    file_count <- sum(!file.info(files)$isdir)

    if (file_count <= 3) {
      unlink(dir, recursive = TRUE)
      message(sprintf("Deleted: %s (contained %d files)", dir, file_count))
    }
  }
}

#' @title Check that an object is of class `lifelihoodResults`
#'
#' @description
#' Internally, `lifelihood` has to check multiple times
#' that the passed object is the expected one.
#'
#' It basically raises an explicit error if the object
#' is not of class `lifelihoodResults`.
#'
#' @param object An object to test.
#'
#' @examples
#' \dontrun{
#' # raise an error
#' obj <- c(1,2,3)
#' check_valid_lifelihoodResults(obj)
#'
#' # works (does nothing)
#' class(obj) = "lifelihoodResults"
#' check_valid_lifelihoodResults(obj)
#' }
#'
#' @keywords internal
check_valid_lifelihoodResults <- function(object) {
  if (!(inherits(object, "lifelihoodResults"))) {
    stop(paste0(
      "`object` expects a 'lifelihoodResults' object, not: '",
      class(object),
      "'"
    ))
  }
}

#' @title Convert a factor variable to integers
#'
#' @description
#' Function necessary because the Pascal program
#' expects integers for factor levels.
#'
#' @param x A column name
#'
#' @returns The integer
#'
#' @keywords internal
factor_to_num <- function(x) {
  if (is.factor(x)) {
    levels <- levels(x)
    num <- match(x, levels) - 1
    levels(num) <- levels
    num
  } else {
    x
  }
}
