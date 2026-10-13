#' @title Data object for lifelihood
#'
#' @description
#' Creates a `lifelihoodData` object, which is a list containing
#' all the information needed to run the lifelihood program of a
#' given dataset of individual life history. This function will
#' mainly be used to pass to [lifelihood()] or for customizing
#' parameter boundaries with [default_bounds_df()].
#'
#' @param df Dataframe with the data of life history. It
#' should have one row per life history / observation.
#' @param sex Column name containing the sex of the
#' observations.
#' @param sex_start Column name containing the first
#' date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date
#' of the interval in which the sex was determined.
#' @param maturity_start Optional column name containing the first
#' date of the interval in which the maturity was determined. Required when
#' fitting maturity or reproduction.
#' @param maturity_end Optional column name containing the second
#' date of the interval in which the maturity was determined. Required when
#' fitting maturity or reproduction.
#' @param clutchs Optional vector containing the names of the clutch
#' columns. The order should be: first clutch first date, first
#' clutch second date, first clutch clutch size, second clutch
#' first date, first clutch second date, second clutch clutch
#' size, and so on. If the observation with the most clutches
#' is, for example, 10, then the vector must be of size 10 x 3 = 30
#' (3 elements per clutch: first date, second date and size).
#' @param death_start Optional column name containing the first date of
#' the interval in which the death was determined. Required when fitting
#' mortality or reproduction.
#' @param death_end Optional column name containing the second date of
#' the interval in which the death was determined. Required when fitting
#' mortality or reproduction.
#' @param dist Data frame with one or more columns selected from `mortality`,
#' `maturity`, and `reproduction`. Each row specifies one model to fit with
#' [lifelihood()]. Reproduction requires mortality and maturity data.
#' Each cell must contain "wei" (Weibull), "exp" (exponential), "gam" (gamma),
#' or "lgn" (log-normal). Use a one-row data frame to fit a single model.
#' @param covariates Vector containing the names of the covariates.
#' @param block Column name containing the block to which each individual belong to.
#' @param matclutch Whether the maturity event (designated by
#' `maturity_start` and `maturity_end`) is a clutch event or not.
#' If `TRUE`, must specify the `matclutch_size` argument. Default
#' is `FALSE`.
#' @param matclutch_size Column name containing the size of the
#' clutch for the maturity event. Required when `matclutch` is `TRUE` and
#' maturity data are used, including as a dependency of reproduction.
#' @param right_censoring_date (CURRENTLY IGNORED) Time (integer)
#' point at which a subject’s data is censored. This means that for
#' subjects who do not experience the event of interest (e.g.,
#' death, failure) by this date, their data is considered censored.
#' In practice, choose a value much larger than the maximum longevity
#' seen in the data.
#' @param critical_age (CURRENTLY IGNORED) Critical age (integer)
#' below which life histories are not followed individually.
#' @param ratiomax (CURRENTLY IGNORED) Maximum ratio (integer)
#' between number of offspring of last and first reproduction events.
#' Cannot be greater than ratiomax.
#'
#' @return `lifelihoodData` object
#'
#' @export
as_lifelihoodData <- function(
  df,
  sex,
  sex_start,
  sex_end,
  maturity_start = NULL,
  maturity_end = NULL,
  clutchs = NULL,
  death_start = NULL,
  death_end = NULL,
  dist,
  covariates,
  block = NULL,
  matclutch = FALSE,
  matclutch_size = NULL,
  right_censoring_date = 1000,
  critical_age = 20,
  ratiomax = 10
) {
  dist <- validate_dist(dist)

  required_events <- names(dist)
  if ("reproduction" %in% required_events) {
    required_events <- union(required_events, c("mortality", "maturity"))
  }

  event_columns <- list(
    mortality = c(death_start, death_end),
    maturity = c(maturity_start, maturity_end),
    reproduction = clutchs
  )
  event_columns <- event_columns[required_events]
  for (event in names(event_columns)) {
    columns <- event_columns[[event]]
    if (event == "reproduction") {
      if (
        !is.character(columns) ||
          length(columns) == 0 ||
          length(columns) %% 3 != 0 ||
          anyNA(columns) ||
          any(columns == "")
      ) {
        stop(
          "`clutchs` must contain one or more start/end/size triplets when fitting reproduction.",
          call. = FALSE
        )
      }
    } else if (
      !is.character(columns) ||
        length(columns) != 2 ||
        anyNA(columns) ||
        any(columns == "")
    ) {
      stop(
        "Both interval column names are required when fitting ",
        event,
        ".",
        call. = FALSE
      )
    }
    missing_columns <- setdiff(columns, colnames(df))
    if (length(missing_columns) > 0) {
      stop(
        "Column(s) required for fitting ",
        event,
        " are missing from `df`: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (
    isTRUE(matclutch) &&
      "maturity" %in% required_events &&
      is.null(matclutch_size)
  ) {
    stop("`matclutch_size` argument cannot be NULL when `matclutch` is TRUE.")
  }

  if (isTRUE(matclutch) && "maturity" %in% required_events) {
    if (
      !is.character(matclutch_size) ||
        length(matclutch_size) != 1 ||
        !matclutch_size %in% colnames(df)
    ) {
      stop(
        "`matclutch_size` must name a column in `df` when `matclutch` is TRUE."
      )
    }

    if (!is.numeric(df[[matclutch_size]])) {
      stop(
        "`matclutch_size` must refer to a numeric column in `df` when `matclutch` is TRUE."
      )
    }
  }

  if (isTRUE(matclutch) && !"maturity" %in% required_events) {
    warning(
      "`matclutch` and `matclutch_size` are ignored because neither maturity nor reproduction is fitted.",
      call. = FALSE
    )
    matclutch <- FALSE
    matclutch_size <- NULL
  } else if (!isTRUE(matclutch) && !is.null(matclutch_size)) {
    warning("`matclutch_size` is ignored when `matclutch` is FALSE.")
  }

  if (!is.null(block) && !block %in% colnames(df)) {
    stop(
      "`block` argument does not match any of the column names in the dataframe `df` provided."
    )
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
    dist = dist,
    covariates = covariates,
    block = block,
    matclutch = matclutch,
    matclutch_size = matclutch_size,
    right_censoring_date = right_censoring_date,
    critical_age = critical_age,
    ratiomax = ratiomax
  )
  class(dataObject) <- "lifelihoodData"
  return(dataObject)
}

#' @keywords internal
validate_dist <- function(dist) {
  required_columns <- c("mortality", "maturity", "reproduction")
  valid_values <- c("wei", "gam", "lgn", "exp")

  if (
    !is.data.frame(dist) ||
      nrow(dist) == 0 ||
      ncol(dist) == 0 ||
      is.null(names(dist)) ||
      anyNA(names(dist)) ||
      any(names(dist) == "") ||
      anyDuplicated(names(dist)) > 0 ||
      !all(names(dist) %in% required_columns) ||
      !all(vapply(
        dist,
        function(x) is.character(x) || is.factor(x),
        logical(1)
      )) ||
      anyNA(dist) ||
      !all(unlist(lapply(dist, as.character)) %in% valid_values)
  ) {
    stop(
      "`dist` must be a non-empty data frame with one or more columns selected from (",
      paste0(required_columns, collapse = ", "),
      ") where all cells are in ",
      paste0(valid_values, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  dist <- as.data.frame(dist[required_columns[
    required_columns %in% names(dist)
  ]])
  dist[] <- lapply(dist, as.character)
  dist
}

#' @keywords internal
complete_dist <- function(dist) {
  all_events <- c("mortality", "maturity", "reproduction")
  completed <- as.list(rep("exp", length(all_events)))
  names(completed) <- all_events
  completed[names(dist)] <- dist
  as.data.frame(completed, stringsAsFactors = FALSE)
}
