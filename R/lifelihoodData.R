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
#' @param maturity_start Column name containing the first
#' date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second
#' date of the interval in which the maturity was determined.
#' @param clutchs Vector containing the names of the clutch
#' columns. The order should be: first clutch first date, first
#' clutch second date, first clutch clutch size, second clutch
#' first date, first clutch second date, second clutch clutch
#' size, and so on. If the observation with the most clutches
#' is, for example, 10, then the vector must be of size 10 x 3 = 30
#' (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of
#' the interval in which the death was determined.
#' @param death_end Column name containing the second date of
#' the interval in which the death was determined.
#' @param dist Vector of characters with the name of the
#' statistical law to use. Must be of length 3 and each element
#' must be one of "wei" (Weibull law), "exp" (Exponential law),
#' "gam" (Gamma law) or "lgn" (Log-normal law). The first one is
#' used for mortality, the second one is used for maturity and the
#' third is used for reproduction.
#' @param covariates Vector containing the names of the covariates.
#' @param block Column name containing the block to which each individual belong to.
#' @param matclutch Whether the maturity event (designated by
#' `maturity_start` and `maturity_end`) is a clutch event or not.
#' If `TRUE`, must specify the `matclutch_size` argument. Default
#' is `FALSE`.
#' @param matclutch_size Column name containing the size of the
#' clutch for the maturity event. Only used (and required) if
#' `matclutch` is `TRUE`.
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
  maturity_start,
  maturity_end,
  clutchs,
  death_start,
  death_end,
  dist,
  covariates,
  block = NULL,
  matclutch,
  matclutch_size = NULL,
  right_censoring_date = 1000,
  critical_age = 20,
  ratiomax = 10
) {
  valid_dist <- c("wei", "gam", "lgn", "exp")
  if (length(dist) != 3 || !all(dist %in% valid_dist)) {
    stop(
      "'dist' must be a character vector of length 3 containing only 'wei', 'exp', 'gam', or 'lgn'"
    )
  }

  if (isTRUE(matclutch) && is.null(matclutch_size)) {
    stop("`matclutch_size` argument cannot be NULL when `matclutch` is TRUE.")
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
