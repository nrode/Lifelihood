#' @title Compute empirical mortality rate
#'
#' @description
#' Calculate the empirical mortality rate over a given interval.
#'
#' @inheritParams lifelihood
#' @inheritParams validate_groupby_arg
#' @param interval_width The interval width used to calculate the
#' mortality rate. For instance, if the time unit for deaths in
#' the original dataset is days and `interval_width` is set to 10,
#' the mortality rate will be calculated every 10 days for each group.
#' @param max_time The maximum time for calculating the mortality
#' rate. If set to NULL, the time of the last observed death is used.
#' @param min_sample_size The minimum number of individuals alive 
#' at the beggining of a time interval for computing the observed mortality rate
#' 
#' @return A dataframe with 3 columns: Interval (time interval, based
#' on `interval_width` value), Group (identifier of a given subgroup,
#' or "Overall" if groupby = NULL), and MortalityRate (mortality rate
#' at this time).
#'
#' @importFrom dplyr mutate if_else
#'
#' @examples
#' library(lifelihood)
#' library(tidyverse)
#'
#' df <- fakesample |>
#'   mutate(
#'     geno = as.factor(geno),
#'     type = as.factor(type)
#'   )
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
#' mort_df <- compute_mortality_rate(dataLFH, interval_width = 2)
#' head(mort_df)
#'
#' mort_df <- compute_mortality_rate(
#'   dataLFH,
#'   interval_width = 2,
#'   groupby = NULL,
#'   max_time = 170
#' )
#' head(mort_df)
#' @export
compute_mortality_rate <- function(
  lifelihoodData,
  interval_width,
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL
) {
  groupby <- validate_groupby_arg(lifelihoodData, groupby)

  data <- lifelihoodData$df
  start_col <- lifelihoodData$death_start
  end_col <- lifelihoodData$death_end
  covariates <- lifelihoodData$covariates
  right_censoring_date <- lifelihoodData$right_censoring_date

  if (is.null(max_time)) {
    sorted_values <- sort(
      unique(data[[end_col]]),
      decreasing = TRUE,
      na.last = NA
    )
    if (sorted_values[1] == right_censoring_date) {
      max_time <- sorted_values[2]
    } else {
      max_time <- sorted_values[1]
    }
  }

  n_intervals <- ceiling(max_time / interval_width)

  if (!is.null(groupby)) {
    data$group <- do.call(interaction, data[groupby])
    groups <- sort(unique(data$group))
  } else {
    data$group <- "Overall"
    groups <- "Overall"
  }

  mortality_rate <- matrix(0, nrow = n_intervals, ncol = length(groups))
  colnames(mortality_rate) <- groups
  rownames(mortality_rate) <- seq(
    interval_width,
    n_intervals * interval_width,
    by = interval_width
  )

  for (grp in groups) {
    group_data <- data[data$group == grp, ]

    for (i in 1:n_intervals) {
      interval_start <- (i - 1) * interval_width
      interval_end <- i * interval_width

      alive_start <- sum(
        group_data[[start_col]] >= interval_start |
          is.na(group_data[[start_col]])
      )

      deaths <- sum(
        group_data[[start_col]] >= interval_start &
          group_data[[start_col]] < interval_end &
          !is.na(group_data[[end_col]])
      )

      mortality_rate[i, grp] <- if (alive_start > min_sample_size) deaths / alive_start else
        NA
    }
  }

  mortality_rate_df <- reshape2::melt(mortality_rate)
  colnames(mortality_rate_df) <- c("Interval_end", "Group", "MortalityRate")
  mortality_rate_df$Group <- as.factor(mortality_rate_df$Group)

  if (is.null(groupby)) {
    mortality_rate_df <- subset(mortality_rate_df, select = -c(Group))
  }

  # remove times where mortality rate is 1
  mortality_rate_df |>
    dplyr::mutate(
      Interval_start = Interval_end-interval_width,
      Mean_Interval = Interval_end-interval_width/2
      ) |>
    dplyr::relocate(Interval_start, .before = Interval_end) |>
    dplyr::relocate(Mean_Interval, .before = Group)

  return(mortality_rate_df)
}


#' @title Check that the `groupby` argument is valid
#'
#' @description
#' Check that `groupby` has an expected value, and returns it
#'
#' @inheritParams lifelihood
#' @param groupby covariate(s) over which mortality rate should be
#' computed (default is `NULL`).
#' - If NULL, calculates a single overall mortality rate.
#' - If `"all"`, calculates mortality rate over each combination
#' of covariates listed in the`lifelihoodData` object provided.
#' - Otherwise must be a character (`"covariate1"`) or a
#' character vector (`c("covariate1", "covariate2")`).
#'
#' @returns The valid `groupby` value
#'
#' @keywords internal
validate_groupby_arg <- function(lifelihoodData, groupby) {
  if (is.null(groupby)) {
    return(NULL)
  } else if (length(groupby) > 1) {
    covariates <- lifelihoodData$covariates

    if (!all(groupby %in% covariates)) {
      missing_vars <- groupby[!groupby %in% covariates]
      stop(
        "`groupby` argument contains invalid values. ",
        "These are not in the covariates of the `lifelihoodData` object: ",
        paste0(missing_vars, collapse = ", "),
        ".\n",
        "Valid covariates are: ",
        paste0(covariates, collapse = ", ")
      )
    } else {
      return(covariates)
    }
  } else if (groupby == "all") {
    return(lifelihoodData$covariates)
  } else {
    return(groupby)
  }
}
