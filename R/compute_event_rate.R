#' @title Compute fitted mortality rate
#'
#' @description
#' Calculate the empirical mortality rate over a given interval
#' on some new data.
#'
#' @param lifelihoodResults A `lifelihoodResults` object
#' @inheritParams compute_observed_event_rate
#'
#' @return A dataframe with 3 columns: Interval (time interval, based
#' on `interval_width` value), group (identifier of a given subgroup,
#' or "Overall" if groupby = NULL), and Event_rate (event rate
#' at this time).
#'
#' @import dplyr
#'
#' @export
compute_fitted_event_rate <- function(
  lifelihoodResults,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  max_time = NULL,
  groupby = NULL
) {
  event <- match.arg(event)
  check_valid_lifelihoodResults(lifelihoodResults)

  lifelihoodData <- lifelihoodResults$lifelihoodData

  if (event == "mortality") {
    start_col <- lifelihoodData$death_start
    end_col <- lifelihoodData$death_end
    family <- lifelihoodData$model_specs[1]
    covar <- c(
      lifelihoodResults$formula$expt_death,
      lifelihoodResults$formula$survival_param2
    ) |>
      unique() |>
      setdiff("intercept")
  } else if (event == "maturity") {
    start_col <- lifelihoodData$maturity_start
    end_col <- lifelihoodData$maturity_end
    family <- lifelihoodData$model_specs[2]
    covar <- c(
      lifelihoodResults$formula$expt_maturity,
      lifelihoodResults$formula$maturity_param2
    ) |>
      unique() |>
      setdiff("intercept")
  } else if (event == "reproduction") {
    # Use death columns for determining alive status
    start_col <- lifelihoodData$death_start
    end_col <- lifelihoodData$death_end
    family <- lifelihoodData$model_specs[3]
    covar <- c(
      lifelihoodResults$formula$expt_reproduction,
      lifelihoodResults$formula$reproduction_param2
    ) |>
      unique() |>
      setdiff("intercept")
  }

  if (!all(groupby %in% covar)) {
    missing_vars <- groupby[!groupby %in% covar]
    stop(
      "`groupby` argument contains invalid values. ",
      paste0(
        "Covariate(s) `",
        paste0(missing_vars, collapse = ", "),
        "` not fitted for event `",
        event,
        "` in the `lifelihoodResults` object provided"
      ),
      ".\n",
      paste0(
        "Fitted covariate(s) for`",
        event,
        "` event: ",
        paste0(covar, collapse = ", ")
      )
    )
  }
  if (is.null(max_time)) {
    sorted_values <- sort(
      unique(lifelihoodData$df[[end_col]]),
      decreasing = TRUE,
      na.last = NA
    )
    if (sorted_values[1] == lifelihoodData$right_censoring_date) {
      max_time <- sorted_values[2]
    } else {
      max_time <- sorted_values[1]
    }
  }

  n_intervals <- ceiling(max_time / interval_width) - 1

  if (is.null(newdata)) {
    params <- setNames(
      lapply(covar, function(x) levels(as.factor(lifelihoodData$df[[x]]))),
      covar
    )
    params$time <- seq(
      from = 0,
      to = (n_intervals - 1) * interval_width,
      by = interval_width
    )
    newdata <- expand.grid(params) |> relocate(time)
    newdata <- newdata |>
      dplyr::mutate(
        Interval_start = time,
        Interval_end = time + interval_width,
        Mean_Interval = time + interval_width / 2
      )
  }

  if (!is.null(groupby)) {
    ## Remove interactions not present in original dataset
    newdata <- newdata |>
      dplyr::mutate(Group_tmp = interaction(newdata[groupby])) |>
      dplyr::filter(
        Group_tmp %in% unique(interaction(lifelihoodData$df[groupby]))
      ) |>
      dplyr::select(-Group_tmp)

    newdata <- newdata |>
      dplyr::arrange(across(all_of(groupby))) |>
      dplyr::mutate(across(
        all_of(groupby),
        ~ paste0(cur_column(), "=", .),
        .names = "{.col}"
      )) |>
      tidyr::unite("group", all_of(groupby), sep = ".", remove = FALSE)
  } else {
    newdata$group <- "Overall"
  }

  newdata$group <- as.factor(newdata$group)

  if (event == "mortality") {
    parameter_name1 <- "expt_death"
    parameter_name2 <- "survival_param2"
  } else if (event == "maturity") {
    parameter_name1 <- "expt_maturity"
    parameter_name2 <- "maturity_param2"
  } else if (event == "reproduction") {
    parameter_name1 <- "expt_reproduction"
    parameter_name2 <- "reproduction_param2"
  }

  param1 <- prediction(
    object = lifelihoodResults,
    parameter_name = parameter_name1,
    newdata = newdata,
    type = "response"
  )
  param2 <- prediction(
    object = lifelihoodResults,
    parameter_name = parameter_name2,
    newdata = newdata,
    type = "response"
  )

  newdata$Event_Rate <- prob_event_interval_dt(
    t = newdata$time,
    dt = interval_width,
    param1 = param1,
    param2 = param2,
    family = family
  )

  if (event == "reproduction") {
    newdata$n_offspring <- prediction(
      lifelihoodResults,
      parameter_name = "n_offspring",
      newdata = newdata,
      type = "response"
    )
  }
  return(newdata)
}

#' @title Compute empirical event rate
#'
#' @description
#' Calculate the empirical event rate over a given interval.
#'
#' @inheritParams lifelihood
#' @inheritParams validate_groupby_arg
#' @param interval_width The interval width used to calculate the
#' event rate. For instance, if the time unit for deaths in
#' the original dataset is days and `interval_width` is set to 10,
#' the event rate will be calculated every 10 days for each group.
#' @param newdata Data for computation. If absent, predictions are for
#' the subjects used in the original fit.
#' @param max_time The maximum time for calculating the event
#' rate. If set to NULL, the time of the last observed death is used.
#' @param min_sample_size The minimum number of individuals alive
#' at the beggining of a time interval for computing the observed event rate
#'
#' @return A dataframe with 3 columns: Interval (time interval, based
#' on `interval_width` value), group (identifier of a given subgroup,
#' or "Overall" if groupby = NULL), and Event_rate (event rate
#' at this time).
#'
#' @importFrom dplyr mutate if_else select
#'
#' @export
compute_observed_event_rate <- function(
  lifelihoodData,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL
) {
  event <- match.arg(event)
  groupby <- validate_groupby_arg(lifelihoodData, groupby)

  if (is.null(newdata)) {
    newdata <- lifelihoodData$df
  }

  # Select event-specific columns
  if (event == "reproduction") {
    # Extract clutch column names - pattern is start, end, size repeated
    clutch_cols <- lifelihoodData$clutchs
    n_clutches <- length(clutch_cols) / 3
    start_cols <- clutch_cols[seq(1, length(clutch_cols), by = 3)]
    end_cols <- clutch_cols[seq(2, length(clutch_cols), by = 3)]
    size_cols <- clutch_cols[seq(3, length(clutch_cols), by = 3)]

    # Use death columns for determining alive status
    start_col <- lifelihoodData$death_start
    end_col <- lifelihoodData$death_end
  } else if (event == "mortality") {
    start_col <- lifelihoodData$death_start
    end_col <- lifelihoodData$death_end
  } else if (event == "maturity") {
    start_col <- lifelihoodData$maturity_start
    end_col <- lifelihoodData$maturity_end
  }

  right_censoring_date <- lifelihoodData$right_censoring_date

  if (is.null(max_time)) {
    sorted_values <- sort(
      unique(lifelihoodData$df[[end_col]]),
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
    ## Add group column to newdata
    newdata <- newdata |>
      dplyr::arrange(across(all_of(groupby))) |>
      dplyr::mutate(across(
        all_of(groupby),
        ~ paste0(cur_column(), "=", .),
        .names = "{.col}_tmp"
      )) |> ## Add the name of the columns to the group
      tidyr::unite(
        "group",
        all_of(paste0(groupby, "_tmp")),
        sep = ".",
        remove = FALSE
      ) |>
      dplyr::mutate(group = as.factor(group))

    groups <- levels(newdata$group)
    # creates a empty dataframe where the estimated event rates will be stored
    params <- setNames(
      lapply(groupby, function(x) levels(as.factor(lifelihoodData$df[[x]]))),
      groupby
    )
    params$time <- seq(
      from = 0,
      to = (n_intervals - 1) * interval_width,
      by = interval_width
    )

    event_rate <- expand.grid(params) |> dplyr::relocate(time)

    event_rate <- event_rate |>
      dplyr::mutate(across(
        all_of(groupby),
        ~ paste0(cur_column(), "=", .),
        .names = "{.col}"
      )) |>
      tidyr::unite("group", all_of(groupby), sep = ".", remove = FALSE) |>
      dplyr::filter(group %in% groups) |>
      dplyr::mutate(group = as.factor(group))
  } else {
    newdata$group <- "Overall"
    groups <- "Overall"
    event_rate <- expand.grid(
      time = seq(
        from = 0,
        to = n_intervals * interval_width,
        by = interval_width
      ),
      group = "Overall"
    )
  }

  event_rate <- event_rate |>
    dplyr::mutate(
      Interval_start = time,
      Interval_end = time + interval_width,
      Mean_Interval = time + interval_width / 2
    )

  event_rate$Event_Rate <- 0

  # Main counting loop
  for (grp in groups) {
    group_data <- newdata[newdata$group == grp, ]

    for (i in 1:n_intervals) {
      interval_start <- (i - 1) * interval_width
      interval_end <- i * interval_width

      # Number of individuals alive at the beggining of the time interval
      alive_start <- sum(
        group_data[[start_col]] >= interval_start &
          (group_data[[end_col]] != right_censoring_date |
            (group_data[[end_col]] == right_censoring_date &
              group_data[[start_col]] >= interval_end))
      )

      ## mortality or maturity: count event occurrences
      if (event %in% c("mortality", "maturity")) {
        events <- sum(
          group_data[[start_col]] >= interval_start &
            group_data[[end_col]] < interval_end
        )
      }

      # reproduction: total offspring across all clutches in interval
      if (event == "reproduction") {
        events <- 0

        # Iterate through all clutch events
        for (j in seq_along(start_cols)) {
          clutch_start <- group_data[[start_cols[j]]]
          clutch_end <- group_data[[end_cols[j]]]
          clutch_size <- group_data[[size_cols[j]]]

          # Count offspring for clutches that fall within the interval
          # A clutch is counted if its timing overlaps with the interval
          events <- events +
            sum(
              ifelse(
                !is.na(clutch_end) &
                  !is.na(clutch_start) &
                  clutch_end < interval_end &
                  clutch_start >= interval_start,
                clutch_size,
                0
              ),
              na.rm = TRUE
            )
        }
      }

      event_rate$Event_Rate[
        event_rate$time == interval_start & event_rate$group == grp
      ] <- if (alive_start > min_sample_size) events / alive_start else NA
    }
  }

  if (is.null(groupby)) {
    event_rate <- dplyr::select(event_rate, -group)
  }

  return(event_rate)
}

#' @title Check that the `groupby` argument is valid
#'
#' @description
#' Check that `groupby` has an expected value, and returns it
#'
#' @inheritParams lifelihood
#' @param groupby vector of covariate(s) over which mortality rate should be
#' computed (default is `NULL`).
#' - If NULL, calculates a single overall mortality rate.
#' - If `"all"`, calculates mortality rate over each combination
#' of covariates listed in the`lifelihoodData` object provided.
#' - Otherwise must be a character (`"covariate1"`) or a
#' character vector (`c("covariate1", "covariate2")`).
#' Note that the function will consider continuous covariates as factors
#'
#' @returns The valid `groupby` value
#'
#' @keywords internal
validate_groupby_arg <- function(lifelihoodData, groupby) {
  if (is.null(groupby)) {
    return(NULL)
  } else if (length(groupby) == 1 && groupby == "all") {
    return(lifelihoodData$covariates)
  } else {
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
      return(groupby)
    }
  }
}
