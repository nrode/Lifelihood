#' @title Compute fitted mortality rate
#'
#' @description
#' Calculate the empirical mortality rate over a given interval
#' on some new data.
#'
#' @param lifelihoodResults output of [lifelihood()].
#' @param interval_width The interval width used to calculate the
#' event rate. For instance, if the time unit for deaths in
#' the original dataset is days and `interval_width` is set to 10,
#' the event rate will be calculated every 10 days for each group.
#' @param event Which event to compute? Must be one of "mortality", "maturity", "reproduction".
#' @param newdata Optional `data.frame` providing covariate values for prediction.
#' If `NULL`, the original model data are used.
#' @param max_time The maximum time for calculating the event
#' rate. If set to NULL, the time of the last observed death is used.
#' @param groupby One or multiple covariates used to group the computation.
#'
#' @return A dataframe with 3 columns: Interval (time interval, based
#' on `interval_width` value), group (identifier of a given subgroup,
#' or "Overall" if groupby = NULL), and Event_rate (event rate
#' over the interval).
#'Note that for reproduction event, the first reproduction event of
#'each individual cannot be computed if maturity was not observed (i.e. mat_clutch is true)
#'When the interval between the last reproduction event of an individual and their death is
#'greater than `interval_width` the individuals are included in the computation of reproduction rate
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
  check_lifelihoodResults(lifelihoodResults)

  lifelihoodData <- lifelihoodResults$lifelihoodData

  if (event == "mortality") {
    end_col <- lifelihoodData$death_end
    family <- lifelihoodData$model_specs[1]
    covar <- c(
      lifelihoodResults$formula$expt_death,
      lifelihoodResults$formula$survival_param2
    ) |>
      unique() |>
      setdiff("intercept")
  } else if (event == "maturity") {
    end_col <- lifelihoodData$maturity_end
    family <- lifelihoodData$model_specs[2]
    covar <- c(
      lifelihoodResults$formula$expt_maturity,
      lifelihoodResults$formula$maturity_param2
    ) |>
      unique() |>
      setdiff("intercept")
  } else if (event == "reproduction") {
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
        "Fitted covariate(s) for` ",
        event,
        "` event: ",
        paste0(covar, collapse = ", ")
      )
    )
  }
  if (is.null(max_time)) {
    if (event == "reproduction") {
      
      reproduction_intervals <- compute_reproduction_intervals(lifelihoodData, verbose=FALSE)
      sorted_values <- sort(
        unique(reproduction_intervals[["pon_end"]]),
        decreasing = TRUE,
        na.last = NA
      )
      
    }else{
      
      sorted_values <- sort(
        unique(lifelihoodData$df[[end_col]]),
        decreasing = TRUE,
        na.last = NA
      )
    }
    
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
#' @param max_time The maximum time for calculating the event
#' rate. If set to NULL, the time of the last observed death is used.
#' @param min_sample_size The minimum number of individuals alive
#' at the beggining of a time interval for computing the observed event rate
#' @param event Which event to compute? Must be one of "mortality", "maturity", "reproduction".
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
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL
) {
  event <- match.arg(event)
  check_lifelihoodData(lifelihoodData)
  groupby <- validate_groupby_arg(lifelihoodData, groupby)

  # Select event-specific columns
  if (event == "reproduction") {
    # Extract clutch column names - pattern is start, end, size repeated
    clutch_cols <- lifelihoodData$clutchs
    n_clutches <- length(clutch_cols) / 3
    start_cols <- clutch_cols[seq(1, length(clutch_cols), by = 3)]
    end_cols <- clutch_cols[seq(2, length(clutch_cols), by = 3)]
    size_cols <- clutch_cols[seq(3, length(clutch_cols), by = 3)]

    # Use death columns for determining alive status.
    # Start_col and death_col are defined after for
    # reproduction event.
    death_start_col <- lifelihoodData$death_start
    death_end_col <- lifelihoodData$death_end
    maturity_start_col <- lifelihoodData$maturity_start
    maturity_end_col <- lifelihoodData$maturity_end
    start_col <- "pon_start"
    end_col <- "pon_end"
  } else if (event == "mortality") {
    start_col <- lifelihoodData$death_start
    end_col <- lifelihoodData$death_end
  } else if (event == "maturity") {
    start_col <- lifelihoodData$maturity_start
    end_col <- lifelihoodData$maturity_end
  }

  newdata <- lifelihoodData$df
  right_censoring_date <- lifelihoodData$right_censoring_date

  if (event == "reproduction") {
    # since reproduction can occur multiple times for each individual,
    # we handle them as one reproduction = one event.
    # Each event is the difference between 2 reproduction events OR
    # between the death and the last reproduction (right censoring of
    # unobserved reproductions that could have occurred after death).
    newdata <- compute_reproduction_intervals(lifelihoodData)
    write.csv2(newdata, "here.csv")
  }

  if (is.null(max_time)) {
    sorted_values <- sort(
      unique(newdata[[end_col]]),
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

      # Number of individuals alive/immature/non-reproducing at the beggining of the time interval
      num_start <- sum(
        group_data[[start_col]] >= interval_start &
          (group_data[[end_col]] != right_censoring_date |
            (group_data[[end_col]] == right_censoring_date &
              group_data[[start_col]] >= interval_end))
      )
      # Number of individuals dead/mature/that reproduced at the end of the time interval
      num_failures <- sum(
        group_data[[start_col]] >= interval_start &
          group_data[[end_col]] < interval_end
      )

      event_rate$Event_Rate[
        event_rate$time == interval_start & event_rate$group == grp
      ] <- if (num_start > min_sample_size) num_failures / num_start else NA
    }
  }

  if (is.null(groupby)) {
    event_rate <- dplyr::select(event_rate, -group)
  }

  return(event_rate)
}

#' @title Compute time interval between clutches
#'
#' @description
#' This function computes the time interval between consecutive clutches
#' and add censored unobserved last clutch between the last clutch and time
#' of death unless the time of death is right censored. It also removes
#' individuals that never reproduced.
#'
#' @param lifelihoodData Ouput of [lifelihoodData()]
#' @param verbose Boolean whether to print messages during the computation process (default=TRUE)
#'
#' @return A dataframe with time interval between consecutive clutches starting from maturity.
#'
#' @export
compute_reproduction_intervals <- function(lifelihoodData, verbose=TRUE) {
  check_lifelihoodData(lifelihoodData)

  # Extract clutch column names - pattern is start, end, size repeated
  clutch_cols <- lifelihoodData$clutchs
  n_clutches <- length(clutch_cols) / 3
  start_cols <- clutch_cols[seq(1, length(clutch_cols), by = 3)]
  end_cols <- clutch_cols[seq(2, length(clutch_cols), by = 3)]
  size_cols <- clutch_cols[seq(3, length(clutch_cols), by = 3)]

  right_censoring_date <- lifelihoodData$right_censoring_date
  newdata <- lifelihoodData$df

  # Use death columns for determining alive status.
  # Start_col and death_col are defined after for
  # reproduction event.
  death_start_col <- lifelihoodData$death_start
  death_end_col <- lifelihoodData$death_end
  maturity_start_col <- lifelihoodData$maturity_start
  maturity_end_col <- lifelihoodData$maturity_end

  id_removed <- which(
    newdata[, lifelihoodData$maturity_end] == right_censoring_date
  )
  if (length(id_removed) > 0 & verbose) {
    message(glue(
      "Removed individuals {paste0(id_removed, collapse=', ')} with no reproduction events"
    ))
  }

  # Identify individuals whose time of death is right censored
  id_censored <- as.vector(newdata[, death_end_col] == right_censoring_date)
  if (length(id_censored) > 0 & verbose) {
    message(glue(
      "The death of individuals {paste0(which(id_censored), collapse=', ')} is right censored. No \"unobserved\" last clutch considered."
    ))
  }
  newdata <- newdata |>
    mutate(
      death_end_censored = if_else(
        id_censored,
        NA,
        !!as.symbol(death_end_col)
      )
    )
  diff_death <- (newdata[, death_start_col] +
    newdata[, "death_end_censored"]) /
    2

  ## Add 2 columns for unobserved clutches before death
  diff_death <- diff_death |>
    as_tibble() |>
    mutate(last_clutch_interval = NA, last_clutch_interval2 = NA) |>
    relocate(
      last_clutch_interval,
      last_clutch_interval2,
      .before = everything()
    )

  ## Add maturity (matclutch=false) or first reproduction (matclutch=true) as first event
  clutch_time <- bind_cols(
    (newdata[, maturity_start_col] + newdata[, maturity_end_col]) / 2,
    (newdata[, start_cols] + newdata[, end_cols]) / 2,
    diff_death
  )

  # Add an unobserved clutch after all observed clutches,
  # which is right censored by age at death (i.e. the last column of the dataset).
  clutch_time[] <- t(apply(clutch_time, 1, function(row) {
    i <- which(is.na(row))[1]
    row[i] <- row[length(row)]
    row
  }))
  clutch_time <- clutch_time[, -ncol(clutch_time)]

  # Define newdata using with reproduction data in the same data format as "maturity" and "mortality" data
  newdata <- clutch_time |>
    t() |>
    diff() |> # transpose is required to diff() by columns instead of rows
    t() |>
    bind_cols(newdata |> select(lifelihoodData$covariates)) |>
    mutate(id = 1:nrow(clutch_time)) |>
    relocate(id) |>
    # We remove individuals that never reproduced (maturity right censored)
    filter(!(id %in% id_removed)) |>
    pivot_longer(
      cols = all_of(c(
        start_cols,
        "last_clutch_interval",
        "last_clutch_interval2"
      ))
    ) |>
    mutate(
      pon_end = if_else(
        # Specify the right censoring of the last unobserved clutch
        # of each individual which always followed by a NA reproduction
        # event unless the last clutch is observed (time of death is right censored)
        is.na(lead(value)) & !(id %in% which(id_censored)),
        right_censoring_date,
        value
      )
    ) |>
    filter(!is.na(value)) |>
    filter(value != 0) |> ## Remove data from individual whose last reproduction and death occured over the same time interval
    rename(pon_start = "value")

  return(newdata)
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
