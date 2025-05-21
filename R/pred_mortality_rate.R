#' @title Compute predicted mortality rate
#'
#' @description
#' Calculate the predicted mortality rate over a given interval.
#'
#' @param lifelihoodResults output of [lifelihood()]
#' @param bygroup TODO
#' @inheritParams prediction
#' @inheritParams lifelihood
#' @inheritParams lifelihoodData
#' @inheritParams mortality_rate
#'
#' @return A dataframe with 3 columns: Interval (time interval, based on `interval_width` value), Group (identifier of a given subgroup, or "Overall" if bygroup = FALSE), and MortalityRate (mortality rate at this time).
#'
#' @export
pred_mortality_rate <- function(
  lifelihoodResults,
  interval_width,
  newdata = NULL,
  max_time = NULL,
  bygroup = TRUE
) {
  lifelihoodData <- lifelihoodResults$lifelihoodData
  data <- if (is.null(newdata)) lifelihoodData$df else newdata
  data$pred_death <- predict(
    lifelihoodResults,
    "expt_death",
    type = "response",
    newdata = data
  )
  end_col <- "pred_death"
  covariates <- lifelihoodData$covariates

  if (is.null(max_time)) {
    max_time <- max(data[[end_col]], na.rm = TRUE)
  }

  n_intervals <- ceiling(max_time / interval_width)

  if (bygroup) {
    data$group <- do.call(interaction, data[covariates])
    groups <- sort(unique(data$group))
  } else {
    data$group <- "Overall"
    groups <- "Overall"
  }

  result <- list()
  for (grp in groups) {
    group_data <- data[data$group == grp, ]
    rates <- numeric(n_intervals)

    for (i in seq_len(n_intervals)) {
      interval_start <- (i - 1) * interval_width
      interval_end <- i * interval_width

      at_risk <- sum(group_data[[end_col]] > interval_start, na.rm = TRUE)
      events <- sum(
        group_data[[end_col]] >= interval_start &
          group_data[[end_col]] < interval_end,
        na.rm = TRUE
      )

      rates[i] <- if (at_risk > 0) events / at_risk else NA
    }

    result[[grp]] <- data.frame(
      Interval = seq(
        interval_width,
        n_intervals * interval_width,
        by = interval_width
      ),
      Group = grp,
      MortalityRate = rates
    )
  }

  pred_mortality_rate_df <- do.call(rbind, result)
  pred_mortality_rate_df$MortalityRate[is.na(
    pred_mortality_rate_df$MortalityRate
  )] <- 1

  if (!bygroup) {
    pred_mortality_rate_df <- subset(pred_mortality_rate_df, select = -c(Group))
  }
  rownames(pred_mortality_rate_df) <- NULL
  return(pred_mortality_rate_df)
}
