#' @title Display change in predicted event rate with age
#'
#' @description
#' Useful function for creating a good-quality line graph
#' of changes in predicted event rate with age.
#'
#' If you want more control over the style of the graph,
#' use the [compute_fitted_event_rate()] function to directly retrieve the predicted data.
#'
#' @name plot_event_rate
#'
#' @inheritParams lifelihood
#' @inheritParams plot_event_rate
#' @inheritParams prediction
#' @inheritParams validate_groupby_arg
#' @param add_observed_mortality_rate Boolean to add the observed event rate to the graph (default=TRUE)
#' @param min_sample_size The minimum number of individuals alive
#' at the beggining of a time interval for computing the observed event rate (only used if add_observed_mortality_rate=TRUE, default=1)
#'
#' @details This function requires [ggplot2](https://ggplot2.tidyverse.org/) to be installed.
#'
#' @return a ggplot2 plot
#'
#' @export
plot_fitted_event_rate <- function(
  lifelihoodResults,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  add_observed_mortality_rate = TRUE,
  min_sample_size = 1,
  max_time = NULL,
  groupby = NULL,
  use_facet = FALSE,
  xlab = "Time",
  ylab = "Event Rate"
) {
  groupby <- validate_groupby_arg(lifelihoodResults$lifelihoodData, groupby)

  rate_df <- compute_fitted_event_rate(
    lifelihoodResults = lifelihoodResults,
    interval_width = interval_width,
    event = event,
    newdata = newdata,
    max_time = max_time,
    groupby = groupby
  )

  pfitted <- plot_event_rate(
    rate_df = rate_df,
    max_time = max_time,
    type = "lines",
    groupby = groupby,
    use_facet = use_facet,
    xlab = xlab,
    ylab = ylab
  )

  if (add_observed_mortality_rate) {
    obs_rate_df <- compute_observed_event_rate(
      lifelihoodData = lifelihoodResults$lifelihoodData,
      interval_width = interval_width,
      event = event,
      max_time = max_time,
      groupby = groupby,
      min_sample_size = min_sample_size
    )
    if (!is.null(groupby)) {
      pfitted <- pfitted +
        geom_point(
          data = obs_rate_df,
          aes(x = Mean_Interval, y = Event_Rate, color = group)
        )
    } else {
      pfitted <- pfitted +
        geom_point(obs_rate_df, aes(x = Mean_Interval, y = MortalityRate))
    }
  }
  pfitted
}

#' @title Display evolution of empirical mortality rate
#'
#' @description
#' Useful function for creating a good-quality line graph
#' of changes in the empirical mortality rate.
#'
#' You don't need to fit with [lifelihood()] to use this
#' function, only to retrieve a lifelihood data object
#' with [lifelihoodData()]
#'
#' If you want more control over the style of the graph,
#' use the [compute_observed_event_rate()] function to retrieve data.
#'
#' @name plot_event_rate
#'
#' @inheritParams lifelihood
#' @inheritParams compute_observed_event_rate
#' @inheritParams plot_event_rate
#' @inheritParams validate_groupby_arg
#'
#' @details This function requires [ggplot2](https://ggplot2.tidyverse.org/) to be installed.
#'
#' @return a ggplot2 plot
#'
#' @export
plot_observed_event_rate <- function(
  lifelihoodData,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL,
  use_facet = FALSE,
  xlab = "Time",
  ylab = "Mortality Rate"
) {
  groupby <- validate_groupby_arg(lifelihoodData, groupby)

  rate_df <- compute_observed_event_rate(
    lifelihoodData,
    interval_width,
    event = event,
    max_time = max_time,
    groupby = groupby,
    min_sample_size = min_sample_size
  )

  pobs <- plot_event_rate(
    rate_df = rate_df,
    max_time = max_time,
    groupby = groupby,
    use_facet = use_facet,
    xlab = xlab,
    ylab = ylab
  )
  pobs
}

#' @title Plot event rate
#'
#' @description
#' Convenient function used in [plot_observed_event_rate()]
#' and [plot_fitted_event_rate()].
#'
#' @inheritParams compute_observed_event_rate
#' @inheritParams validate_groupby_arg
#' @param rate_df Dataframe with event rate
#' @param type The type of symbol to be used for the plot (either of "points" or 'lines")
#' @param use_facet Use facet_wrap to plot one panel per group (default=FALSE)
#' @param groupby Factor(s) whosse levels over which event rate should be represented (default=NULL)
#'
#' @return a ggplot2 plot
#'
#' @importFrom ggplot2 ggplot aes labs theme_minimal facet_wrap ylim
#'
#' @keywords internal
plot_event_rate <- function(
  rate_df,
  max_time,
  type = c("points", "lines"),
  groupby,
  use_facet,
  xlab = "Time",
  ylab = "Event rate"
) {
  type <- match.arg(type)

  if (!is.null(groupby)) {
    p <- ggplot2::ggplot(
      rate_df,
      ggplot2::aes(
        x = Mean_Interval,
        y = Event_Rate,
        color = group
      )
    )
  } else {
    p <- ggplot2::ggplot(
      rate_df,
      ggplot2::aes(
        x = Mean_Interval,
        y = Event_Rate
      )
    )
  }

  if (type == "points") {
    p <- p + geom_point()
  } else if (type == "lines") {
    p <- p + geom_line()
  }

  plot <- p +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    ylim(0, 1) +
    ggplot2::theme_minimal()

  if (!is.null(max_time)) {
    plot <- plot + ggplot2::xlim(0, max_time * 1.1)
  }

  if (use_facet) {
    plot <- plot +
      ggh4x::facet_nested(as.formula(paste(
        "~",
        paste(groupby, collapse = " + ")
      )))
  }

  return(plot)
}
