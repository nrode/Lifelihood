#' @title Compute and visualize predicted mortality rate
#' @name pred_mortality_rate
#' @description Compute the predicted mortality rate using the fitted values of the expected death and the survival shape.
#' @param newdata Dataframe with the values of the covariates to predict the mortality rate
#' @param intervals Vector of the intervals (x-axis, same unit as the data) to compute the mortality rate
#' @inheritParams check_valid_estimation
#' @export
pred_mortality_rate <- function(
    results_lifelihood,
    newdata,
    intervals) {
  if (!inherits(results, "LifelihoodResults")) {
    stop("Error: 'results_lifelihood' must be of class 'LifelihoodResults'.")
  }

  results_lifelihood <- results
  newdata <- df
  # get the design matrices
  covariates <- results_lifelihood$covariates
  design_matrices <- make_design_matrix(covariates, newdata)
  fitted_data <- design_matrices$fitted_data
  mat_expt_death <- design_matrices$mat_expt_death
  mat_survival_shape <- design_matrices$mat_survival_shape

  # values per combination of factor
  expt_death <- results_lifelihood$effects$estimation[1:ncol(mat_expt_death)]
  survival_shape <- results_lifelihood$effects$estimation[(ncol(mat_expt_death) + 1):(ncol(mat_expt_death) + ncol(mat_survival_shape))]

  # fitted values per combination of factor
  fitted_data$fitted_expt_death <- mat_expt_death %*% expt_death
  fitted_data$fitted_survival_shape <- mat_survival_shape %*% survival_shape

  # apply link to get prediction of expected death
  fitted_data$predicted_expt_death <- sapply(
    fitted_data$fitted_expt_death,
    link,
    min_and_max = c(
      results_lifelihood$parameter_ranges$min[1],
      results_lifelihood$parameter_ranges$max[1]
    )
  )

  # apply link to get prediction of survival shape
  fitted_data$predicted_survival_shape <- sapply(
    fitted_data$fitted_survival_shape,
    link,
    min_and_max = c(
      results_lifelihood$parameter_ranges$min[2],
      results_lifelihood$parameter_ranges$max[2]
    )
  )

  # compute mortality rate per interval
  pred_mort_rate <- (
    SurvWei(
      intervals[-length(intervals)],
      ExpLong = fitted_data$predicted_expt_death[1],
      Shape = fitted_data$predicted_survival_shape[1]
    ) -
      SurvWei(
        intervals[-1],
        ExpLong = fitted_data$predicted_expt_death[1],
        Shape = fitted_data$predicted_survival_shape[1]
      )) /
    SurvWei(
      intervals[-length(intervals)],
      ExpLong = fitted_data$predicted_expt_death[1],
      Shape = fitted_data$predicted_survival_shape[1]
    )

  # create dataframe with interval and values
  pred_mort_rate_per_interval <- data.frame(
    mid_interval = (intervals[-length(intervals)] + intervals[-1]) / 2,
    pred_mort_rate = pred_mort_rate
  )
  return(pred_mort_rate_per_interval)
}

#' @title Compute and visualize predicted mortality rate
#' @name plot_mortality_rate
#' @description Simple plot of the predicted mortality rate. To access the values, use the output of [pred_mortality_rate()].
#' @param use_log_x Boolean to use log scale for the x-axis (default: FALSE)
#' @param use_log_y Boolean to use log scale for the y-axis (default: FALSE)
#' @inheritParams pred_mortality_rate
#' @export
plot_mortality_rate <- function(
    results_lifelihood,
    newdata,
    intervals,
    use_log_x = FALSE,
    use_log_y = FALSE) {
  covariates <- results_lifelihood$covariates
  predicted_mortality_rate <- pred_mortality_rate(
    results_lifelihood = results_lifelihood,
    covariates = covariates,
    newdata = newdata,
    intervals = intervals
  )
  intervals <- predicted_mortality_rate$mid_interval
  mortality_rate <- predicted_mortality_rate$pred_mort_rate

  if (use_log_x) {
    x_values <- log(intervals)
  } else {
    x_values <- intervals
  }

  if (use_log_y) {
    y_values <- log(mortality_rate)
  } else {
    y_values <- mortality_rate
  }

  plot(x = x_values, y = y_values, type = "pch")
}
