#' @title Display evolution of empirical mortality rate
#' @description Useful function for creating a good-quality line graph of changes in the empirical mortality rate.
#' @name plot_emp_mortality_rate
#' @inheritParams lifelihood
#' @inheritParams mortality_rate
#' @param log_x Determine whether the x-axis should be displayed on a logarithmic scale.
#' @param log_y Determine whether the y-axis should be displayed on a logarithmic scale.
#' @details This function requires [ggplot2](https://ggplot2.tidyverse.org/) to be installed.
#' @return none
#' @examples
#' df <- read.csv(here::here("data/fake_sample.csv"))
#' df$type <- as.factor(df$type)
#' df$geno <- as.factor(df$geno)
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
#'   death_start = "mor_start",
#'   death_end = "mor_end",
#'   covariates = c("geno", "type"),
#'   model_specs = c("gam", "lgn", "wei")
#' )
#'
#' plot_emp_mortality_rate(dataLFH, interval_width = 2)
#' @export
plot_emp_mortality_rate <- function(
    lifelihoodData,
    interval_width,
    min_time = 0,
    max_time = NULL,
    log_x = FALSE,
    log_y = FALSE) {
  mortality_rate_df <- mortality_rate(
    lifelihoodData,
    interval_width = interval_width,
    max_time = max_time
  )

  plot <- ggplot2::ggplot(mortality_rate_df, ggplot2::aes(x = as.numeric(as.character(Interval)), y = MortalityRate, color = Group)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Mortality Rate Over Time", x = "Time", y = "Mortality Rate") +
    ggplot2::theme_minimal()

  if (log_x) {
    plot <- plot + ggplot2::scale_x_log10()
  }
  if (log_y) {
    plot <- plot + ggplot2::scale_y_log10()
  }

  if (!is.null(max_time)) {
    plot <- plot + ggplot2::xlim(min_time, max_time)
  }

  plot
}
