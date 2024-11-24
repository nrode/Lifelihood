#' @title Compute empirical mortality rate
#' @name mortality_rate
#' @description Calculate the empirical mortality rate over a given interval.
#' @inheritParams lifelihood
#' @param interval_width The interval width used to calculate the mortality rate. For instance, if the time unit for deaths in the original dataset is days and `interval_width` is set to 10, the mortality rate will be calculated every 10 days for each group.
#' @param max_time The maximum time for calculating the mortality rate. If set to NULL, the time of the last observed death is used.
#' @return A dataframe with 3 columns: Interval (time interval, based on `interval_width` value), Group (identifier of a given subgroup) and MortalityRate (mortality rate of this sub group at this time).
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
#' mort_df <- mortality_rate(dataLFH, interval_width = 5)
#' head(mort_df)
#' @export
mortality_rate <- function(
    lifelihoodData,
    interval_width,
    max_time = NULL) {
  data <- lifelihoodData$df
  start_col <- lifelihoodData$death_start
  end_col <- lifelihoodData$death_end
  covariates <- lifelihoodData$covariates

  if (is.null(max_time)) {
    max_time <- max(data[[end_col]], na.rm = TRUE)
  }

  n_intervals <- ceiling(max_time / interval_width)

  data$group <- do.call(interaction, data[covariates])
  groups <- sort(unique(data$group))

  mortality_rate <- matrix(0, nrow = n_intervals, ncol = length(groups))
  colnames(mortality_rate) <- groups
  rownames(mortality_rate) <- seq(interval_width, n_intervals * interval_width, by = interval_width)

  for (grp in groups) {
    group_data <- data[data$group == grp, ]

    for (i in 1:n_intervals) {
      interval_start <- (i - 1) * interval_width
      interval_end <- i * interval_width

      alive_start <- sum(group_data[[start_col]] >= interval_start | is.na(group_data[[start_col]]))

      deaths <- sum(group_data[[start_col]] >= interval_start &
        group_data[[start_col]] < interval_end &
        !is.na(group_data[[end_col]]))

      mortality_rate[i, grp] <- if (alive_start > 0) deaths / alive_start else NA
    }
  }

  mortality_rate_df <- reshape2::melt(mortality_rate)
  colnames(mortality_rate_df) <- c("Interval", "Group", "MortalityRate")

  mortality_rate_df$MortalityRate[is.na(mortality_rate_df$MortalityRate)] <- 1
  return(mortality_rate_df)
}
