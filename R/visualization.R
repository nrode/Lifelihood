#' @title Display evolution of mortality rate
#' @name plot_mortality_rate_emp
#' @return none
#' @export
plot_mortality_rate_emp <- function(
   data,
   start_col,
   end_col,
   covariates,
   interval_width = 2,
   max_time = NULL
) {

   mortality_rate_df <- compute_mortality_rate(
      data,
      start_col = start_col,
      end_col = end_col,
      covariates = covariates,
      interval_width = interval_width
   )
   ggplot2::ggplot(mortality_rate_df, ggplot2::aes(x = as.numeric(as.character(Interval)), y = MortalityRate, color = Group)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = "Mortality Rate Over Time", x = "Time Interval", y = "Mortality Rate") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = 'none')
}


#' @title Compute empirical mortality rate
#' @name compute_mortality_rate
#' @description Compute the empirical mortality rate
#' @param data Dataframe with the data of life history. It should have one row per life history / observation.
#' @param start_col Column name containing the first date of the interval in which the event was determined.
#' @param end_col Column name containing the second date of the interval in which the event was determined.
#' @param covariates Vector containing the names of the covariates.
#' @param interval_width Width of the interval to compute the mortality rate.
#' @param max_time Maximum time to compute the mortality rate.
#' @return none
#' @export
compute_mortality_rate <- function(
   data,
   start_col,
   end_col,
   covariates,
   interval_width = 2,
   max_time = NULL
){
   
   # Input validation
   missing_cols <- setdiff(c(start_col, end_col, covariates), names(data))
   if (length(missing_cols) > 0) {
      print(names(data))
      stop(paste(
         "The following specified column(s) not found in the data:",
         paste(missing_cols, collapse = ", ")
      ))
   }

   # Determine maximum time if not provided
   if (is.null(max_time)) {
      max_time <- max(data[[end_col]], na.rm = TRUE)
   }

   # Calculate number of intervals
   n_intervals <- ceiling(max_time / interval_width)

   # Create grouping variable
   data$group <- do.call(interaction, data[covariates])
   groups <- sort(unique(data$group))

   # Initialize mortality rate matrix
   mortality_rate <- matrix(0, nrow = n_intervals, ncol = length(groups))
   colnames(mortality_rate) <- groups
   rownames(mortality_rate) <- seq(interval_width, n_intervals * interval_width, by = interval_width)

   # Compute mortality rates
   for (grp in groups) {
      group_data <- data[data$group == grp, ]

      for (i in 1:n_intervals) {
         interval_start <- (i - 1) * interval_width
         interval_end <- i * interval_width

         # Count alive at interval start
         alive_start <- sum(group_data[[start_col]] >= interval_start | is.na(group_data[[start_col]]))

         # Count deaths during interval
         deaths <- sum(group_data[[start_col]] >= interval_start &
            group_data[[start_col]] < interval_end &
            !is.na(group_data[[end_col]]))

         # Calculate mortality rate
         mortality_rate[i, grp] <- if (alive_start > 0) deaths / alive_start else NA
      }
   }

   mortality_rate_df <- reshape2::melt(mortality_rate)
   colnames(mortality_rate_df) <- c("Interval", "Group", "MortalityRate")

   return(mortality_rate_df)
}