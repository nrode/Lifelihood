#' @title Check if estimation are too close from boundaries.
#' @name check_valid_estimation
#' @keywords internal
#' @export
check_valid_estimation <- function(results_lifelihood) {
   estimations <- results_lifelihood$effects
   boundaries <- results_lifelihood$parameter_ranges
   metric_names <- unique(estimations$metric)
   for (metric_name in metric_names) {
      metric_values <- subset(estimations, metric == metric_name)
      metric_bounds <- subset(boundaries, name == metric_name)
      min_bound <- metric_bounds$min
      max_bound <- metric_bounds$max
      sum_estimations <- sum(metric_values$estimation)
      sum_estimations_linked <- link(sum_estimations, min_and_max = c(min_bound, max_bound))

      tolerance <- 0.005 # 0.5% tolerance
      min_threshold <- min_bound + (min_bound * tolerance)
      max_threshold <- max_bound - (max_bound * tolerance)
      if (sum_estimations_linked <= min_threshold) {
         warning(paste(
            "Estimation of '",
            metric_name,
            "' is close to the minimum bound: ",
            metric_name, "≃", sum_estimations_linked, ". ",
            "Consider decreasing minimum bound.",
            sep = ""
         ))
      } else if (sum_estimations_linked >= max_threshold) {
         warning(paste(
            "Estimation of '",
            metric_name,
            "' is close to the maximum bound: ",
            metric_name, "≃", sum_estimations_linked, ". ",
            "Consider increasing maximum bound.",
            sep = ""
         ))
      }
   }
}