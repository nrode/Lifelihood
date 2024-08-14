#' Get Mapping for Element
#'
#' @title Get Mapping for Element
#'
#' @keywords internal
#' @name map_metric_name
#' @description This function takes an element as input and returns its corresponding mapping from a predefined list. If the element is not found in the mapping, it throws an error.
#' @param element A character string representing the element to be mapped.
#' @return A named list containing the mapped value for the input element.
#' @export
map_metric_name <- function(name) {
   keys <- c(
      "survival_shape", "ratio_expt_death", "prob_death", "sex_ratio",
      "reproduction_shape", "increase_death_hazard", "pontn",
      "tof_reduction_date", "increase_tof_n_offspring", "lin_decrease_hazard",
      "quad_decrease_hazard", "lin_change_n_offspring", "quad_change_n_offspring",
      "tof_n_offspring", "maturity_shape", "ratio_expt_maturity",
      "quad_senescence", "expt_reproduction", "expt_maturity","expt_death", "n_offspring", "fitness"
   )
   for (key in keys) {
      if (grepl(key, name)) {
         return(key)
      }
   }
   stop(paste("Impossible to find matching metric for:", name))
}


#' Get Mapping for Element
#'
#' @title Get Mapping for Element
#'
#' @keywords internal
#' @name find_parameter_kind
#' @description This function takes an element as input and returns its corresponding mapping from a predefined list. If the element is not found in the mapping, it throws an error.
#' @param element A character string representing the element to be mapped.
#' @return A named list containing the mapped value for the input element.
#' @export
find_parameter_kind <- function(name) {
   if (startsWith(name, "int_")) {
      return("intercept")
   } else if (startsWith(name, "eff_")) {
      return("coefficient")
   } else {
      stop(paste("Impossible to find parameter kind for:", name))
   }
}

