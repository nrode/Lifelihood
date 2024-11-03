#' @title Associate the parameter name with the associated metric
#' @keywords internal
#' @name map_metric_name
#' @description This function takes a name as input and returns its match from a predefined list. If the element is not found in the match, an error is generated. This function is used to determine which metric a parameter estimate is based on.
#' @param name A character string representing the element to be mapped.
#' @examples
#' map_metric_name("eff_expt_death_geno1")
#' map_metric_name("eff_ratio_expt_death_geno1")
#' map_metric_name("int_sex_ratio")
#' @return A named list containing the mapped value for the input element.
#' @export
map_metric_name <- function(name) {
  keys <- c(
    "survival_shape", "ratio_expt_death", "prob_death", "sex_ratio",
    "reproduction_shape", "increase_death_hazard", "pontn",
    "tof_reduction_date", "increase_tof_n_offspring", "lin_decrease_hazard",
    "quad_decrease_hazard", "lin_change_n_offspring", "quad_change_n_offspring",
    "tof_n_offspring", "maturity_shape", "ratio_expt_maturity",
    "quad_senescence", "expt_reproduction", "expt_maturity", "expt_death", "n_offspring", "fitness"
  )
  for (key in keys) {
    if (grepl(key, name)) {
      return(key)
    }
  }
  stop(paste("Impossible to find matching metric for:", name))
}


#' @title Deducting the type of parameter from an estimate
#' @keywords internal
#' @name find_parameter_kind
#' @description This function takes an estimate name as input and returns whether it is a intercept or a coefficient/slope. If the element is not found in the possible match, it returns an error. This function is used to add information about the type of estimate to the output of the [lifelihood()] function.
#' @param name A character string representing the parameter name.
#' @examples
#' find_parameter_kind("eff_expt_death_geno1")
#' find_parameter_kind("eff_ratio_expt_death_geno1")
#' find_parameter_kind("int_sex_ratio")
#' @return The kind of parameter: either intercept or coefficient/slope
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
