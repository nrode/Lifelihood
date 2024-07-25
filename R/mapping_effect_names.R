#' Get Mapping for Element
#'
#' @title Get Mapping for Element
#'
#' @keywords internal
#' @description This function takes an element as input and returns its corresponding mapping from a predefined list. If the element is not found in the mapping, it throws an error.
#' @param element A character string representing the element to be mapped.
#' @return A named list containing the mapped value for the input element.
#' @export
get_mapping <- function(element) {

   # create the mapping
   list1 <- c(
      "mortuf", "morta", "Rmortum", "mortp", "propmal", "matuf", "mata",
      "Rmatum", "pontu", "ponta", "(W)pontn", "to(ps)int", "to(ps)am",
      "to(ps)tp", "sen(pu)t", "sen(pu)t2", "sen(pn)t", "sen(pn)t2", "to(pupn)"
   )
   list2 <- c(
      "expt_death", "survival_shape", "ratio_expt_death", "prob_death",
      "sex_ratio", "expt_reproduction", "reproduction_shape", "n_offspring",
      "increase_death_hazard", "tof_reduction_date", "increase_tof_n_offspring",
      "lin_decrease_hazard", "quad_decrease_hazard", "lin_change_n_offspring",
      "quad_change_n_offspring", "tof_n_offspring", "expt_maturity",
      "maturity_shape", "ratio_expt_maturity"
   )
   mapping <- setNames(list2, list1)

   # try to find corresponding element
   if (element %in% names(mapping)) {
      return(mapping[element])
   } else {
      stop(paste("Element ('", element,"') not found in the mapping", sep=""))
   }
}