#' @title Link function
#' @name link
#' @description Link function to transform the parameters from the lifelihood scale to the original scale
#' @keywords internal
#' @param estimate Numeric. The estimate of the parameter
#' @param min_and_max Numeric vector of length 2. The minimum and maximum values of the parameter
#' @return Numeric. The transformed parameter
#' @export
link <- function(estimate, min, max) {
  return(min + (max - min) / (1 + exp(-estimate)))
}

#' @title Delink function
#' @name delink
#' @description Delink function to transform the parameters from original scale to lifelihood scale.
#' @keywords internal
#' @param obs Numeric. The observed value of the parameter
#' @param min_and_max Numeric vector of length 2. The minimum and maximum values of the parameter
#' @return Numeric. The transformed parameter
#' @export
delink <- function(obs, min, max) {
  return(log((obs - min) / (max - obs)))
}

#' @title derivLink function
#' @name derivLink
#' @description Derivative of the link function used to transform the parameters from lifelihood scale to original scale.
#' @keywords internal
#' @param estimate Numeric. The estimate of the parameter
#' @param min The minimum value of the parameter
#' @param max The maximum value of the parameter
#' @return Numeric. Derivative of the link function at parameter estimate
#' @export
derivLink <- function(estimate, min, max) {
  numerator <- (max - min) * exp(-estimate)
  denominator <- (1 + exp(-estimate))^2

  return(numerator / denominator)
}
