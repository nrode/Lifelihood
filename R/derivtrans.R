#' @title Derivtrans function
#' @name derivtrans
#' @description Derivative of the link function used to transform the parameters from lifelihood scale to original scale.
#' @keywords internal
#' @param estimate Numeric. The estimate of the parameter
#' @param min The minimum value of the parameter
#' @param max The maximum value of the parameter
#' @return Numeric. Derivative of the link function at parameter estimate
#' @export
derivtrans <- function(estimate, min, max) {
  numerator <- (max - min) * exp(-estimate)
  denominator <- (1 + exp(-estimate))^2

  return(numerator / denominator)
}
