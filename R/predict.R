#' @name predict
#' @title prediction
predict.LifelihoodResults <- function(
    lifelihoodResults,
    newdata = NULL,
    type = c("link", "response"),
    se.fit = FALSE) {
  if (!inherits(lifelihoodResults, "lifelihoodResults")) {
    stop("lifelihoodResults must be of class lifelihoodResults")
  }

  type <- match.arg(type)
  data <- ifelse(is.null(newdata), lifelihoodResults$lifelihoodData, newdata)
}
