#' @keywords internal
lifelihood_mcmcse <- function(x, method = "bm", r = 3, size = NULL, g = NULL) {
  n <- nrow(x)
  p <- ncol(x)

  if (n < (p + 1)) {
    stop(
      "The number of MCMC iterations (",
      n,
      ") should be higher than the number of parameters + 1 (",
      p + 1,
      ")."
    )
  }

  method <- match.arg(
    method,
    choices = c("bm", "obm", "bartlett", "tukey", "lug")
  )

  if (method == "lug") {
    method <- "bm"
    r <- 3
  }

  calls <- mcmcse::mcse.multi(
    x,
    method = method,
    r = r,
    size = size,
    g = g,
    adjust = TRUE,
    blather = FALSE
  )

  se <- as.numeric(sqrt(diag(calls$cov) / n))
  value <- list("est" = calls$est, "se" = se, "vcov" = calls$cov)

  value
}
