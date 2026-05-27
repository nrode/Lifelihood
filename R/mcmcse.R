#' @keywords internal
lifelihood_mcmcse <- function(x, method = "bm", r = 3, size = NULL, g = NULL) {
  n <- nrow(x)
  p <- ncol(x)

  method <- match.arg(
    method,
    choices = c("bm", "obm", "bartlett", "tukey", "lug")
  )

  if (method == "lug") {
    method <- "bm"
    r <- 3
  }

  calls <- tryCatch(
    mcmcse::mcse.multi(
      x,
      method = method,
      r = r,
      size = size,
      g = g,
      adjust = TRUE,
      blather = FALSE
    ),
    error = function(e) {
      if (
        e$message ==
          "sample size is insufficient for a Markov chain of this dimension"
      ) {
        warning(
          "Could not compute MCMC standard errors. ",
          "Number of MCMC iterations (",
          n,
          ") ",
          "must be higher than ",
          "number of individuals (",
          p,
          ")."
        )
        return(NULL)
      }
    }
  )

  if (!is.null(calls)) {
    value <- list(
      "est" = calls$est,
      "se" = as.numeric(sqrt(diag(calls$cov) / n)),
      "vcov" = calls$cov
    )
  } else {
    value <- list(
      "est" = rep(NA, p),
      "se" = rep(NA, p),
      "vcov" = matrix(NA, nrow = p, ncol = p)
    )
  }

  value
}
