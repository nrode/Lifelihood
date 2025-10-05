#' @title Fit multiple times lifelihood
#'
#' @description
#' This function fits [`lifelihood()`] `n_fit` times and return
#' the fit with the best likelihood.
#'
#' @param ... Arguments passed to [`lifelihood()`]
#' @param n_fit Number of times to fit. Default to 3.
#'
#' @import glue
#'
#' @return `lifelihoodResults`
lifelihood_optim <- function(..., n_fit = 3) {
  dots <- list(...)

  # we force generate seeds here because it would not make sense
  # to use n times the same seeds.
  if ("seeds" %in% names(dots)) {
    stop("Can't set `seeds` when using `lifelihood::lifelihood_optim()`")
  }

  all_results <- list()
  for (i in 1:n_fit) {
    seeds <- sample(1:10000, 4, replace = T)

    temp_dir <- file.path(
      here::here(),
      paste0(paste0("lifelihood_", paste(seeds, collapse = "_")))
    )

    results <- lifelihood::lifelihood(..., seeds = seeds, temp_dir = temp_dir)
    all_results[[glue::glue("lifelihood_fit_{i}")]] <- results
  }

  likelihoods <- sapply(all_results, function(x) x$likelihood)
  ord <- order(likelihoods, decreasing = TRUE)

  best_fit <- all_results[[ord[1]]]

  # we want to make sure that we couldn't easily find a better
  # solution (https://github.com/nrode/Lifelihood/issues/111)
  if (length(likelihoods) > 1) {
    diff_best <- likelihoods[ord[1]] - likelihoods[ord[2]]
    if (diff_best > 0.1) {
      warning(glue::glue(
        "Best and second-best likelihoods differ by {round(diff_best, 3)} (> 0.1). ",
        "Consider increasing n_fit (currently {n_fit}) for more stable optimization."
      ))
    }
  }

  return(best_fit)
}
