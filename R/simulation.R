simulate_weibull <- function(results) {
  n <- nrow(results$lifelihoodData$df)
  expected_death <- prediction(results, "expt_death", type = "response") |>
    mean()
  shape <- prediction(results, "survival_shape", type = "response") |> mean()

  # expected longevity = scale * gamma(1 + 1 / shape)
  scale <- expected_death / gamma(1 + 1 / shape)

  return(rweibull(n = n, shape = shape, scale = scale))
}

simulate_gamma <- function(results) {
  n <- nrow(results$lifelihoodData$df)
  expected_death <- prediction(results, "expt_death", type = "response") |>
    mean()
  scale <- prediction(results, "survival_shape", type = "response") |> mean()

  # expected longevity = shape * scale
  shape <- expected_death / scale

  return(rgamma(n = n, shape = shape, scale = scale))
}

simulate_lognormal <- function(results) {
  n <- nrow(results$lifelihoodData$df)
  expected_death <- prediction(results, "expt_death", type = "response") |>
    mean()
  vp1 <- prediction(results, "survival_shape", type = "response") |> mean()

  mu <- log(expected_death) - 0.5 * log(1 + vp1 / (expected_death^2))
  sigma <- sqrt(log(1 + vp1 / (expected_death^2)))

  return(rlnorm(n = n, meanlog = mu, sdlog = sigma))
}
