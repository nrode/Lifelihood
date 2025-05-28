get_lifelihoodResults <- function() {
  df <- lifelihood::fakesample |>
    dplyr::mutate(
      type = as.factor(type),
      geno = as.factor(geno)
    )

  dataLFH <- lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = c(
      "clutch_start1",
      "clutch_end1",
      "clutch_size1",
      "clutch_start2",
      "clutch_end2",
      "clutch_size2"
    ),
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("geno", "type"),
    model_specs = c("gam", "lgn", "wei")
  )

  results <- lifelihood(
    lifelihoodData = dataLFH,
    path_config = get_config_path("config"),
    seeds = c(1, 2, 3, 4),
    raise_estimation_warning = FALSE
  )

  return(results)
}
