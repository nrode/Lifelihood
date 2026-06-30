devtools::load_all()

library(tibble)

generate_clutch_vector <- function(n) {
  paste(
    "clutch",
    rep(c("start", "end", "size"), n),
    rep(seq_len(n), each = 3),
    sep = "_"
  )
}

make_lifelihood_data <- function() {
  df <- datadaphnia |>
    as_tibble() |>
    dplyr::mutate(
      par = as.factor(par),
      spore = as.factor(spore)
    )

  as_lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = generate_clutch_vector(28),
    death_start = "death_start",
    death_end = "death_end",
    matclutch = FALSE,
    covariates = c("par", "spore"),
    dist = c("wei", "gam", "lgn")
  )
}

run_lifelihood_case <- function(title, param_bounds_df, seeds) {
  temp_dir <- file.path(
    getwd(),
    paste0("lifelihood_", paste(seeds, collapse = "_"))
  )

  lifelihood(
    lifelihoodData = lifelihood_data,
    path_config = use_test_config("config_pierrick"),
    param_bounds_df = param_bounds_df,
    seeds = seeds,
    delete_temp_files = FALSE,
    raise_estimation_warning = FALSE
  )
}

lifelihood_data <- make_lifelihood_data()
valid_bounds <- default_bounds_df(lifelihood_data)

bad_numeric_bounds <- valid_bounds
bad_numeric_bounds$min[bad_numeric_bounds$param == "survival_param2"] <-
  "not-a-number"

missing_bounds_value <- valid_bounds
missing_bounds_value$max[missing_bounds_value$param == "expt_death"] <- ""


run_lifelihood_case(
  param_bounds_df = bad_numeric_bounds,
  seeds = c(9101, 9102, 9103, 9104)
)

run_lifelihood_case(
  param_bounds_df = missing_bounds_value,
  seeds = c(9201, 9202, 9203, 9204)
)
