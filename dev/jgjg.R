devtools::load_all()
library(tidyverse)

df_female <- datadaphnia |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore)
  )

df_male <- df_female |>
  mutate(
    sex = 1,
    across(starts_with("clutch"), ~NA_real_),
    death_end = death_end * 10,
    death_start = death_start * 10
  )
df <- rbind(df_female, df_male) |>
  mutate(block = c(rep(1, nrow(df_female)), rep(2, nrow(df_male))))


generate_clutch_vector <- function(N) {
  return(paste(
    "clutch",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)

lifelihoodData <- as_lifelihoodData(
  df = df,
  matclutch = FALSE,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  block = "block",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  dist = c("wei", "gam", "lgn")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_pierrick"),
  seeds = c(3699, 783, 5401, 6502),
  delete_temp_files = FALSE
)

###############
object <- results
event <- "all"
newdata = NULL
use_censoring = FALSE
visits = NULL
seed = NULL
