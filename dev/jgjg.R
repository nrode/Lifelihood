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
  dist = c(mortality = "wei", maturity = "gam", reproduction = "lgn")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  config = list(
    mortality = list(
      expt_death = "par",
      survival_param2 = 1,
      ratio_expt_death = 1
    ),
    maturity = list(expt_maturity = 1, maturity_param2 = 1),
    reproduction = list(
      expt_reproduction = 1,
      reproduction_param2 = 1,
      n_offspring = 1
    )
  ),
  seeds = c(3699, 783, 5401, 6502),
  delete_temp_files = FALSE
)

###############
object <- results
event <- "all"
newdata <- NULL
use_censoring <- FALSE
visits <- NULL
seed <- NULL
