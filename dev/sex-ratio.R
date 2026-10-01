devtools::load_all()
library(dplyr)

df <- datapierrick |>
  mutate(par = as.factor(par), spore = as.factor(spore))

dataLFH <- as_lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = generate_clutch_vector(28),
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  matclutch = FALSE,
  dist = data.frame(mortality = "wei", maturity = "gam", reproduction = "lgn")
)

config_without <- list(
  mortality = list(
    expt_death = 1,
    survival_param2 = 1,
    ratio_expt_death = 1,
    sex_ratio = "not_fitted"
  ),
  maturity = list(
    expt_maturity = 1,
    maturity_param2 = 1,
    ratio_expt_maturity = 1
  ),
  reproduction = list(
    expt_reproduction = 1,
    reproduction_param2 = 1,
    n_offspring = 1
  )
)
config_with <- config_without
config_with$mortality$sex_ratio <- 1

# Use the same, more thorough annealing settings for both fits to reduce the
# chance that a poor local solution dominates the comparison.
results_without <- lifelihood(
  dataLFH,
  config = config_without,
  seeds = c(11, 12, 13, 14),
  ntr = 10,
  nst = 10,
  raise_estimation_warning = FALSE
)
results_with <- lifelihood(
  dataLFH,
  config = config_with,
  seeds = c(11, 12, 13, 14),
  ntr = 10,
  nst = 10,
  raise_estimation_warning = FALSE
)

male_probability <- prediction(results_with, "sex_ratio", type = "response")
cat("\nObserved male proportion and fitted probability of being male:\n")
print(tibble(
  observed_male_proportion = mean(df$sex == 1),
  fitted_male_probability = mean(male_probability)
))

cat("\nPredicted mortality and maturity means from each fit:\n")
print(
  bind_rows(
    "Without sex_ratio" = prediction(
      results_without,
      c("expt_death", "expt_maturity"),
      type = "response"
    ) |>
      bind_cols(df["sex"]),
    "With sex_ratio" = prediction(
      results_with,
      c("expt_death", "expt_maturity"),
      type = "response"
    ) |>
      bind_cols(df["sex"]),
    .id = "model"
  ) |>
    distinct()
)

# Without fitting sex_ratio, input sex is copied. With it fitted, every sex is
# newly drawn, including individuals whose input sex was already known.
# Equal seeds reproduce each simulation, but the added sex draws shift later
# event draws. Refitting can also change other parameter estimates, so a row's
# event differences cannot all be attributed solely to its changed sex.
sim_without <- simulate_life_history(results_without, seed = 123)
sim_with <- simulate_life_history(results_with, seed = 123)

cat("\nSex composition and number of reassigned individuals:\n")
print(tibble(
  model = c("Without sex_ratio", "With sex_ratio"),
  male_proportion = c(mean(sim_without$sex == 1), mean(sim_with$sex == 1)),
  changed_sex = c(sum(sim_without$sex != df$sex), sum(sim_with$sex != df$sex))
))

simulations <- bind_rows(
  "Without sex_ratio" = sim_without,
  "With sex_ratio" = sim_with,
  .id = "model"
)
cat("\nLife-history summaries by simulated sex (0 = female, 1 = male):\n")
print(
  simulations |>
    group_by(model, sex) |>
    summarise(
      n = n(),
      mean_death_age = mean(mortality_start),
      proportion_matured = mean(maturity_start < mortality_start),
      mean_maturity_age = mean(
        maturity_start[maturity_start < mortality_start],
        na.rm = TRUE
      ),
      proportion_with_clutches = mean(coalesce(total_n_clutches, 0) > 0),
      mean_clutches_per_individual = mean(coalesce(total_n_clutches, 0)),
      mean_offspring_per_individual = mean(coalesce(total_n_offspring, 0)),
      .groups = "drop"
    ),
  width = Inf
)

# Males and individuals with no observed clutches have NA reproduction totals.
# For these population averages, count their contribution as zero. Raw totals
# below retain NA, preserving the distinction used by simulate_life_history().
cat("\nPopulation-level reproductive output:\n")
print(
  simulations |>
    group_by(model) |>
    summarise(
      total_clutches = sum(total_n_clutches, na.rm = TRUE),
      total_offspring = sum(total_n_offspring, na.rm = TRUE),
      mean_offspring_per_individual = mean(coalesce(total_n_offspring, 0)),
      .groups = "drop"
    )
)

cat("\nExamples of changed sex and the corresponding reproduction totals:\n")
print(
  tibble(
    individual = seq_len(nrow(df)),
    input_sex = df$sex,
    simulated_sex = sim_with$sex,
    clutches_without = sim_without$total_n_clutches,
    clutches_with = sim_with$total_n_clutches,
    offspring_without = sim_without$total_n_offspring,
    offspring_with = sim_with$total_n_offspring
  ) |>
    filter(input_sex != simulated_sex) |>
    group_by(input_sex, simulated_sex) |>
    slice_head(n = 3) |>
    ungroup(),
  width = Inf
)
