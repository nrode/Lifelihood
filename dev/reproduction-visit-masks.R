devtools::load_all()
library(tidyverse)

# Visit masks are computed per block: individuals in the same block share the
# same set of visit dates. Here we use `geno` as the block.
df <- datapierrick |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore),
    block = 1
  )

clutchs <- generate_clutch_vector(28)

lifelihoodData <- as_lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  matclutch = FALSE,
  covariates = c("par", "geno"),
  block = "block", # <- enables per-block visit masks
  dist = c("wei", "gam", "lgn")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_pierrick"),
  raise_estimation_warning = FALSE,
  delete_temp_files = FALSE
)

prediction(results, "expt_death", type = "response") |>
  as_tibble() |>
  bind_cols(df) |>
  distinct(geno, par, value)

# `visits` holds the observed visit dates per block, derived from the fitted
# data. The same seed makes the two simulations draw identical latent clutch
# ages, so the only difference between them is the visit masking.
visits <- get_visits(lifelihoodData)
visits

sim_raw <- simulate_life_history(
  results,
  event = "reproduction",
  use_censoring = TRUE,
  visits = data.frame(block = c(1), visit = c(1:13, 14, 19, 19:200)),
  seed = 1
)

sim_cens <- simulate_life_history(
  results,
  event = "reproduction",
  use_censoring = TRUE,
  visits = visits,
  seed = 1
)

sim_cens |> head(10)

# Pull one individual's clutch slots out of a simulation, dropping empty slots,
# so the raw and censored versions can be read side by side.
row_vec <- function(d, i, prefix) {
  cols <- grep(paste0("^", prefix, "[0-9]+$"), names(d), value = TRUE)
  unname(unlist(d[i, cols]))
}


show_individual <- function(i) {
  block_i <- as.character(sim_raw$geno[i])
  cat("\nvisit dates for this block:\n")
  print(sort(visits$visit[visits$geno == block_i]))
  cat("\nRAW latent clutches (no censoring):\n")
  print(
    tibble(
      age = row_vec(sim_raw, i, "clutch_start_"),
      size = row_vec(sim_raw, i, "clutch_size_")
    ) |>
      filter(!is.na(age))
  )
  cat(
    "\nCENSORED clutches (age wrapped in visit bounds, same-interval merged):\n"
  )
  print(
    tibble(
      age = row_vec(sim_cens, i, "clutch_"),
      start = row_vec(sim_cens, i, "clutch_start_"),
      end = row_vec(sim_cens, i, "clutch_end_"),
      size = row_vec(sim_cens, i, "clutch_size_")
    ) |>
      filter(!is.na(age))
  )
  cat("\ntotal_n_offspring:", sim_cens$total_n_offspring[i], "\n")
}

# How many clutches each individual has before vs after censoring. Wherever the
# censored count is smaller, two or more latent clutches fell between the same
# pair of visits and were merged. This is just to find interesting rows to look
# at, not a check.
counts <- tibble(
  i = seq_len(nrow(sim_cens)),
  geno = sim_cens$geno,
  sex = sim_cens$sex,
  n_raw = rowSums(!is.na(sim_raw |> select(matches("^clutch_start_[0-9]+$")))),
  n_censored = rowSums(
    !is.na(sim_cens |> select(matches("^clutch_start_[0-9]+$")))
  )
) |>
  mutate(n_merged_away = n_raw - n_censored)
print(counts, n = 20)

# A female whose clutches got merged the most: the clearest demonstration.
counts |>
  arrange(desc(n_merged_away)) |>
  slice(1) |>
  pull(i) |>
  show_individual()

# A female with no merging (every clutch landed in its own interval).
counts |>
  filter(sex == 0, n_merged_away == 0, n_censored > 0) |>
  slice(1) |>
  pull(i) |>
  show_individual()

# A male: reproduction columns are all NA, total_n_offspring stays NA.
counts |> filter(sex == 1) |> slice(1) |> pull(i) |> show_individual()
