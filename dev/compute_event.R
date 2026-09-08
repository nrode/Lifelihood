devtools::load_all()

df <- datadaphnia |>
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
#df <- rbind(df_female, df_male) |>
#  mutate(block = c(rep(1, nrow(df_female)), rep(2, nrow(df_male))))

# name of the columns of the clutchs into a single vector
clutchs <- generate_clutch_vector(28)
dataLFH <- as_lifelihoodData(
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
  dist = c(mortality = "wei", maturity = "gam", reproduction = "lgn")
)
results <- lifelihood(
  lifelihoodData = dataLFH,
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
  seeds = c(1, 2, 3, 4)
)

compute_observed_event_rate(
  dataLFH,
  interval_width = 5,
  event = "mortality",
  max_time = 150,
  groupby = c("par", "sex")
) |>
  as_tibble()

compute_fitted_event_rate(
  lifelihoodResults = results,
  interval_width = 5,
  event = c("mortality"),
  max_time = 150,
  groupby = c("par")
) |>
  as_tibble()


plot_fitted_event_rate(
  results,
  interval_width = 5,
  event = "mortality",
  max_time = 150,
  groupby = c("par"),
  add_observed_event_rate = TRUE,
  use_facet = TRUE
)

p <- fitted_emergence_rate |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Mean_Interval,
      y = Event_Rate,
      color = par,
      shape = par
    )
  ) +
  geom_point() +
  geom_line(linewidth = 0.5) +
  xlab("Time (days)") +
  ylab("Fitted mortality rate over 5 day-periods") +
  facet_wrap(vars(sex, par))
p


p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

# Use vars() to supply faceting variables:
p + facet_wrap(vars(class), labeller = "label_both")
