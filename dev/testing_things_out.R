rm(list = ls())
devtools::load_all(compile = FALSE) # load the package
df <- read.csv(here::here("data/fake_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)
data <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "mor_start",
  death_end = "mor_end",
  covariates = c("geno", "type"),
  model_specs = c("exp", "lgn", "wei")
)
results <- lifelihood(
  lifelihoodData = data,
  path_config = here::here("config2.yaml")
)
summary(results)

delink(3.1, min_and_max = c(0.001, 40))
delink(12.17, min_and_max = c(0.001, 40))


library(survival)
# m <- lm((df$mor_start + df$mor_end) / 2 ~ df$geno)
df <- read.csv(here::here("data/fake_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)
df$event <- rep(1, nrow(df))
m <- survreg(Surv(
  time = mor_start,
  time2 = mor_end,
  event = event,
  type = "interval"
) ~ geno, dist = "exponential", data = df)

summary(m)
exp(m$coefficients)


delink(6.5, min_and_max = c(0.001, 40))

link(results$effects$estimation, min_and_max = c(0.001, 40))
default_bounds_df(data)


plot_mortality_rate(
  results_lifelihood = results,
  newdata = df,
  covariates = c("geno", "type"),
  intervals = seq(0, 20, 5),
  use_log_x = TRUE,
  use_log_y = TRUE
)

stats::predict.glm
