devtools::load_all()
library(tidyverse)


df <- fakesample |>
  head(12) |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  ) |>
  as_tibble()

lifelihoodData <- lifelihoodData(
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
  model_specs = c("wei", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = get_config_path("config2"),
  delete_temp_files = FALSE,
  seeds = c(1, 2, 3, 4)
)
coef(results)

AIC(results)
BIC(results)

coeff(results, "expt_death")
coeff(results, "survival_shape")
logLik(results)
vcov(results)
results$effects
results$mcmc

newdata <- expand.grid(
  geno_l = levels(dataLifelihood$geno_l),
  toto = levels(dataLifelihood$toto)
)
prediction(results, "expt_death", newdata, type = "response")
prediction(results, "survival_shape", newdata, type = "response")

t <- 0:10
dt <- 0.5
plot(
  t,
  IntX1toX2MortWei(
    t,
    dt,
    ExpLong = prediction(results, "expt_death", newdata, type = "response")[1],
    Shape = 1
  )
)

# fonction lifelihood()
mat <- matrix(c(as.numeric(df$par), as.numeric(df$spore)), ncol = 2)
mat <- model.matrix(~ par + spore, data = df)
z <- .Call(stats:::C_Cdqrls, mat, df$death_start, 10e-3, FALSE)
z$rank < ncol(mat)
colnames(mat)[z$coefficients == 0] # raise error: model is not identifiable. check effect1, effect2 are redundants with other effects in the model!!
data(iris)
iris$Sepal.Width2 <- iris$Sepal.Width
lm(Sepal.Length ~ Sepal.Width + Sepal.Width2, data = iris, singular.ok = FALSE)

plot_observed_mortality_rate(
  dataLFH,
  interval_width = 15,
  max_time = 170,
  log_y = TRUE
)
plot_observed_mortality_rate(
  dataLFH,
  interval_width = 25,
  max_time = 170,
  groupby = c("par", "spore"),
  log_y = TRUE
)
plot_fitted_mortality_rate(
  results,
  interval_width = 5,
  log_y = TRUE,
  groupby = "all"
)

pred_mortality_rate(results, interval_width = 15)


df <- read.csv(here::here("data/fake_re_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)
df$event <- rep(1, nrow(df))
df$geno_type <- as.factor(paste(df$geno, df$type, sep = "_"))
m <- survival::survreg(
  Surv(
    time = death_start,
    time2 = death_end,
    event = event,
    type = "interval"
  ) ~
    geno * type,
  dist = "weibull",
  data = df
)
predict(m)

summary(m)
exp(m$coefficients)
survival::predict(m)


df <- fakesample
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)
head(df)

clutchs <- c(
  "clutch_start1",
  "clutch_end1",
  "clutch_size1",
  "clutch_start2",
  "clutch_end2",
  "clutch_size2"
)

dataLFH <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

bounds_df <- default_bounds_df(dataLFH)
head(bounds_df)

# for example, we want to change this value
bounds_df[bounds_df$name == "increase_death_hazard", "max"] <- 80

# then we pass it to lifelihood()
results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  param_bounds_df = bounds_df,
  raise_estimation_warning = FALSE
)
