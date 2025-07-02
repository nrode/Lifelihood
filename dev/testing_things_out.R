devtools::load_all()
library(tidyverse)

set.seed(123)
n <- 1000
longevity <- 10
slope <- 1
toto <- rep(1:2, each = n / 2)
t1 <- rexp(n = n, rate = 1 / (longevity * toto))
tapply(t1, toto, mean)


# df <- fakesample |>
#   head(12) |>
#   mutate(
#     geno = geno_l,
#     type = as.factor(type)
#   ) |>
#   as_tibble()

df <- datapierrick |>
  as_tibble() |>
  mutate(
    geno = as.factor(geno),
    par = as.factor(par)
  )

dataLifelihood <- data.frame(geno_l = toto - 1, t1 = t1 - 0.5, t2 = t1 + 0.5) |>
  as_tibble() |>
  dplyr::mutate(
    sex_start = dplyr::if_else(t1 <= 1, 0.0001, t1 - 1),
    sex_end = dplyr::if_else(t2 <= 1, 0.0001, t2 - 1),
    sex = 0
  ) |>
  dplyr::mutate(
    mat_start = dplyr::if_else(t1 <= 1, 0.0001, t1 - 1),
    mat_end = dplyr::if_else(t2 <= 1, 0.0001, t2 - 1)
  ) |>
  dplyr::mutate(clutch_start1 = NA, clutch_end1 = NA, clutch_size1 = NA) |>
  dplyr::mutate(
    t1 = dplyr::if_else(t1 <= 0, 0.0001, t1),
    t2 = dplyr::if_else(t2 <= 0, 0.0001, t2)
  ) |>
  dplyr::rename(death_start = t1, death_end = t2) |>
  dplyr::relocate(death_start, .after = last_col()) |>
  dplyr::relocate(death_end, .after = last_col()) |>
  mutate(geno_l = as.factor(geno_l), toto = as.factor(geno_l))

lifelihoodData <- lifelihoodData(
  df = dataLifelihood,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = c(
    "clutch_start1",
    "clutch_end1",
    "clutch_size1"
  ),
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("geno_l", "toto"),
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
