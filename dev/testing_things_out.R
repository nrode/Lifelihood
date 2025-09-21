devtools::load_all()
library(tidyverse)


df <- fakesample |>
  head(12) |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  ) |>
  as_tibble()


df <- datapierrick |>
  as_tibble() |>
  mutate(
    par = as.numeric(par),
    geno = as.factor(geno),
    spore = as.factor(spore)
  )

generate_clutch_vector <- function(N) {
  return(paste(
    "pon",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)

lifelihoodData <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "geno", "spore"),
  model_specs = c("wei", "gam", "exp")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = get_config_path("config_pierrick"),
  delete_temp_files = FALSE,
  seeds = c(1, 2, 3, 4)
)

lifelihoodResults <- results

plot_observed_mortality_rate(
  lifelihoodData,
  interval_width = 15,
  groupby = c("par", "geno"),
  use_facet = TRUE
)

coef(results)
object <- results
prediction(
  results,
  parameter_name = "expt_death",
  type = "response",
  newdata = data.frame(par = 0, geno = 0, spore = 0)
)


prediction(results, parameter_name = "pontn")
prediction(results, parameter_name = "n_offspring")
prediction(results, parameter_name = "expt_death") |> head()
simulate_life_history(results) |> head()
simulate_life_history(results, event = "mortality") |> head()
simulate_life_history(results, event = "maturity") |> head()
simulate_life_history(results, event = "reproduction") |> head()


logLik(results)
AIC(results)
BIC(results)
results$effects
coeff(results, "expt_death")
coeff(results, "survival_shape")

vcov(results)
results$mcmc

newdata <- expand.grid(
  geno = levels(lifelihoodData$df$geno),
  par = levels(as.factor(lifelihoodData$df$par)),
  spore = levels(lifelihoodData$df$spore),
  time = seq(0, 170, by = 10)
)

rate_df <- compute_fitted_mortality_rate(
  lifelihoodResults = results,
  interval_width = 10,
  newdata = NULL,
  max_time = NULL,
  groupby = c("spore", "par")
)
rate_df[rate_df$Group=="spore=1.par=0",]
plot_fitted_mortality_rate(
  lifelihoodResults = results,
  interval_width = 10,
  newdata = NULL,
  max_time = NULL,
  xlab = "Time",
  ylab = "Mortality Rate",
  groupby = c("spore", "par"),
  use_facet = TRUE
)
df$par[df$spore==0]
### Old stuff
prediction(results, "expt_death", type = "response")
prediction(results, "survival_shape", type = "response")

t <- 0:10
dt <- 0.5
plot(
  t,
  IntX1toX2MortWei(
    t,
    dt,
    param1 = prediction(results, "expt_death", type = "response")[1],
    param2 = 1,
    law = NULL
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

newdata <- expand.grid(
  geno = levels(lifelihoodData$df$geno),
  par = levels(lifelihoodData$df$par |> as.factor())
)
compute_fitted_mortality_rate(
  results,
  interval_width = 15,
  newdata = newdata
)
plot_observed_mortality_rate(
  results,
  interval_width = 15,
  groupby = "all",
  use_facet = TRUE
)
plot_observed_mortality_rate(
  lifelihoodData,
  interval_width = 25,
  max_time = 170,
  groupby = c("par", "spore"),
  log_y = TRUE
)
plot_observed_mortality_rate(
  lifelihoodData,
  interval_width = 25,
  max_time = 170,
  groupby = "par",
  use_facet = TRUE,
  log_y = TRUE
)
plot_fitted_mortality_rate(
  results,
  interval_width = 5,
  groupby = "all",
  use_facet = TRUE
)


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
