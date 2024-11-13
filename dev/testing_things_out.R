rm(list = ls())
devtools::load_all(compile = FALSE)
df <- read.csv(here::here("data/fake_re_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)
dataLFH <- lifelihoodData(
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
  model_specs = c("wei", "lgn", "wei")
)
results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = here::here("config2.yaml")
)

head(predict(results, "expt_death", type = "response"))
newdata <- data.frame(
  type = c(1, 2, 0, 1, 2, 0),
  geno = c(0, 1, 0, 1, 0, 1)
)
newdata$type <- factor(newdata$type)
newdata$geno <- factor(newdata$geno)
predict(results, "expt_death", newdata)
predict(results, "expt_death", newdata, type = "response")

default_bounds_df(dataLFH)

results$covariates



devtools::dev_package_deps("../Lifelihood")







df$geno
# aller chercher dnas results les formulas associées à chaque paramètre
# Predict de expt_death sur échelle lifelihood
m <- model.frame(~ geno * type, data = df)
Terms <- terms(m)
predicted <- model.matrix(Terms, m) %*% results$effects$estimation[1:6] # on prend les 6 premiers car ils concernent geno
pred_expdeath <- link(predicted, min = 0.001, max = 40) # original scale
# equivalent predict survival

# aller chercher dnas results les formulas associées à chaque paramètre
# Predict de survival_shape sur échelle lifelihood
# m <- model.frame(~ geno * type, data = df)
# Terms <- terms(m)
# predicted <- model.matrix(Terms, m) %*% results$effects$estimation[3:8] # on prend les 3 à 8 car ils concernent geno + type
# pred_survivalshape <- link(predicted, min_and_max = c(0.05, 500)) # original scale



library(survival)
# m <- lm((df$mor_start + df$mor_end) / 2 ~ df$geno)
df <- read.csv(here::here("data/fake_re_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)
df$event <- rep(1, nrow(df))
df$geno_type <- as.factor(paste(df$geno, df$type, sep = "_"))
m <- survreg(Surv(
  time = mor_start,
  time2 = mor_end,
  event = event,
  type = "interval"
) ~ geno * type, dist = "weibull", data = df)
predict(m)

summary(m)
exp(m$coefficients)
survival::predict(m)

survival::predict.survreg

predict.lm



# si niveau de référence alors link de intercept,
# sinon intercept
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




































df <- read.csv(here::here("data/fake_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)
head(df)

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

dataLFH <- lifelihoodData(
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
  model_specs = c("gam", "lgn", "wei")
)

bounds_df <- default_bounds_df(dataLFH)
head(bounds_df)

# for example, we want to change this value
bounds_df[bounds_df$name == "increase_death_hazard", "max"] <- 80

# then we pass it to lifelihood()
results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = here::here("config.yaml"),
  param_bounds_df = bounds_df,
  raise_estimation_warning = FALSE
)
