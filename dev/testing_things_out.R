rm(list = ls())
devtools::load_all(compile = FALSE)
df <- read.csv(here::here(
  "data/raw_data/DataPierrick/100%mort_Pierrick211genoparinteraction.csv"
))
df$geno <- as.factor(df$geno)
df$par <- as.factor(df$par)
df$spore <- as.factor(df$par)

clutchs <- c(
  "clutch_start1",
  "clutch_end1",
  "clutch_size1",
  "clutch_start2",
  "clutch_end2",
  "clutch_size2"
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
  covariates = c("par", "spore"),
  model_specs = c("wei", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = here::here("config_pierrick.yaml"),
  delete_temp_files = FALSE,
  MCMC = 3
)

AIC(results, "expt_death")
BIC(results, "expt_death")

# summary(results)
coef(results, "expt_death")
# logLik(results)
# results$effects
# results$mcmc
# results$vcov

newdata <- data.frame(
  par = c(0, 1, 2, 0, 1, 2),
  spore = c(0, 1, 2, 1, 0, 1)
)
newdata$par <- factor(newdata$par)
newdata$spore <- factor(newdata$spore)
predict(results, "expt_death", newdata, se.fit = FALSE)
predict(results, "expt_death", newdata, type = "response")

# fonction lifelihood()
mat <- matrix(c(as.numeric(df$par), as.numeric(df$spore)), ncol = 2)
mat <- model.matrix(~ par + spore, data = df)
z <- .Call(stats:::C_Cdqrls, mat, df$death_start, 10e-3, FALSE)
z$rank < ncol(mat)
colnames(mat)[z$coefficients == 0] # raise error: model is not identifiable. check effect1, effect2 are redundants with other effects in the model!!
data(iris)
iris$Sepal.Width2 <- iris$Sepal.Width
lm(Sepal.Length ~ Sepal.Width + Sepal.Width2, data = iris, singular.ok = FALSE)

plot_mortality_rate(
  dataLFH,
  interval_width = 15,
  max_time = 170,
  bygroup = FALSE,
  log_y = TRUE
)
plot_mortality_rate(
  dataLFH,
  interval_width = 25,
  max_time = 170,
  bygroup = TRUE,
  log_y = TRUE
)
plot_mortality_rate(
  lifelihoodResults = results,
  interval_width = 25,
  max_time = 170,
  log_y = TRUE,
  prediction = TRUE
)

mortality_rate(dataLFH, interval_width = 10)
mortality_rate(dataLFH, interval_width = 10, bygroup = FALSE, max_time = 170)

devtools::load_all(compile = FALSE)
pred_mortality_rate(results, interval_width = 10)


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


df <- read.csv(here::here("data/fake_sample.csv"))
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
  path_config = here::here("config.yaml"),
  param_bounds_df = bounds_df,
  raise_estimation_warning = FALSE
)
