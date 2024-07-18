



##################################################################
# MINIMALIST USE CASE
##################################################################
rm(list = ls())
devtools::load_all() # load the package
df <- read.csv(here::here("data/fake_sample.csv"))
clutchs <- c(
   "clutch_start1", "clutch_end1", "clutch_size1",
   "clutch_start2", "clutch_end2", "clutch_size2"
)
results <- lifelihood(
   df = df,
   path_config = "config.yaml",
   sex = "sex",
   sex_start = "sex_start",
   sex_end = "sex_end",
   maturity_start = "mat_start",
   maturity_end = "mat_end",
   clutchs = clutchs,
   death_start = "mor_start",
   death_end = "mor_end",
   covariates = c("geno", "type"),
   matclutch = FALSE,
   seeds = c(11, 22, 33, 44),
   models = c("gam", "lgn", "wei")
)
summary(results)
results$likelihood
results$effects$estimation





##################################################################
# VISUALIZATION 
##################################################################
rm(list = ls())
devtools::load_all() # load the package
df <- read.csv(here::here("data/fake_re_sample.csv"))
length <- dim(df)[1]
df$type <- sample(0:2, length, replace = TRUE)
df$geno <- sample(0:1, length, replace = TRUE)
plot_mortality_rate(
   df,
   start_col = "mor_start",
   end_col = "mor_end",
   covariates = c("type", "geno"),
   interval_width = 1
)
