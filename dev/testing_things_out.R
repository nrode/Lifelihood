



##################################################################
# MINIMALIST USE CASE
##################################################################
rm(list = ls())
devtools::load_all() # load the package
df <- read.csv(here::here("data/fake_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

n <- ifelse(nlevels(df$type) == 0, 1, nlevels(df$type))
nlevels(df$type)
str(df$type)

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
   models = c("wei", "lgn", "gam"),
   delete_temp_files = FALSE
)
summary(results)
results$likelihood
results$effects$estimation

fitted_data <- expand.grid(geno = factor(levels(df$geno)), type = factor(levels(df$type)))
mat_expt_death <- model.matrix(~ geno + type, fitted_data)
mat_survival_shape <- model.matrix(~ geno + type, fitted_data)

expt_death <- results$effects$estimation[1:ncol(mat_expt_death)]
survival_shape <- results$effects$estimation[(ncol(mat_expt_death) + 1):(ncol(mat_expt_death)+ncol(mat_survival_shape))]

fitted_data$fitted_expt_death <- mat_expt_death %*% expt_death
fitted_data$fitted_survival_shape <- mat_survival_shape %*% survival_shape

link <- function(estimate, min_max){
   return(min_max[1] + (min_max[2] - min_max[1])/(1+exp(-estimate)))
}

delink <- function(obs, min_max) {
   return(log((obs - min_max[1]) / (min_max[2] - obs)))
}
link(fitted_data$fitted_expt_death[1], min_max=c(results$parameter_ranges$min[1], results$parameter_ranges$max[1]))

fitted_data$predicted_expt_death <- sapply(fitted_data$fitted_expt_death, link, min_max=c(results$parameter_ranges$min[1], results$parameter_ranges$max[1]))
fitted_data$predicted_survival_shape <- sapply(fitted_data$fitted_survival_shape, link, min_max = c(results$parameter_ranges$min[2], results$parameter_ranges$max[2]))

results$parameter_ranges$min[1]
results$parameter_ranges$max[1]

SurvWei <- function(t, ExpLong, Shape) {
   Scale <- ExpLong / gamma(1 + 1 / Shape)
   exp(-(t / Scale)^Shape)
}

interv <- seq(0, 20, by=5)

pred_mort_rate <- (SurvWei(interv[-length(interv)], ExpLong = fitted_data$predicted_expt_death[1], Shape = fitted_data$predicted_survival_shape[1])
- SurvWei(interv[-1], ExpLong = fitted_data$predicted_expt_death[1], Shape = fitted_data$predicted_survival_shape[1]))/SurvWei(interv[-length(interv)], ExpLong = fitted_data$predicted_expt_death[1], Shape = fitted_data$predicted_survival_shape[1])

pred_mort_rate_per_interval <- data.frame(mid_interval = (interv[-length(interv)] + interv[-1]) / 2, pred_mort_rate = pred_mort_rate)

plot(x = log((interv[-length(interv)] + interv[-1]) / 2), y = log(pred_mort_rate), type='l')

results$effects$estimation
df$geno
df$type




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




############################
file <- yaml::yaml.load_file('config.yaml')
file$mortalitt
