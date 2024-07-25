##################################################################
# MINIMALIST USE CASE
##################################################################
rm(list = ls())
devtools::load_all() # load the package
df <- read.csv(here::here("data/fake_sample_numeric.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

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
results$effects

link <- function(estimate, min_and_max) {
   min <- min_and_max[1]
   max <- min_and_max[2]
   return(min + (max - min) / (1 + exp(-estimate)))
}

delink <- function(obs, min_and_max) {
   min <- min_and_max[1]
   max <- min_and_max[2]
   return(log((obs - min) / (max - obs)))
}

SurvWei <- function(t, ExpLong, Shape) {
   Scale <- ExpLong / gamma(1 + 1 / Shape)
   exp(-(t / Scale)^Shape)
}

make_design_matrix <- function(covariates, data) {
   
   # check if all covariate names are present in the data
   if (!all(covariates %in% names(data))) {
      stop("Some covariate names are not present in the data.")
   }

   # create a list of factor levels for each covariate
   factor_levels <- lapply(covariates, function(cov) {
      if (is.factor(data[[cov]])) {
         levels(data[[cov]])
      } else {
         unique(data[[cov]])
      }
   })
   factor_levels

   # create the formula
   names(factor_levels) <- covariates
   fitted_data <- do.call(expand.grid, factor_levels)
   formula_str <- paste("~", paste(covariates, collapse = " + "))
   formula <- as.formula(formula_str)

   # create design matrices
   mat_expt_death <- model.matrix(formula, fitted_data)
   mat_survival_shape <- model.matrix(formula, fitted_data)

   # return the results as a list
   return(list(
      fitted_data = fitted_data,
      mat_expt_death = mat_expt_death,
      mat_survival_shape = mat_survival_shape
   ))
}

pred_mortality_rate <- function(
   results_lifelihood,
   covariates,
   data,
   intervals = seq(0, 20, 5)
){

   # get the design matrices
   design_matrices <- make_design_matrix(covariates, df)
   fitted_data <- design_matrices$fitted_data
   mat_expt_death <- design_matrices$mat_expt_death
   mat_survival_shape <- design_matrices$mat_survival_shape

   # values per combination of factor
   expt_death <- results_lifelihood$effects$estimation[1:ncol(mat_expt_death)]
   survival_shape <- results_lifelihood$effects$estimation[(ncol(mat_expt_death) + 1):(ncol(mat_expt_death)+ncol(mat_survival_shape))]

   # fitted values per combination of factor
   fitted_data$fitted_expt_death <- mat_expt_death %*% expt_death
   fitted_data$fitted_survival_shape <- mat_survival_shape %*% survival_shape

   # apply link to get prediction of expected death
   fitted_data$predicted_expt_death <- sapply(
      fitted_data$fitted_expt_death,
      link,
      min_and_max=c(
         results_lifelihood$parameter_ranges$min[1],
         results_lifelihood$parameter_ranges$max[1]
      )
   )

   # apply link to get prediction of survival shape
   fitted_data$predicted_survival_shape <- sapply(
      fitted_data$fitted_survival_shape,
      link, 
      min_and_max = c(
         results_lifelihood$parameter_ranges$min[2],
         results_lifelihood$parameter_ranges$max[2]
      )
   )

   # compute mortality rate per interval
   pred_mort_rate <- (
      SurvWei(
         intervals[-length(intervals)],
         ExpLong = fitted_data$predicted_expt_death[1],
         Shape = fitted_data$predicted_survival_shape[1]
      ) -
      SurvWei(
         intervals[-1],
         ExpLong = fitted_data$predicted_expt_death[1],
         Shape = fitted_data$predicted_survival_shape[1]
      ))  /
      SurvWei(
         intervals[-length(intervals)],
         ExpLong = fitted_data$predicted_expt_death[1],
         Shape = fitted_data$predicted_survival_shape[1]
      )

   # create dataframe with interval and values
   pred_mort_rate_per_interval <- data.frame(
      mid_interval = (intervals[-length(intervals)] + intervals[-1]) / 2,
      pred_mort_rate = pred_mort_rate
   )
   return(pred_mort_rate_per_interval)
}

plot_mortality_rate <- function(
   results_lifelihood,
   covariates,
   data,
   intervals,
   use_log_x = FALSE,
   use_log_y = FALSE
){
   predicted_mortality_rate <- pred_mortality_rate(
      results_lifelihood = results_lifelihood,
      covariates = covariates,
      data = df,
      intervals = intervals
   )
   intervals <- predicted_mortality_rate$mid_interval
   mortality_rate <- predicted_mortality_rate$pred_mort_rate

   if (use_log_x){x_values <- log(intervals)}
   else {x_values <- intervals}
   
   if (use_log_y){y_values <- log(mortality_rate)}
   else {y_values <- mortality_rate}
   
   plot(x = intervals, y = mortality_rate, type = 'l')
}


plot_mortality_rate(
   results_lifelihood = results,
   data = df,
   covariates = c("geno", "type"),
   intervals = seq(0,100,5)
)
