#' @title Dataframe with default boundaries
#' @name create_default_boundaries
#' @param model_specs Vector of characters with the name of the statistical law to use. Must be of length 3 and each element must be in "wei", "exp", "gam" or "lgn". The first one is used for maturity, the second one is used for clutchs and the third one for death.
#' @param max_death Maximum value 
#' @export 
create_default_boundaries <- function(
   model_specs,
   max_death,
   max_clutch,
   max_maturity
) {
   models_bounds <- data.frame(
      name = c("wei", "gam", "lgn", "exp"),
      min_default = c(0.05, 0.005, 0.0025, 0.05),
      max_default = c(500, 600, 10, 1000)
   )

   maturity_model <- model_specs[1]
   maturity_specs <- subset(models_bounds, name == maturity_model)
   maturity_shape_min <- maturity_specs$min_default
   maturity_shape_max <- maturity_specs$max_default

   clutch_model <- model_specs[2]
   clutch_specs <- subset(models_bounds, name == clutch_model)
   clutch_shape_min <- clutch_specs$min_default
   clutch_shape_max <- clutch_specs$max_default

   death_model <- model_specs[3]
   death_specs <- subset(models_bounds, name == death_model)
   death_shape_min <- death_specs$min_default
   death_shape_max <- death_specs$max_default

   boundaries <- list(
      expt_death = c(name = "expt_death", min = 0.001, max = max_death),
      survival_shape = c(name = "survival_shape", min = death_shape_min, max = death_shape_max),
      ratio_expt_death = c(name = "ratio_expt_death", min = 0.01, max = 100),
      prob_death = c(name = "prob_death", min = 0 + 0.00001, max = 1 - 0.00001),
      sex_ratio = c(name = "sex_ratio", min = 0 + 0.00001, max = 1 - 0.00001),
      expt_maturity = c(name = "expt_maturity", min = 0.001, max = max_maturity),
      maturity_shape = c(name = "maturity_shape", min = maturity_shape_min, max = maturity_shape_max),
      ratio_expt_maturity = c(name = "ratio_expt_maturity", min = 0.01, max = 100),
      expt_reproduction = c(name = "expt_reproduction", min = 0.001, max = max_clutch),
      reproduction_shape = c(name = "reproduction_shape", min = clutch_shape_min, max = clutch_shape_max),
      pontn = c(name = "pontn", min = 1, max = 50),
      increase_death_hazard = c(name = "increase_death_hazard", min = 1e-05, max = 10),
      tof_reduction_date = c(name = "tof_reduction_date", min = 1e-07, max = 10),
      increase_tof_n_offspring = c(name = "increase_tof_n_offspring", min = 1e-07, max = 10),
      lin_decrease_hazard = c(name = "lin_decrease_hazard", min = -20, max = 20),
      quad_senescence = c(name = "quad_senescence", min = -20, max = 20),
      quad_decrease_hazard = c(name = "quad_decrease_hazard", min = -10, max = 10),
      quad_change_n_offspring = c(name = "quad_change_n_offspring", min = -10, max = 10),
      tof_n_offspring = c(name = "tof_n_offspring", min = -10, max = 10),
      fitness = c(name = "fitness", min = 0.001, max = 1000) # chnager max
   )
   boundaries_df <- data.frame(
      param = sapply(boundaries, function(x) x["name"]),
      min = sapply(boundaries, function(x) x["min"]),
      max = sapply(boundaries, function(x) x["max"]),
      row.names = NULL
   )
   return(boundaries_df)
}
