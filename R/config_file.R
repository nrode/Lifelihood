#' @name format_config
#' @title (internal function) Read the configuration file and format it for the input file.
#' @description Use the configuration file to create the well formatted vector of strings that will be put under "***modele***" in the input file, excluding the distribution law.
#' @param path_config Location of the configuration file
#' @param covariates Vector containing the names of the covariates
#' @return Vector of string to add in the input file
#' @export
format_config <- function(path_config, covariates){
   
   # read the yaml file
   if (!file.exists(path_config)){stop(paste("Configuration file", path_config, "not found"))}
   config <- yaml::yaml.load_file(path_config)

   # get constant values (currently ignored)
   censoring <- config$constants$right_censoring_date
   critical_age <- config$constants$critical_age
   ratiomax <- config$constants$ratiomax

   # get mortality, maturity and reproduction config
   formatted_config = c(
      paste("expt_death", config$mortality$expt_death),
      paste("survival_shape", config$mortality$survival_shape),
      paste("ratio_expt_death", config$mortality$ratio_expt_death),
      paste("prob_death", config$mortality$prob_death),
      paste("sex_ratio", config$mortality$sex_ratio),
      paste("expt_maturity", config$maturity$expt_maturity),
      paste("maturity_shape", config$maturity$maturity_shape),
      paste("ratio_expt_maturity", config$maturity$ratio_expt_maturity),
      paste("expt_reproduction", config$reproduction$expt_reproduction),
      paste("reproduction_shape", config$reproduction$reproduction_shape),
      paste("n_offspring", config$reproduction$n_offspring),
      paste("increase_death_hazard", config$reproduction$increase_death_hazard),
      paste("tof_reduction_date", config$reproduction$tof_reduction_date),
      paste("increase_tof_n_offspring", config$reproduction$increase_tof_n_offspring),
      paste("lin_decrease_hazard", config$reproduction$lin_decrease_hazard),
      paste("quad_senescence", config$reproduction$quad_senescence),
      paste("quad_decrease_hazard", config$reproduction$quad_decrease_hazard),
      paste("lin_change_n_offspring", config$reproduction$lin_change_n_offspring),
      paste("quad_change_n_offspring", config$reproduction$quad_change_n_offspring),
      paste("tof_n_offspring", config$reproduction$tof_n_offspring),
   )

   return(formatted_config)
}


#' @name R_to_lifelihood
#' @export
R_to_lifelihood <- function(R_format, covariates){
   
   # ensure input is a string
   R_format <- as.character(R_format)
   
   if (R_format == 'not_fitted'){
      lifelihood_format <- '-1'
   } else if (R_format == '1'){
      lifelihood_format <- '0'
   }
   
   return(lifelihood_format)
}
