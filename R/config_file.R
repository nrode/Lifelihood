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
   est <- config$estimations
   formatted_config = c(
      paste("expt_death", R_to_lifelihood(est$mortality$expt_death, covariates)),
      paste("survival_shape", R_to_lifelihood(est$mortality$survival_shape, covariates)),
      paste("ratio_expt_death", R_to_lifelihood(est$mortality$ratio_expt_death, covariates)),
      paste("prob_death", R_to_lifelihood(est$mortality$prob_death, covariates)),
      paste("sex_ratio", R_to_lifelihood(est$mortality$sex_ratio, covariates)),
      paste("expt_maturity", R_to_lifelihood(est$maturity$expt_maturity, covariates)),
      paste("maturity_shape", R_to_lifelihood(est$maturity$maturity_shape, covariates)),
      paste("ratio_expt_maturity", R_to_lifelihood(est$maturity$ratio_expt_maturity, covariates)),
      paste("expt_reproduction", R_to_lifelihood(est$reproduction$expt_reproduction, covariates)),
      paste("reproduction_shape", R_to_lifelihood(est$reproduction$reproduction_shape, covariates)),
      paste("n_offspring", R_to_lifelihood(est$reproduction$n_offspring, covariates)),
      paste("increase_death_hazard", R_to_lifelihood(est$reproduction$increase_death_hazard, covariates)),
      paste("tof_reduction_date", R_to_lifelihood(est$reproduction$tof_reduction_date, covariates)),
      paste("increase_tof_n_offspring", R_to_lifelihood(est$reproduction$increase_tof_n_offspring, covariates)),
      paste("lin_decrease_hazard", R_to_lifelihood(est$reproduction$lin_decrease_hazard, covariates)),
      paste("quad_senescence", R_to_lifelihood(est$reproduction$quad_senescence, covariates)),
      paste("quad_decrease_hazard", R_to_lifelihood(est$reproduction$quad_decrease_hazard, covariates)),
      #paste("lin_change_n_offspring", R_to_lifelihood(est$reproduction$lin_change_n_offspring, covariates)),
      paste("quad_change_n_offspring", R_to_lifelihood(est$reproduction$quad_change_n_offspring, covariates)),
      paste("tof_n_offspring", R_to_lifelihood(est$reproduction$tof_n_offspring, covariates))
   )

   return(formatted_config)
}


#' @name R_to_lifelihood
#' @export
R_to_lifelihood <- function(R_format, covariates) {
   # ensure input is a string
   R_format <- as.character(R_format)
   

   if (R_format == "not_fitted") {
      return("-1")
   } else if (R_format == "1") {
      return("0")
   } else {
      # get a list of each covariable (separated by '+')
      used_covariables <- trimws(unlist(strsplit(R_format, split = "\\+")))

      # initiate a list of all covariables
      all_covariables <- c()

      # create a list of all covariables used
      for (cov in used_covariables) {
         if (grepl("*", cov, fixed = TRUE)) {
            interaction_covs <- trimws(unlist(strsplit(cov, split = "\\*")))
            all_covariables <- c(all_covariables, interaction_covs)
         } else {
            all_covariables <- c(all_covariables, cov)
         }
      }
      all_covariables <- unique(all_covariables)

      # ensure that all provided covariables are valid ones
      for (cov in all_covariables) {
         if (!(cov %in% covariates)) {
            stop("Unknow covariable: `", cov, "`")
         }
      }

      # create the lifelihood format output
      lifelihood_format <- "0"
      for (cov in used_covariables) {
         if (grepl("*", cov, fixed = TRUE)) {
            interaction_terms <- strsplit(cov, split = "\\*")
            first_term <- which(covariates == interaction_terms[[1]][1])
            second_term <- which(covariates == interaction_terms[[1]][2])
            position <- paste(first_term, second_term, sep = "")
         } else {
            position <- which(covariates == cov)
         }
         lifelihood_format <- paste(lifelihood_format, position)
      }
   }

   return(lifelihood_format)
}
