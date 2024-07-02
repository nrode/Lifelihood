#' @name format_config
#' @title (internal function) Read the configuration file and format it for the input file.
#' @description TODO
#' @param path_config Location of the configuration file
#' @return String to add in the input file
#' @export
format_config <- function(path_config){
   
   # read the yaml file
   if (!file.exists(path_config)){stop(paste("Configuration file", path_config, "not found"))}
   config_file <- yaml::yaml.load_file(path_config)

   # get constant values (currently ignored)
   censoring <- config_file$constants$right_censoring_date
   critical_age <- config_file$constants$critical_age
   ratiomax <- config_file$constants$ratiomax

   # get mortality values

}
