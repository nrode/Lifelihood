#' Read the output of the Lifelihood program
#' @description Read the output of the Lifelihood program
#' @param file_path Path to the output file
#' @return A list with the following elements:
#' \itemize{
#'  \item \code{datafile} Path to the output file
#' \item \code{seeds} A vector of four integers representing the seeds used in the simulation
#' \item \code{likelihood_max} The maximum likelihood value
#' \item \code{effects} A data frame with the following columns:
#' \itemize{
#' \item \code{Name} The name of the effect
#' \item \code{Estimate} The estimated value of the effect
#' \item \code{StdError} The standard error of the estimate
#' }
#' \item \code{parameter_ranges} A data frame with the following columns:
#' \itemize{
#' \item \code{Name} The name of the parameter
#' \item \code{Min} The minimum value of the parameter
#' \item \code{Max} The maximum value of the parameter
#' }
#' @name read_output_from_file
#'
#' @export
#' 
#' @examples
#' # path to input
#' file_path = file.path(
#'   'data',
#'   'raw_data',
#'   'DataPierrick_GroupbyGroup',
#'   '100%mort_Pierrick211genoparinteraction.out'
#')
#'
#' # read the output and print the results
#' fitted = read_output_from_file(file_path)
#' fitted$datafile
#' fitted$seeds
#' fitted$likelihood_max
#' fitted$effects
#' fitted$parameter_ranges

source(file.path('R', 'parsers.R'))

read_output_from_file <- function(
  file_path,
  group_by_group = FALSE
) {

  # initialize results
  lines <- readLines(file_path)
  results <- list()
  seeds <- get_seeds(lines=lines, group_by_group=group_by_group)
  
  # parser when group by group is not selected
  if (!group_by_group){

    # get likelihood
    likelihood_line <- lines[grepl("Likelihood_max=", lines)]
    results$likelihood <- as.numeric(sub("Likelihood_max=\\s*(-?\\d+\\.\\d+)", "\\1", likelihood_line))

    # get number of parameters (format: #parameters= 84)
    n_parameters_line <- lines[grepl("#parameters", lines)]
    n_parameters <- as.numeric(sub("#parameters=\\s*(\\d+)", "\\1", n_parameters_line))
    results$n_parameters <- n_parameters

    # get effects
    effect_lines <- lines[grepl("^eff_", lines)]
    effects <- strsplit(effect_lines, " ")
    results$effects <- data.frame(
      Name = as.character(sapply(effects, function(x) x[1])),
      Estimate = as.numeric(sapply(effects, function(x) x[2])),
      # TODO (Jo): verify with Nicolas and Thomas that this is actually the standard error
      StdError = as.numeric(sapply(effects, function(x) x[3]))
    )

    # get parameter ranges
    start <- which(grepl("Parameter_Range_Table", lines)) # find the first match
    end <- which(grepl("ratiomax", lines)) # find the end match
    range_lines <- lines[(start+1):(end-1)]
    range_values <- strsplit(range_lines, " ")
    results$parameter_ranges <- data.frame(
      Name = as.character(sapply(range_values, function(x) x[1])),
      Min = as.numeric(sapply(range_values, function(x) x[2])),
      Max = as.numeric(sapply(range_values, function(x) x[3]))
    )

    # get ratiomax
    ratiomax_line <- lines[end]
    results$ratiomax <- as.numeric(gsub("[^0-9]", "", ratiomax_line))

    return(results)
  }

  # parser when group by group is selected
  else {
     
  }
  
}



# use case
file_path = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   '100%mort_Pierrick211genoparinteraction.out'
)
fitted = read_output_from_file(file_path)
fitted$datafile
fitted$seeds
fitted$likelihood
fitted$n_parameters
fitted$effects
fitted$parameter_ranges
fitted$ratiomax
