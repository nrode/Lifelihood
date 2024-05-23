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
#' @name read_output_from_file()
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

read_output_from_file <- function(file_path, group_by_group = FALSE){

  # initialize results
  lines <- readLines(file_path)
  results <- list()
  
  # parse elements from .out file
  seeds <- parse(lines, "seeds", group_by_group)
  likelihood <- parse(lines, "likelihood", group_by_group)
  effects <- parse(lines, "effects", group_by_group)
  parameter_ranges <- parse(lines, "parameter_ranges", group_by_group)
  ratiomax <- parse(lines, "ratio_max", group_by_group)

  # store results
  results$datafile <- file_path
  results$seeds <- seeds
  results$likelihood <- likelihood
  results$effects <- effects
  results$parameter_ranges <- parameter_ranges
  results$ratiomax <- ratiomax
  
  return(results)
}


# use case
file = file.path(
  'data',
  'raw_data',
  'DataPierrick_GroupbyGroup',
  '100%mort_Pierrick211genoparinteraction.out'
)
read_output_from_file(file)
