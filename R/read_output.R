#' @title Read and format the output file of the program
#' @description Takes the file path of the output file and read the results using parsers from [lifelihood::parse_output()].
#' @keywords internal
#' @name read_output_from_file
#' @param file_path Location of the output file of the program
#' @inheritParams lifelihood
#' @return An object of class `LifelihoodResults` with all results from the output file
#' @export 
read_output_from_file <- function(
  file_path,
  group_by_group = FALSE,
  covariates = NULL
){

  lines <- readLines(file_path)
  results <- list()

  seeds <- parse_output(lines, "seeds", group_by_group)
  likelihood <- parse_output(lines, "likelihood", group_by_group)
  effects <- parse_output(lines, "effects", group_by_group)
  parameter_ranges <- parse_output(lines, "parameter_ranges", group_by_group)
  ratiomax <- parse_output(lines, "ratio_max", group_by_group)

  results$seeds <- seeds
  results$likelihood <- likelihood
  results$effects <- effects
  results$effects$metric <- sapply(results$effects$name, map_metric_name)
  results$effects$kind <- sapply(results$effects$name, find_parameter_kind)
  results$parameter_ranges <- parameter_ranges
  results$ratiomax <- ratiomax
  results$group_by_group <- group_by_group
  results$covariates <- covariates
  class(results) <- "LifelihoodResults"
  return(results)
}