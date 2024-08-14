#' @title Read and format the output file of the program
#' @description Takes the file path of the output file and read the results using parsers from [lifelihood::parse_output()].
#' @keywords internal
#' 
#' @name read_output_from_file
#' @param file_path Location of the output file of the program
#' @param group_by_group Boolean indicating whether parsing should be performed group by group or not (`FALSE` by default). This argument is necessary because the structure of the output file is different depending on whether the analysis was carried out "group by group" or not (the analysis method used will then be different, for certain parsers).#' @return An object of class `LifelihoodResults` with all results from the output file
#' @export 
read_output_from_file <- function(file_path, group_by_group = FALSE){

  # test validity of input
  if (!file.exists(file_path)){stop(paste("File", file_path  ,"not found"))}
  if (!grepl(".out$", file_path)){stop("File is not a .out file")}
  if (file.size(file_path) == 0){stop("File is empty")}
  if (!is.logical(group_by_group)){stop("group_by_group must be a boolean")}

  # initialize results
  lines <- readLines(file_path)
  results <- list()

  # parse_output elements from .out file
  seeds <- parse_output(lines, "seeds", group_by_group)
  likelihood <- parse_output(lines, "likelihood", group_by_group)
  effects <- parse_output(lines, "effects", group_by_group)
  parameter_ranges <- parse_output(lines, "parameter_ranges", group_by_group)
  ratiomax <- parse_output(lines, "ratio_max", group_by_group)

  # store results
  results$seeds <- seeds
  results$likelihood <- likelihood
  results$effects <- effects
  results$effects$metric <- sapply(results$effects$name, map_metric_name)
  results$effects$kind <- sapply(results$effects$name, find_parameter_kind)
  results$parameter_ranges <- parameter_ranges
  results$ratiomax <- ratiomax
  results$group_by_group <- group_by_group

  class(results) <- "LifelihoodResults"
  return(results)
}