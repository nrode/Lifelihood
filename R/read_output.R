#' Functions used to read the output file and create an object with results
#' 
#' @name read_output_from_file
#' @description takes the file path of the output file and read the results using parsers from [lifelihood::parse_output()]
#' @param file_path location of the output file from the programme
#' @param group_by_group boolean indicating whether the analysis should be made group by group or not (default to false)
#' @export 
read_output_from_file <- function(file_path, group_by_group = FALSE){

  # test validity of input
  if (!file.exists(file_path)){stop("File not found")}
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
  results$datafile <- file_path
  results$seeds <- seeds
  results$likelihood <- likelihood
  results$effects <- effects
  results$parameter_ranges <- parameter_ranges
  results$ratiomax <- ratiomax

  class(results) <- "LifelihoodResults"
  return(results)
}

#' @name summary
#' @title custom summary function for lifelihood
#' @description creates a custom summary method for the LifelihoodResults object
#' @param object `LifelihoodResults` object from [lifelihood::read_output_from_file()]
#' @export
summary.LifelihoodResults <- function(object, ...) {
  cat("LIFELIHOODIZATION\n\n")
  
  cat("Seeds:\n")
  print(object$seeds)
  cat("\n")
  
  cat("Likelihood optimum found:\n")
  print(object$likelihood)
  cat("\n")
  
  cat("Effects:\n")
  print(object$effects)
  cat("\n")
  
  cat("parameter ranges/boundaries:\n")
  print(object$parameter_ranges)
  cat("\n")
  
  cat("Ratio Max:\n")
  print(object$ratiomax)
  cat("\n")
}