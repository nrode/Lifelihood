library(here)
source(here('R', 'parsers.R'))

read_output_from_file <- function(file_path, group_by_group = FALSE){

  # test validity of input
  if (!file.exists(file_path)){stop("File not found")}
  if (!grepl(".out$", file_path)){stop("File is not a .out file")}
  if (file.size(file_path) == 0){stop("File is empty")}
  if (!is.logical(group_by_group)){stop("group_by_group must be a boolean")}

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

  class(results) <- "LifelihoodResults"
  return(results)
}

# define a custom summary method
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
  
  cat("Parameter Ranges:\n")
  print(object$parameter_ranges)
  cat("\n")
  
  cat("Ratio Max:\n")
  print(object$ratiomax)
  cat("\n")
}




# use case
# file_name <- "DataLenski_gam_gam_gam__Rep1"
# file <- here("data", "raw_data", "DataLenski", paste0(file_name, ".out"))
# results <- read_output_from_file(file)
# summary(results)
