#' Data Parsing Functions for Model Output Files
#'
#' These functions parse specific elements from model output files, including seeds, likelihood values, parameter ranges, effects, and ratio maximum values. The functions can handle group-by-group parsing where applicable.
#'
#' In practice, the `parse()` function is used to call the specific parsing functions based on the element to be parsed. It is then used in the `read_output_from_file()` function to extract the relevant information from the output file.
#' 
#' @param lines A character vector where each element represents a line from the output file.
#' @param group_by_group A logical indicating whether to parse the data in a group-by-group manner. Default is FALSE.
#'
#' @return Depending on the function:
#' - `get_seeds`: Returns a numeric vector of seeds or a matrix of seeds if `group_by_group` is TRUE.
#' - `get_likelihood`: Returns a numeric value of the likelihood or a matrix of likelihood values if `group_by_group` is TRUE.
#' - `get_param_ranges`: Returns a data frame with parameter names and their respective min and max values.
#' - `get_effects`: Returns a data frame with effect names, estimates, and standard errors.
#' - `get_ratio_max`: Returns a numeric value of the ratio maximum.
#'
#' @examples
#'
#' lines <- c(
#'   "seed1= 123 seed2= 456 seed3= 789 seed4= 101",
#'   "Likelihood_max= -123.456",
#'   "Parameter_Range_Table",
#'   "param1 0 10",
#'   "param2 5 15",
#'   "ratiomax 20",
#'   "eff_param1 0.5 0.1",
#'   "eff_param2 0.8 0.2"
# )
#'
#' seeds <- parse(lines, "seeds")
#' likelihood <- parse(lines, "likelihood")
#' param_ranges <- parse(lines, "parameter_ranges")
#' effects <- parse(lines, "effects")
#' ratio_max <- parse(lines, "ratio_max")
#' 
#' @name parsers



library(glue)


get_seeds = function(lines, group_by_group=FALSE){

   # find the line starting with pattern "seed1="
   seeds_line <- lines[grepl("seed1=", lines)]

   # retrieve the seeds
   seeds <- as.numeric(unlist(strsplit(sub(

      # pattern of seed1=, seed2=, seed3=, seed4=
      ".*seed1=\\s*(\\d+) seed2=\\s*(\\d+) seed3=\\s*(\\d+) seed4=\\s*(\\d+).*",

      # replacement pattern
      "\\1,\\2,\\3,\\4",

      # the line containing the seeds
      seeds_line),

      # split by
      split=","
   )))

   if (group_by_group){

      # 4 seeds per group
      n_groups <- length(seeds)/4 

      # ensure that the number of groups is an integer
      if (n_groups != round(n_groups)){
         message(glue("Number of groups seems weird: ", n_groups))
      }

      # reshape the seeds into a matrix
      seeds_gbg <- matrix(seeds, ncol=4)
      return(seeds_gbg)

   } else {return(seeds)}
}



get_likelihood = function(lines, group_by_group=FALSE){

   if (group_by_group){
      
      # find the lines starting with pattern "group \d+ Likelihood_max="
      likelihood_lines <- lines[grepl("group \\d+ Likelihood_max=", lines)]
      
      # retrive the likelihood values
      likelihood_gbg <- as.numeric(unlist(strsplit(sub(
         
         # pattern of group \d+ Likelihood_max=
         ".*group \\d+ Likelihood_max=\\s*(-?\\d+\\.\\d+).*",
         
         # replacement pattern
         "\\1", likelihood_lines),
         
         # split by
         split=" "
      )))

      # count the number of groups to reshape into a matrix
      n_groups <- length(lines[grepl("datafile=", lines)])
      likelihood_gbg <- matrix(likelihood_gbg, ncol=n_groups)
      return(likelihood_gbg)

   } else {

      # find the line starting with "Likelihood_max="
      likelihood_line <- lines[grepl("Likelihood_max=", lines)]

      # retrieve the likelihood value
      likelihood <- as.numeric(sub(
         
         # pattern of Likelihood_max=
         "Likelihood_max=\\s*(-?\\d+\\.\\d+)",
         
         # replacement pattern
         "\\1", likelihood_line
      ))
      return(likelihood)
   }
}



get_param_ranges = function(lines){

   # find start and end of the parameter range table
   start <- which(grepl("Parameter_Range_Table", lines))
   end <- which(grepl("ratiomax", lines))
   
   # get vector of lines in the range
   range_lines <- lines[(start+1):(end-1)]

   # split the range lines into values
   range_values <- strsplit(range_lines, " ")
   
   # return the parameter ranges as a data frame
   parameter_ranges <- data.frame(
      Name = as.character(sapply(range_values, function(x) x[1])),
      Min = as.numeric(sapply(range_values, function(x) x[2])),
      Max = as.numeric(sapply(range_values, function(x) x[3]))
   )
   return(parameter_ranges)
}



get_effects = function(lines, group_by_group=FALSE){
   
   if (group_by_group){

      #TODO: implement parser for group by group effects
      stop("Group by group effects not implemented yet")

   } else {
      # get effects
      effect_lines <- lines[grepl("^eff_", lines)]
      effects <- strsplit(effect_lines, " ")
      all_effects <- data.frame(
         Name = as.character(sapply(effects, function(x) x[1])),
         Estimate = as.numeric(sapply(effects, function(x) x[2])),
         # TODO (Jo): verify with Nicolas and Thomas that this is actually the standard error
         # probably not
         StdError = as.numeric(sapply(effects, function(x) x[3]))
      )
      return(all_effects)
   }
}



get_ratio_max = function(lines){

   # find the line containing the ratiomax value
   index <- which(grepl("ratiomax", lines))
   ratiomax_line <- lines[index]

   # get the ratiomax value
   ratiomax <- as.numeric(gsub("[^0-9]", "", ratiomax_line))
   return(ratiomax)
}


parse = function(lines, element, group_by_group=FALSE){
   switch(
      element,
      seeds = get_seeds(lines, group_by_group),
      likelihood = get_likelihood(lines, group_by_group),
      effects = get_effects(lines, group_by_group),
      parameter_ranges = get_param_ranges(lines),
      ratio_max = get_ratio_max(lines)
   )
}