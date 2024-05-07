
read_output <- function(file_path) {
   lines <- readLines(file_path)
   results <- list()
   results$datafile <- file_path

   # get seeds
   seeds_line <- lines[grepl("seed1=", lines)]
   results$seeds <- as.numeric(unlist(strsplit(sub(".*seed1=\\s*(\\d+) seed2=\\s*(\\d+) seed3=\\s*(\\d+) seed4=\\s*(\\d+).*", "\\1,\\2,\\3,\\4", seeds_line), ",")))

   # get likelihood
   likelihood_line <- lines[grepl("Likelihood_max=", lines)]
   results$likelihood_max <- as.numeric(sub("Likelihood_max=\\s*(-?\\d+\\.\\d+)", "\\1", likelihood_line))

   # get effects
   effect_lines <- lines[grepl("^eff_", lines)]
   effects <- strsplit(effect_lines, " ")
   results$effects <- data.frame(
      Name = as.character(sapply(effects, function(x) x[1])),
      Estimate = as.numeric(sapply(effects, function(x) x[2])),
      # TODO: verify with Nicolas and Thomas that this is actually the standard error
      StdError = as.numeric(sapply(effects, function(x) x[3]))
   )

   # get parameter ranges
   index <- which(grepl("Parameter_Range_Table", lines)) # find the first match
   range_lines <- lines[index+1:length(lines)]
   range_values <- strsplit(range_lines, " ")
   results$parameter_ranges <- data.frame(
      Name = as.character(sapply(range_values, function(x) x[1])),
      Min = as.numeric(sapply(range_values, function(x) x[2])),
      Max = as.numeric(sapply(range_values, function(x) x[3]))
   )

   return(results)
}


file_path = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   '100%mort_Pierrick211genoparinteraction.out'
)
fitted = read_output(file_path)

fitted$datafile
fitted$seeds
fitted$likelihood_max
head(fitted$effects)
head(fitted$parameter_ranges)