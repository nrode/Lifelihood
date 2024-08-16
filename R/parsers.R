#' @title Parsing functions used to read the output file of the program


#' @keywords internal
#' @name parse_output
#' @description `parse_output()`: Find specific result in the output file of the lifelihood program, according to the `element` argument. This function is an aggregator of all the `get_*()` functions described below.
#' @param lines Vector of the output file (`.out`), where each element is a line of the file.
#' @param element Name of the result to parse. Must be in one of 'seeds', 'likelihood', 'effects', 'parameter_ranges', 'ratio_max'.
#' @param group_by_group Boolean indicating whether parsing should be performed group by group or not (`FALSE` by default). This argument is necessary because the structure of the output file is different depending on whether the analysis was carried out "group by group" or not (the analysis method used will then be different, for certain parsers).
#' @return The parsed element
parse_output <- function(lines, element, group_by_group = FALSE) {
  switch(element,
    seeds = get_seeds(lines, group_by_group),
    likelihood = get_likelihood(lines, group_by_group),
    effects = get_effects(lines, group_by_group),
    parameter_ranges = get_param_ranges(lines),
    ratio_max = get_ratio_max(lines)
  )
}


#' @rdname parse_output
#'
#' @description `get_seeds()`: find the seeds value in the output file of the lifelihood program.
get_seeds <- function(lines, group_by_group = FALSE) {
  # find the line starting with pattern "seed1="
  seeds_line <- lines[grepl("seed1=", lines)]

  # retrieve the seeds
  seeds <- as.numeric(unlist(strsplit(
    sub(

      # pattern of seed1=, seed2=, seed3=, seed4=
      ".*seed1=\\s*(\\d+) seed2=\\s*(\\d+) seed3=\\s*(\\d+) seed4=\\s*(\\d+).*",

      # replacement pattern
      "\\1,\\2,\\3,\\4",

      # the line containing the seeds
      seeds_line
    ),

    # split by
    split = ","
  )))

  if (group_by_group) {
    # 4 seeds per group
    n_groups <- length(seeds) / 4

    # ensure that the number of groups is an integer
    if (n_groups != round(n_groups)) {
      message(paste("Number of groups seems weird:", n_groups))
    }

    # reshape the seeds into a matrix
    seeds_gbg <- matrix(seeds, ncol = 4)
    return(seeds_gbg)
  } else {
    return(seeds)
  }
}

#' @rdname parse_output
#'
#' @description `get_likelihood()`: find the likelihood value (optimum found) in the output file of the lifelihood program.
get_likelihood <- function(lines, group_by_group = FALSE) {
  if (group_by_group) {
    # find the lines starting with pattern "group \d+ Likelihood_max="
    likelihood_lines <- lines[grepl("group \\d+ Likelihood_max=", lines)]

    # retrive the likelihood values
    likelihood_gbg <- as.numeric(unlist(strsplit(
      sub(

        # pattern of group \d+ Likelihood_max=
        ".*group \\d+ Likelihood_max=\\s*(-?\\d+\\.\\d+).*",

        # replacement pattern
        "\\1", likelihood_lines
      ),

      # split by
      split = " "
    )))

    # count the number of groups to reshape into a matrix
    n_groups <- length(lines[grepl("datafile=", lines)])
    likelihood_gbg <- matrix(likelihood_gbg, ncol = n_groups)
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

#' @rdname parse_output
#'
#' @description `get_param_ranges()`: find the parameter ranges/boundaries in the output file of the lifelihood program.
get_param_ranges <- function(lines) {
  # find start and end of the parameter range table
  start <- which(grepl("Parameter_Range_Table", lines))
  end <- which(grepl("ratiomax", lines))

  # get vector of lines in the range
  range_lines <- lines[(start + 1):(end - 1)]

  # split the range lines into values
  range_values <- strsplit(range_lines, " ")

  # return the parameter ranges/boundaries as a data frame
  parameter_ranges <- data.frame(
    name = as.character(sapply(range_values, function(x) x[1])),
    min = as.numeric(sapply(range_values, function(x) x[2])),
    max = as.numeric(sapply(range_values, function(x) x[3]))
  )
  return(parameter_ranges)
}

#' @rdname parse_output
#'
#' @description `get_ratio_max()`: find the ratio max value in the output file of the lifelihood program.
get_ratio_max <- function(lines) {
  # find the line containing the ratiomax value
  index <- which(grepl("ratiomax", lines))
  ratiomax_line <- lines[index]

  # get the ratiomax value
  ratiomax <- as.numeric(gsub("[^0-9]", "", ratiomax_line))
  return(ratiomax)
}

#' @rdname parse_output
#'
#' @description `get_effects()`: find the estimated effects in the output file of the lifelihood program.
get_effects <- function(lines, group_by_group = FALSE) {
  if (group_by_group) {
    # find all lines starting with "group"
    groups <- grep("^group", lines)

    all_effects_gbg <- lapply(groups, function(i) {
      # get the lines for the group and split them
      line <- lines[i]
      effects <- unlist(strsplit(line, " "))

      # get name, first and second value, with a step of 3
      effect_names <- effects[seq(5, length(effects) - 1, by = 3)]
      est <- as.numeric(effects[seq(6, length(effects) - 1, by = 3)])
      stderr <- as.numeric(effects[seq(7, length(effects), by = 3)])

      data.frame(
        name = effect_names,
        estimation = est,
        stderror = stderr
      )
    })
    return(all_effects_gbg)
  } else {
    # find start and end of the effects
    start <- grep("Likelihood_max", lines)
    after_likelihood <- lines[(start + 1):length(lines)]
    end <- which(after_likelihood == "")[1]

    # get vector of lines in the range
    effect_lines <- after_likelihood[1:(end - 1)]
    effects <- strsplit(effect_lines, " ")

    # return the effects as a data frame
    all_effects <- data.frame(
      name = as.character(sapply(effects, function(x) x[1])),
      estimation = as.numeric(sapply(effects, function(x) x[2])),
      stderror = as.numeric(sapply(effects, function(x) x[3]))
    )
    return(all_effects)
  }
}
