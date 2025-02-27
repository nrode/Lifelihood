#' @title Create the input data file from a dataframe
#' @name format_dataframe_to_txt
#' @description Takes a dataframe (`df` argument in [lifelihoodData()] function) and apply to each row the [format_row()] function to create the input data file.
#' @keywords internal
#' @inheritParams lifelihoodData
#' @inheritParams lifelihood
#' @param temp_dir Name of the temporary directory with temporary files
#' @return NULL
#' @export
format_dataframe_to_txt <- function(
  df,
  sex,
  sex_start,
  sex_end,
  maturity_start,
  maturity_end,
  matclutch,
  matclutch_size,
  clutchs,
  death_start,
  death_end,
  covariates,
  model_specs,
  path_config,
  temp_dir
) {
  all_column_names <- c(
    sex,
    sex_start,
    sex_end,
    covariates,
    maturity_start,
    maturity_end,
    death_start,
    death_end,
    clutchs
  )

  for (column_name in all_column_names) {
    if (!(column_name %in% colnames(df))) {
      matches <- agrep(
        column_name,
        colnames(df),
        value = TRUE,
        max.distance = 0.2
      )
      match <- ifelse(length(matches) > 1, matches[1], matches)

      message_display <- paste(
        "'",
        column_name,
        "'",
        " is not a column in the passed dataframe.",
        " Did you mean: ",
        match,
        "?\n",
        sep = ""
      )
      warning(message_display)
    }
  }

  formatted_rows <- apply(
    df,
    1,
    function(row) {
      format_row(
        row,
        sex,
        sex_start,
        sex_end,
        maturity_start,
        maturity_end,
        clutchs,
        death_start,
        death_end,
        covariates
      )
    }
  )

  header_line <- "*******data*********"
  formatted_rows <- c(header_line, formatted_rows)

  config_file_info <- format_config(
    path_config = path_config,
    covariates = covariates
  )
  model_info <- c(
    "****modele******",
    paste(model_specs, collapse = " "),
    config_file_info
  )
  formatted_rows <- c(model_info, formatted_rows)

  matclutch <- ifelse(matclutch, "true", "false")
  n_cat_covariates <- c()
  for (cov in covariates) {
    if (is.numeric(df[[cov]])) {
      stop(paste(
        "Error: The column",
        cov,
        "is numeric.",
        "This feature is currently not supported.",
        "Try converting your covariates into factors/integers or discretising them into categories."
      ))
    }
    n_cat <- ifelse(nlevels(df[[cov]]) == 0, 1, nlevels(df[[cov]]))
    n_cat_covariates <- c(n_cat_covariates, n_cat)
  }
  data_struct_info <- c(
    "*******data struct****",
    paste("matclutch", matclutch),
    paste(covariates, collapse = " "),
    paste(n_cat_covariates, collapse = " ")
  )
  formatted_rows <- c(data_struct_info, formatted_rows)

  path_to_data <- file.path(temp_dir, "temp_file_data_lifelihood.txt")
  writeLines(formatted_rows, con = path_to_data)
  return(path_to_data)
}


#' @keywords internal
#' @name format_row
#' @title Format a dataframe row for the input data file
#' @description Takes a row from a dataframe with input data (sex, maturity, clutch date and death) in interval format and transform it into a large string (required format for the data input file).
#' @param row A row of the dataframe object provided by the user (`df` argument in [lifelihoodData()] function).
#' @inheritParams lifelihoodData
#' @return A string of the well formated row.
#' @export
format_row <- function(
  row,
  sex,
  sex_start,
  sex_end,
  maturity_start,
  maturity_end,
  clutchs,
  death_start,
  death_end,
  covariates
) {
  # add variables from covariate columns
  cov_values <- ""
  for (cov in covariates) {
    cov_values <- paste(cov_values, row[cov])
  }

  # add the sex col and maturity event
  formatted_row <- paste(
    cov_values,
    "sex",
    row[sex_start],
    row[sex_end],
    row[sex],
    "mat",
    row[maturity_start],
    row[maturity_end]
  )

  # extract clutch columns dynamically
  clutch_cols <- clutchs
  for (i in seq(1, length(clutch_cols), by = 3)) {
    clutch_start <- gsub("[[:space:]]", "", row[[clutch_cols[i]]])
    clutch_end <- gsub("[[:space:]]", "", row[[clutch_cols[i + 1]]])
    clutch_size <- gsub("[[:space:]]", "", row[[clutch_cols[i + 2]]])
    if (!(is.na(clutch_start) || is.na(clutch_end) || is.na(clutch_size))) {
      if (clutch_start != "NA" & clutch_end != "NA" & clutch_size != "NA") {
        formatted_row <- paste(
          formatted_row,
          "pon",
          clutch_start,
          clutch_end,
          clutch_size
        )
      }
    }
  }

  # add the death events
  death_start_value <- row[death_start]
  death_end_value <- row[death_end]
  formatted_row <- paste(
    formatted_row,
    "mor",
    death_start_value,
    death_end_value
  )

  formatted_row <- gsub("\\s+", " ", formatted_row)
  formatted_row <- sub("^\\s+", "", formatted_row)

  # return the well formatted row
  return(formatted_row)
}
