#' @title Create the input data file from a dataframe
#' 
#' @description Takes a dataframe (`df` argument in [lifelihood()] function) and apply to each row the [format_row()] function to create the input data file.
#' 
#' @keywords internal
#' 
#' @name format_dataframe_to_txt
#' @param df The dataframe object of the input data.
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param matclutch Whether the maturity event (designated by `maturity_start` and `maturity_end`) is a clutch event or not. If `TRUE`, must specify the `matclutch_size` argument.
#' @param matclutch_size Column name containing the size of the clutch for the maturity event. Only used (and required) if `matclutch` is `TRUE`.
#' @param clutchs Vector containing the names of the clutch columns. The order should be:
#' - first date of the first clutch
#' - second date of first clutch
#' - clutch size of the first clutch
#' - second clutch first date
#' - first clutch second date
#' - second clutch clutch size
#' - and so on.
#' If the observation with the most clutches is for example 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param covariates Vector containing the names of the covariates.
#' @param models Vector of characters with the name of the statistical law to use. Must be of length 3 and each element must be in "wei", "gam" or "lgn". The first one is used for maturity, the second one is used for clutchs and the third one for death.
#' @param path_config Path to the configuration file (YAML).
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
   models,
   path_config
){

   # create vector with all column names
   all_column_names <- c(sex, sex_start, sex_end, covariates, maturity_start, maturity_end, death_start, death_end, clutchs)

   # test the presence of the columns in the dataframe
   for (column_name in all_column_names){
      if (!(column_name %in% colnames(df))){

         # get close match
         matches <- agrep(column_name, colnames(df), value = TRUE, max.distance = 0.2)
         match <- ifelse(length(matches)>1, matches[1], matches)
         
         # display warning if not found
         message_display = paste(
            "'", column_name, "'",
            " is not a column in the passed dataframe.",
            " Did you mean: ", match, "?\n",
            sep = ""
         )
         warning(message_display)
      }
   }

   # apply the formatting function to each row of the dataframe
   formatted_rows <- apply(
      df, 1,
      function(row) format_row(
         row, sex, sex_start, sex_end, maturity_start, maturity_end,
         clutchs, death_start, death_end, covariates
      )
   )

   # add the header of the data
   header_line <- "*******data*********"
   formatted_rows <- c(header_line, formatted_rows)
   
   # add model info
   config_file_info <- format_config(path_config = path_config, covariates = covariates)
   model_info <- c(
      "****modele******",
      paste(models, collapse = " "),
      config_file_info
   )
   formatted_rows <- c(model_info, formatted_rows)

   # add data structure info
   matclutch <- ifelse(matclutch, "true", "false")
   n_cat_covariates <- c()
   for (cov in covariates){
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

   # write the formatted rows to the output file
   path_to_data <- here::here("temp_file_data_lifelihood.txt")
   writeLines(formatted_rows, con = path_to_data)

   return(path_to_data)
}


#' @keywords internal
#' @name format_row
#' @title Format a dataframe row for the input data file
#' @description Takes a row from a dataframe with input data (sex, maturity, clutch date and death) in interval format and transform it into a large string (required format for the data input file).
#' @param row A row of the dataframe object provided by the user (`df` argument in [lifelihood()] function).
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param matclutch Whether the maturity event (designated by `maturity_start` and `maturity_end`) is a clutch event or not. If `TRUE`, must specify the `matclutch_size` argument.
#' @param matclutch_size Column name containing the size of the clutch for the maturity event. Only used (and required) if `matclutch` is `TRUE`.
#' @param clutchs Vector containing the names of the clutch columns. The order should be:
#' - first date of the first clutch
#' - second date of first clutch
#' - clutch size of the first clutch
#' - second clutch first date
#' - first clutch second date
#' - second clutch clutch size
#' - and so on.
#' 
#' If the observation with the most clutches is for example 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param covariates Vector containing the names of the covariates.
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
    covariates) {
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
      if (clutch_start != "NA" & clutch_end != "NA" & clutch_size != "NA") {
         formatted_row <- paste(formatted_row, "pon", clutch_start, clutch_end, clutch_size)
      }
   }

   # add the death events
   death_start_value <- row[death_start]
   death_end_value <- row[death_end]
   formatted_row <- paste(formatted_row, "mor", death_start_value, death_end_value)

   formatted_row <- gsub("\\s+", " ", formatted_row)
   formatted_row <- sub("^\\s+", "", formatted_row)

   # return the well formatted row
   return(formatted_row)
}