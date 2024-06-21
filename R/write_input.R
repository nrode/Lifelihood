#' @name format_row
#' @title (internal function) Format a single row for the input data file
#' @description (internal function) Takes a row from a dataframe with input data (sex, maturity, clutch date and death) in interval format and transform it into a large string (required format for the data input file)
#' @param row A row of a dataframe object
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param clutchs Vector containing the names of the clutch columns. The order should be: first clutch first date, first clutch second date, first clutch clutch size, second clutch first date, first clutch second date, second clutch clutch size, and so on. If the observation with the most clutches is, for example, 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param extra1 Column name of the first column to add in the input data file
#' @param extra2 Column name of the second column to add in the input data file
#' @param extra3 Column name of the third column to add in the input data file
#' @return A string of the well formated row
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
   extra1,
   extra2,
   extra3
){

   # add variables from extra column
   extra_columns = ""
   for (extra in c(extra1, extra2, extra3)) {
      extra_val <- ifelse(is.null(extra), "", row[extra])
      extra_columns <- paste(extra_columns, extra_val)
   }

   # add the sex col and maturity event
   sex_value <- row[sex]
   sex_start_value <- row[sex_start]
   sex_end_value <- row[sex_end]
   mat_start_value <- row[maturity_start]
   mat_end_value <- row[maturity_end]
   formatted_row <- paste(
      extra_columns,
      "sex",
      sex_start_value,
      sex_end_value,
      sex_value,
      "mat",
      mat_start_value,
      mat_end_value
   )

   # extract clutch columns dynamically
   clutch_cols <- names(row)[grep("pon_", names(row))]
   clutch_cols <- clutchs
   for (i in seq(1, length(clutch_cols), by = 3)) {
      clutch_start <- gsub("[[:space:]]", "", row[[clutch_cols[i]]])
      clutch_end <- gsub("[[:space:]]", "", row[[clutch_cols[i + 1]]])
      clutch_size <- gsub("[[:space:]]", "", row[[clutch_cols[i + 2]]])
      if (clutch_start != 'NA' & clutch_end != 'NA' & clutch_size != 'NA') {
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

#' @name format_dataframe_to_txt
#' @title (internal function) Create the input data file from a dataframe
#' @description (internal function) Takes a dataframe and apply to each row the [format_row()] function to create the input data file.
#' @param df The dataframe of the input data.
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param matclutch Whether the maturity event (designated by `maturity_start` and `maturity_end`) is a clutch event or not. If `TRUE`, must specify the `matclutch_size` argument.
#' @param matclutch_size Column name containing the size of the clutch for the maturity event. Only used (and required) if `matclutch` is `TRUE`.
#' @param clutchs Vector containing the names of the clutch columns. The order should be: first clutch first date, first clutch second date, first clutch clutch size, second clutch first date, first clutch second date, second clutch clutch size, and so on. If the observation with the most clutches is, for example, 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param extra1 Column name of the first column to add in the input data file
#' @param extra2 Column name of the second column to add in the input data file
#' @param extra3 Column name of the third column to add in the input data file
#' @param models Vector of characters with the name of the statistical law to use. Must be of length 3 and each element must be in "wei", "gam" or "lgn". The first one is used for maturity, the second one is used for clutchs and the third one for death.
#' @details The number of extra column is **currently limited** to 3 for simplicity, but we definitly should **change** this accept an unlimited number of extra columns
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
   extra1,
   extra2,
   extra3,
   models
){

   # create vector with all column names
   all_column_names <- c(sex, sex_start, sex_end, maturity_start, maturity_end, death_start, death_end, clutchs)

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
         clutchs, death_start, death_end, extra1, extra2, extra3
      )
   )

   # add the header of the data
   header_line <- "*******data*********"
   formatted_rows <- c(header_line, formatted_rows)
   
   # add model info (DEFAULT ABITRARY VALUES)
   model_info <- c(
      "****modele******",
      paste(models, collapse = " "),
      "mortuf 0",
      "morta 0",
      "Rmortum -1",
      "mortp -1",
      "propmal -1",
      "matuf 0",
      "mata 0",
      "Rmatum -1",
      "pontu 0",
      "ponta 0",
      "(W)pontn 0",
      "to(ps)int -1",
      "to(ps)am -1",
      "to(ps)tp -1",
      "sen(pu)t -1",
      "sen(pu)t2 -1",
      "sen(pn)t -1",
      "sen(pn)t2 -1",
      "to(pupn) -1"
   )
   formatted_rows <- c(model_info, formatted_rows)

   # add data structure info
   matclutch <- ifelse(matclutch, "true", "false")
   if (is.null(extra1)){n_cat_extra1 <- NULL} else {n_cat_extra1 <- nrow(unique(df[extra1]))}
   if (is.null(extra2)){n_cat_extra2 <- NULL} else {n_cat_extra2 <- nrow(unique(df[extra2]))}
   if (is.null(extra3)){n_cat_extra3 <- NULL} else {n_cat_extra3 <- nrow(unique(df[extra3]))}
   data_struct_info <- c(
      "*******data struct****",
      paste("matclutch", matclutch),
      paste(c(extra1, extra2, extra3), collapse = " "),
      paste(c(n_cat_extra1, n_cat_extra2, n_cat_extra3), collapse = " ")
   )
   formatted_rows <- c(data_struct_info, formatted_rows)

   # write the formatted rows to the output file
   path_to_data <- "input_data_lifelihood.txt"
   writeLines(formatted_rows, con = path_to_data)

   return(path_to_data)
}