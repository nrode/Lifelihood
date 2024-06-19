#' @name format_row
#' @title Format a single row for the input data file
#' @description Takes a row from a dataframe with input data (sex, maturity, clutch date and death) in interval format and transform it into a large string (required format for the data input file)
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
      clutch_start <- row[[clutch_cols[i]]]
      clutch_end <- row[[clutch_cols[i + 1]]]
      clutch_size <- row[[clutch_cols[i + 2]]]
      if (!is.na(clutch_start) & !is.na(clutch_end) & !is.na(clutch_size)) {
         formatted_row <- paste(formatted_row, "pon", clutch_start, clutch_end, clutch_size)
      }
   }

   # add the 'mor' column (death event)
   death_start_value <- row[death_start]
   death_end_value <- row[death_end]
   formatted_row <- paste(formatted_row, "mor", death_start_value, death_end_value)

   # return the well formatted row
   return(formatted_row)
}

#' @name format_dataframe_to_txt
#' @title Create the input data file from a dataframe
#' @description Takes a dataframe and apply to each row the [format_row()] function to create the input data file.
#' @param df The dataframe of the input data.
#' @param sex Column name containing the sex of the observations.
#' @param sex_start Column name containing the first date of the interval in which the sex was determined.
#' @param sex_end Column name containing the second date of the interval in which the sex was determined.
#' @param maturity_start Column name containing the first date of the interval in which the maturity was determined.
#' @param maturity_end Column name containing the second date of the interval in which the maturity was determined.
#' @param clutchs Vector containing the names of the clutch columns. The order should be: first clutch first date, first clutch second date, first clutch clutch size, second clutch first date, first clutch second date, second clutch clutch size, and so on. If the observation with the most clutches is, for example, 10, then the vector must be of size 10 x 3 = 30 (3 elements per clutch: first date, second date and size).
#' @param death_start Column name containing the first date of the interval in which the death was determined.
#' @param death_end Column name containing the second date of the interval in which the death was determined.
#' @param extra1 (facultative) Column name of the first column to add in the input data file
#' @param extra2 (facultative) Column name of the second column to add in the input data file
#' @param extra3 (facultative) Column name of the third column to add in the input data file
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
   clutchs,
   death_start,
   death_end,
   extra1=NULL,
   extra2=NULL,
   extra3=NULL
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

   # write the formatted rows to the output file
   writeLines(formatted_rows, con = "input_data.txt")
}