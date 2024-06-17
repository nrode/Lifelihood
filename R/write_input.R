#' Functions used to create input data (.txt) file for the lifelihood program
#' 
#' @name format_row
#' @title format a single row for the input data file
#' @description takes a row from a dataframe with input data (sex, maturity, clutch date and death) in interval format and transform it into a large string (required format for the data input file)
#' @param row a row of a dataframe object
#' @param extra1 name of the first column to add in the input data file 
#' @param extra2 name of the second column to add in the input data file
#' @param extra3 name of the third column to add in the input data file
#' @return a string of the well formated row
#' @export
format_row <- function(row, extra1, extra2, extra3) {

   # add variables from extra column
   extra_columns = ""
   for (extra in c(extra1, extra2, extra3)) {
      extra_val <- ifelse(is.null(extra), "", row[extra])
      extra_columns <- paste(extra_columns, extra_val)
   }

   # add the sex col and maturity event
   sex_col <- row["sex"]
   mat_start <- row["mat_start"]
   mat_end <- row["mat_end"]
   formatted_row <- paste(extra_columns, "sex", sex_col, "1000 0 mat", mat_start, mat_end)

   # extract clutch columns dynamically
   clutch_cols <- names(row)[grep("pon_", names(row))]
   for (i in seq(1, length(clutch_cols), by = 3)) {
      clutch_start <- row[[clutch_cols[i]]]
      clutch_end <- row[[clutch_cols[i + 1]]]
      clutch_size <- row[[clutch_cols[i + 2]]]
      if (!is.na(clutch_start) & !is.na(clutch_end) & !is.na(clutch_size)) {
         formatted_row <- paste(formatted_row, "pon", clutch_start, clutch_end, clutch_size)
      }
   }

   # add the 'mor' column (death event)
   mor_start <- row["mor_start"]
   mor_end <- row["mor_end"]
   formatted_row <- paste(formatted_row, "mor", mor_start, mor_end)

   # return the well formatted row
   return(formatted_row)
}

#' @name format_dataframe_to_txt
#' @title create the input data file from a dataframe
#' @description takes a dataframe and apply to each row the [format_row()] function to create the input data file
#' @param df the dataframe of the input data
#' @param output_file path used to write the input data file
#' @param extra1 name of the first column to add in the input data file
#' @param extra2 name of the second column to add in the input data file
#' @param extra3 name of the third column to add in the input data file
#' @details the number of extra column is **currently limited** to 3 for simplicity, but we definitly should **change** this accept an unlimited number of extra columns
#' @return NULL
#' @export
format_dataframe_to_txt <- function(df, extra1=NULL, extra2=NULL, extra3=NULL) {

   # apply the formatting function to each row of the dataframe
   formatted_rows <- apply(df, 1, function(row) format_row(row, extra1, extra2, extra3))

   # add the header of the data
   header_line <- "*******data*********"
   formatted_rows <- c(header_line, formatted_rows)

   # write the formatted rows to the output file
   writeLines(formatted_rows, con = "input_data.txt")
}