create_txt_file <- function(dataframe, file_path) {
  # Open the file for writing
  file_conn <- file(file_path, "w")
  
  # Iterate over each row in the dataframe
  for (i in 1:nrow(dataframe)) {
    # Convert the row to a character vector
    row_data <- as.character(unlist(dataframe[i, ]))
    
    # Convert NA values to 0
    row_data[is.na(row_data)] <- "0"
    
    # Combine the elements of the row with " " as separator
    row_string <- paste(row_data, collapse = " ")
    
    # Write the row to the file
    cat(row_string, file = file_conn, "\n")
  }
  
  # Close the file connection
  close(file_conn)
}


