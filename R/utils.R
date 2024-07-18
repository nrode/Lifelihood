#' @title Find the operating system of the user
#' 
#' @description `detect_os()` finds the operating system name
#' 
#' @keywords internal
#' @name detect_os
#' @return String with the name of the operating system
#' @export
detect_os <- function() {
   os <- Sys.info()["sysname"]
   if (os == "Windows") {
      return("Windows")
   } else if (os == "Linux" || os == "Darwin") {
      return("Unix-like")
   } else {
      return("Unknown")
   }
}


#' @title Write the `.txt` file containing the parameter ranges (historically known as `custom.txt`)
#' 
#' @description write_param_range()` takes a data frame of parameter ranges and writes it as a .txt file to the given path (argument `file_name`). These values are used by lifelihood to find out which range to optimise for a given parameter.
#' 
#' @keywords internal
#' @name write_param_range
#' @param data A dataframe with 3 columns: parameter names, minimum value and maximum value
#' @param file_name Path (string) of where the `.txt` file will be stored
#' @export
write_param_range <- function(data, file_name){
  write.table(
   data,
   file = file_name,
   sep = "\t",
   row.names = FALSE,
   col.names = FALSE,
   quote = FALSE
)
   return(file_name)
}