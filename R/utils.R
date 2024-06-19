#' Group of internal functions used in the lifelihood programme
#' 
#' @name detect_os
#' @title (internal function) Find the operating system of the user
#' @description (internal function) `detect_os()` finds the OS name
#' @return String of the operatin system
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

#' @name write_param_range
#' @title (internal function) Write parameter ranges/boundaries to a file
#' @description (internal function) `write_param_range()` takes a dataframe of parameter ranges/boundaries and write it as .txt file
#' @param data A dataframe with 3 columns: parameter names, minimum value and maximum value
#' @param file_name Path of where the .txt file will be stored
#' @export
write_param_range <- function(data, file_name = "param_range.txt"){
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

#' @name delete_param_range
#' @title (internal function) Delete parameter ranges/boundaries file
#' @description (internal function) `delete_param_range()` takes a path to the parameter ranges/boundaries file and delete it
#' @param file_path Path of where the .txt file is
#' @export 
delete_param_range <- function(file_path){
   if (file.exists(file_path)){
      file.remove(file_path)
   } else {
      message <- paste("File", file_path, "does not exist")
      stop(message)
   }
}