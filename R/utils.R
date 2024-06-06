

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


delete_param_range <- function(file_path){
   if (file.exists(file_path)){
      file.remove(file_path)
   } else {
      message <- paste("File", file_path, "does not exist")
      stop(message)
   }
}