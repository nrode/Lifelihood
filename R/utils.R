

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