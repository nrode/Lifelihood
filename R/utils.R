#' @title Compute and visualize predicted mortality rate
#'
#' @name link
#' @keywords internal
#' @export
link <- function(estimate, min_and_max) {
   min <- min_and_max[1]
   max <- min_and_max[2]
   return(min + (max - min) / (1 + exp(-estimate)))
}

#' @title Compute and visualize predicted mortality rate
#' @name delink
#' @keywords internal
#' @export
delink <- function(obs, min_and_max) {
   min <- min_and_max[1]
   max <- min_and_max[2]
   return(log((obs - min) / (max - obs)))
}

#' @title Compute and visualize predicted mortality rate
#' @name SurvWei
#' @keywords internal
#' @export
SurvWei <- function(t, ExpLong, Shape) {
   Scale <- ExpLong / gamma(1 + 1 / Shape)
   exp(-(t / Scale)^Shape)
}

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