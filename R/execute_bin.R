#' Run Lifelihood Simulation
#'
#' This function runs the lifelihood simulation using a specified input file and custom parameters.
#'
#' @param input_file Character string specifying the path to the input file.
#' @param custom_file Character string specifying the path to the custom file.
#' @param GbyG Integer value for the GbyG parameter. Default is 0.
#' @param MCMC Integer value for the MCMC parameter. Default is 0.
#' @param interval Integer value specifying the interval. Default is 25.
#' @param SEcal Integer value for the SEcal parameter. Default is 0.
#' @param saveprobevent Integer value for the saveprobevent parameter. Default is 0.
#' @param fitness Integer value for the fitness parameter. Default is 0.
#' @param r Integer value for the r parameter. Default is 0.
#' @param seed1 Integer value specifying the first seed. Default is 12.
#' @param seed2 Integer value specifying the second seed. Default is 13.
#' @param seed3 Integer value specifying the third seed. Default is 14.
#' @param seed4 Integer value specifying the fourth seed. Default is 15.
#' @param ntr Integer value for the ntr parameter. Default is 2.
#' @param nst Integer value for the nst parameter. Default is 2.
#' @param To Integer value specifying the starting time. Default is 50.
#' @param Tf Integer value specifying the final time. Default is 1.
#' @param climbrate Numeric value specifying the climate rate. Default is 1.
#' @param precision Numeric value specifying the precision. Default is 0.001.
#'
#' @return No return value. This function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' execute_bin("path/to/input_file.txt", "path/to/custom_file.txt")
#' }
#' 
#' @import here
#' @importFrom utils system
#' @export
execute_bin <- function(
   input_file,
   custom_file,
   GbyG=0,
   MCMC=0,
   interval=25,
   SEcal=0,
   saveprobevent=0,
   fitness=0,
   r=0,
   seed1=12,
   seed2=13,
   seed3=14,
   seed4=15,
   ntr=2,
   nst=2,
   To=50,
   Tf=1,
   climbrate=1,
   precision=0.001
) {
   # concatenate the inputs and other parameters
   arg_string <- paste(
      input_file, custom_file, GbyG, MCMC, interval, SEcal, saveprobevent, fitness,
      r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
   )

   # get the path to the compiled program
   if (detect_os() == "Windows") {
      path <- file.path(here("src", "compiled"), "lifelihoodC2023.exe")
   } else if (detect_os() == "Unix-like") {
      path <- file.path(here("src", "compiled"), "lifelihoodC2023")
   } else {
      stop("Unknown OS")
   }

   # run the program
   system(
      path,
      input = arg_string
   )
}



