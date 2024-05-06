#' Run lifelihood program
#' @description Run lifelihood program in console mode
#' @param input_file file with the model and data to be fitted
#' @param custom_file file with the min and max boudaries for each parameter
#' @param GbyG Option to fit the full factorail model with all the interactions between each of the factors
#' @param MCMC Perform MCMC sampling of the parameter after convergence to estimate their 95% confidence interval
#' @param interval TBD - Check the actual meaning
#' @param SEcal Compute the standard error of eahc parameter using the Hessian matrix
#' @param saveprobevent TBD - Check the actual meaning
#' @param fitness Reparametrize the model with one parameter as the lifetime reproductive success
#' @param r Reparametrize the model with one parameter as the intrinsic rate of increase
#' @param seed1 first seed used so that random values can found again (https://en.wikipedia.org/wiki/Multiply-with-carry_pseudorandom_number_generator)
#' @param seed2 second seed used so that random values can found again
#' @param seed3 third seed used so that random values can found again
#' @param seed4 fourth seed used so that random values can found again
#' @param ntr number of thread for the paralelisation ?
#' @param nst TBD - Check the actual meaning
#' @param To Initial temperature for the simulated annealing
#' @param Tf Initial temperature for the simulated annealing
#' @param climbrate Rate for the simulated annealing ?
#' @param precision TBD - Check the actual meaning
#' @name run_lifelihood
#' @return dataset with the simulated hatch rate
#' @export
#'
#' @examples
#' library(here)
#' 
#' # path to inputs
#'input_file = file.path(
#'    'data', 'raw_data', 'DataPierrick_GroupbyGroup',
#'    '100%mort_Pierrick211genoparinteraction.txt'
#' )
#' custom_file = file.path('data', 'custom.txt')
#
#' # run the program
#' run_lifelihood(
#'    input_file = input_file,
#'    custom_file = custom_file
#')

library(here)

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

run_lifelihood <- function(
   input_file,
   custom_file,
   GbyG,
   MCMC,
   interval,
   SEcal,
   saveprobevent,
   fitness,
   r,
   seed1,
   seed2,
   seed3,
   seed4,
   ntr,
   nst,
   To,
   Tf,
   climbrate,
   precision
) {
   # Set default values within the function body
   if (missing(GbyG)) GbyG <- 0
   if (missing(MCMC)) MCMC <- 0
   if (missing(interval)) interval <- 25
   if (missing(SEcal)) SEcal <- 0
   if (missing(saveprobevent)) saveprobevent <- 0
   if (missing(fitness)) fitness <- 0
   if (missing(r)) r <- 0
   if (missing(seed1)) seed1 <- 12
   if (missing(seed2)) seed2 <- 13
   if (missing(seed3)) seed3 <- 14
   if (missing(seed4)) seed4 <- 15
   if (missing(ntr)) ntr <- 2
   if (missing(nst)) nst <- 2
   if (missing(To)) To <- 50
   if (missing(Tf)) Tf <- 1
   if (missing(climbrate)) climbrate <- 1
   if (missing(precision)) precision <- 0.001

   # concatenate the inputs and other parameters
   arg_string <- paste(
      input_file, custom_file, GbyG, MCMC, interval, SEcal, saveprobevent, fitness,
      r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
   )

   # run lifelihood for windows
   if (detect_os() == "Windows") {
      print("Running on Windows")
      path <- file.path(here("src", "compiled"), "lifelihoodC2023.exe")
      system(
         path,
         input = arg_string
      )

   # run lifelihood for Unix-like
   } else if (detect_os() == "Unix-like") {
      print("Running on Unix")
      path <- file.path(here("src", "compiled"), "lifelihoodC2023")
      system(
         path,
         input = arg_string
      )

   # stop the program if the OS is not properly detected
   } else {
      stop("Unknown OS")
   }
}


