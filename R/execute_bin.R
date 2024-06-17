#' Execute compiled files of the lifelihood program
#' @name execute_bin
#' @description (internal function) Run lifelihood program in console mode
#' @param infile file with the model and data to be fitted
#' @param customfile file with the min and max boudaries for each parameter
#' @param GbyG Option to fit the full factorail model with all the interactions between each of the factors
#' @param MCMC Perform MCMC sampling of the parameter after convergence to estimate their 95% confidence interval
#' @param interval TBD - Check the actual meaning
#' @param SEcal Compute the standard error of eahc parameter using the Hessian matrix
#' @param saveprobevent TBD - Check the actual meaning
#' @param fitness Reparametrize the model with one parameter as the lifetime reproductive success
#' @param r Reparametrize the model with one parameter as the intrinsic rate of increase
#' @param seed1 first seed used so that random values can found again
#' @param seed2 second seed used so that random values can found again
#' @param seed3 third seed used so that random values can found again
#' @param seed4 fourth seed used so that random values can found again
#' @param ntr number of thread for the paralelisation ?
#' @param nst TBD - Check the actual meaning
#' @param To Initial temperature for the simulated annealing
#' @param Tf Initial temperature for the simulated annealing
#' @param climbrate Rate for the simulated annealing ?
#' @param precision TBD - Check the actual meaning
#' @return NULL
#' @export
execute_bin <- function(
   input_file,
   custom_file,
   group_by_group=0,
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
      input_file, custom_file, group_by_group, MCMC, interval, SEcal, saveprobevent, fitness,
      r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
   )

   # get the path to the compiled program and execute it
   os <- detect_os()
   path <- switch(
      os,
      "Windows" = file.path(here::here("src", "compiled"), "lifelihoodC2023.exe"),
      "Unix-like" = file.path(here::here("src", "compiled"), "lifelihoodC2023"),
      stop("Unknown OS")
   )
   system(path, input = arg_string)
}