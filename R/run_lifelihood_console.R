#' Run lifelihood program
#' @description Run lifelihood program in console mode
#' @param infile file with the model and data to be fitted
#' @param customfile file with the min and max boudaries for each parameter
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
#' @return dataset with the simulated hatch rate
#' @export
#'
#' @examples
#'run_lifelihood_console(infile="100%mort_Pierrick211genoparinteraction.txt", ntr=10, To=50, Tf=1, seed2=34)

run_lifelihood_console <- function(infile="100%mort_Pierrick211genoparinteraction.txt", customfile="custom.txt", GbyG=0, MCMC=0, interval=25, SEcal=0, saveprobevent=0, fitness=0, r=0, seed1=12, seed2=13, seed3=14, seed4=15, ntr=2, nst=2, To=50, Tf=1, climbrate=1, precision=0.001) 
{
  infile<-paste(getwd(),infile,sep="/")
  infile<-gsub("/","\\",infile,fixed=T)
  customfile<-paste(getwd(), customfile,sep="/")
  customfile<-gsub("/","\\",customfile,fixed=T)
  
  string<-paste(infile, customfile, GbyG, MCMC, interval, SEcal, saveprobevent, fitness, r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate,precision)
  print(string)
  system("lifelihoodC2023.exe", intern = FALSE,
         ignore.stdout = FALSE, ignore.stderr = FALSE,
         wait = TRUE, input = string, show.output.on.console = TRUE,
         minimized = FALSE, invisible = TRUE)
}

