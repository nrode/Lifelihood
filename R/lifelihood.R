#' Main function of the lifelihood function
#' @name lifelihood
#' @param data_path path to the input file with data and model
#' @param param_range_df dataframe with the parameter ranges/boundaries/boundaries
#' @param group_by_group Option to fit the full factorail model with all the interactions between each of the factors
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
#' @export
lifelihood <- function(
   data_path,
   param_range_df=NULL,
   group_by_group=FALSE,
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
){

   # if param_range_df is NULL, use default values
   if(is.null(param_range_df)){
      message("Using default parameter ranges/boundaries")
      param_range_df <- data.frame(
         param = c("E(tmort)f", "morta", "RE(tmort)m", "mortp", "propmal", "E(tmat)f", "mata", 
                     "RE(tmat)m", "E(tpon)", "ponta", "pontn", "to(ps)int", "to(ps)am", "to(ps)tp", 
                     "sen(pu)t", "sen(pu)t2", "sen(pn)t", "sen(pn)t2", "to(pupn)", "W"),
         min = c(1, 0.001, 0.1, 0.0001, 0.00001, 1, 0.0001, 0.1, 0.1, 0.001, 1, 0.00001, 
                  0.0000001, 0.0000001, -20, -20, -10, -10, -10, 0.001),
         max = c(200, 30, 4, 1, 0.99999, 100, 12, 10, 200, 12, 50, 10, 10, 10, 20, 20, 10, 10, 10, 1000)
      )
   }

   # change group by group to 0 or 1
   group_by_group_int <- as.integer(group_by_group)

   # create parameters range file
   path_param_range <- write_param_range(data = param_range_df)
   file_param_range <- 'param_range.txt'
   path_param_range <- here::here(file_param_range)

   # create output file
   execute_bin(
      data_path, path_param_range, group_by_group_int, MCMC, interval, SEcal, saveprobevent,
      fitness, r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
   )

   # delete parameters range file after execution
   delete_param_range(file_param_range)

   # get path to output file
   filename_output <- sub("\\.txt$", "", data_path)
   path_to_output <- paste0(filename_output, ".out")
   
   # read output file and return results
   results <- read_output_from_file(path_to_output, group_by_group = group_by_group)
   return(results)
}