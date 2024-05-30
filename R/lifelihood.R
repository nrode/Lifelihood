source(here('R', 'execute_bin.R'))
source(here('R', 'read_output.R'))

lifelihood <- function(
   data_path,
   custom_path,
   group_by_group=FALSE,
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
){

   # create output file
   execute_bin(path_to_data, path_to_custom, GbyG, MCMC, interval, SEcal, saveprobevent, fitness, r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision)

   # get path to output file
   filename <- sub("\\.txt$", "", data_path)
   path_to_output <- paste0(filename, ".out")
   
   # read output file and return results
   results <- read_output_from_file(path_to_output, group_by_group = group_by_group)
   return(results)
}