source(here::here('R', 'utils.R'))

execute_bin <- function(
   input_file,
   custom_file,
   group_by_group=0,
   MCMC=0,
   interval=25,
   SEcal=0, # std error calculation
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



# use case
execute_bin(
   input_file = here::here("data", "raw_data", "DataLenski", "DataLenski_gam_gam_gam__Rep1.txt"),
   custom_file = here::here("data", "custom.txt")
)
