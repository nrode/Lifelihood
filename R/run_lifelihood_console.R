#' Run lifelihood program
#' @description Run lifelihood program in console mode
#' @param infile
#' @param customfile seed used so that random values can found again (https://r-coder.com/set-seed-r/)
#' @param n_strains Number of strains (e.g. number of strains used for the experiment)
#' @param n_replicates Number of replicates per type of cross
#' @param ref_hatch_rate Reference hatch rate for the intra-strain cross with strain 1 (ie. female strain 1 x male strain 1)
#' @param maternal_genvar_females Variance among female effects (due to both maternal and genetic effects)
#' @param genvar_males Variance among male effects (due to genetic effects)
#' @param heterosis Heterosis effect in embryos from inter-strain crosses
#' @param ci_level Average level of cytoplasmic incompatibility
#' @param var_ci Variance in cytoplasmic incompatibility among Wolbachia variants
#' @param var_within residual error
#'
#' @return dataset with the simulated hatch rate
#' @export
#'
#' @examples
#'run_lifelihood_console(infile="100%mort_Pierrickintercept.txt",ntr=10,To=50,Tf=50,seed2=34)

run_lifelihood_console<-function(infile="100%mort_Pierrick211genoparinteraction.txt",customfile="custom.txt",GbyG=0,MCMC=0,interval=25,SEcal=0,saveprobevent=0,fitness=0,r=0,seed1=12,seed2=13,seed3=14,seed4=15,ntr=2,nst=2,To=50,Tf=1,climbrate=1,precision=0.001)
  
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

