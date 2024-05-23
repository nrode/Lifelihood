library(glue)

get_seeds = function(
   lines,
   group_by_group=FALSE
){

   seeds_line <- lines[grepl("seed1=", lines)]
   seeds <- as.numeric(unlist(strsplit(sub(".*seed1=\\s*(\\d+) seed2=\\s*(\\d+) seed3=\\s*(\\d+) seed4=\\s*(\\d+).*", "\\1,\\2,\\3,\\4", seeds_line), ",")))

   if (group_by_group){

      # 4 seeds per group
      n_groups <- length(seeds)/4 

      # ensure that the number of groups is an integer
      if (n_groups != round(n_groups)){
         message(glue("Number of groups seems weird: ", n_groups))
      }

      # create a list of seeds for each group
      seeds_gbg <- list()

      # loop over the groups
      for (i in 1:n_groups){
         
         # store the seeds for the group
         start <- 4*(i-1)+1
         end <- 4*i
         seeds_gbg[[i]] <- seeds[start:end]
      }
      return(seeds_gbg)
   }
   else {
      return(seeds)
   }
}

get_likelihood = function(
   lines,
   group_by_group=FALSE
){

   if (group_by_group){
      
      # find the lines starting with pattern "group \d+ Likelihood_max="
      likelihood_lines <- lines[grepl("group \\d+ Likelihood_max=", lines)]
      
      # retrive the likelihood values
      likelihoods <- as.numeric(unlist(strsplit(sub(".*group \\d+ Likelihood_max=\\s*(-?\\d+\\.\\d+).*", "\\1", likelihood_lines), " ")))

      n_groups <- length(lines[grepl("datafile=", lines)])
      likelihoods <- matrix(likelihoods, ncol=n_groups)
      return(likelihoods)

   }
   else {

      # find the line starting with "Likelihood_max="
      likelihood_line <- lines[grepl("Likelihood_max=", lines)]

      # retrieve the likelihood value
      likelihood <- as.numeric(sub("Likelihood_max=\\s*(-?\\d+\\.\\d+)", "\\1", likelihood_line))
      return(likelihood)
   }
}



# Example use case
file_path1 = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   '100%mort_Pierrick211genoparinteraction.out'
)
file_path2 = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   'resultgroupbygroup.out'
)

get_seeds(
   readLines(file_path1)
)
get_seeds(
   readLines(file_path2),
   group_by_group=TRUE
)

get_likelihood(
   readLines(file_path1)
)
get_likelihood(
   readLines(file_path2),
   group_by_group=TRUE
)



