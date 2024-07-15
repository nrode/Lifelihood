
compile <- function(
   input_file = "src/source_pascal/lazarus/lifelihoodC2023.lpr",
   output_dir_bin = "src/compiled/",
   output_dir_other = "-FUsrc/source_pascal/lazarus/"
){
   
   # create command to run based on inputs
   command <- glue::glue(
      "fpc -FE",
      output_dir_bin, " ",
      output_dir_other, " ",
      input_file
   )
   
   # print and run command in console
   print(command)
   system(command)
}
