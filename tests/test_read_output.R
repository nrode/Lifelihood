source(file.path('R', 'read_output.R'))

# path to input
file_path = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   '100%mort_Pierrick211genoparinteraction.out'
)

# read the output and print the results
fitted = read_output_from_file(file_path)
fitted$datafile
fitted$seeds
fitted$likelihood_max
head(fitted$effects)
head(fitted$parameter_ranges)