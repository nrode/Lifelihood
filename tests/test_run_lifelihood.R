source(file.path('R', 'run_lifelihood.R'))


# path to inputs
input_file = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   '100%mort_Pierrick211genoparinteraction.txt'
)
custom_file = file.path('data', 'custom.txt')

# run the program
execution_time <- system.time({
   run_lifelihood(
      input_file = input_file,
      custom_file = custom_file
   )
})
print("Execution time: ")
print(execution_time)




