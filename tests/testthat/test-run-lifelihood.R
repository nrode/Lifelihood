library(testthat)
library(here)

test_that("run_lifelihood runs with default parameters", {
  # Define the input files
  input_file = file.path(
   'data',
   'raw_data',
   'DataPierrick_GroupbyGroup',
   '100%mort_Pierrick211genoparinteraction.txt'
  )
  custom_file = file.path('data', 'custom.txt')

  # Mock the detect_os function to return the actual OS
  detect_os <- function() {
    if (.Platform$OS.type == "windows") {
      return("Windows")
    } else {
      return("Unix-like")
    }
  }

  # Run the function with default parameters
  expect_silent(run_lifelihood(
    input_file = input_file,
    custom_file = custom_file
  ))

  # Check if the expected path is being used
  if (detect_os() == "Windows") {
    expected_path <- file.path(here("src", "compiled"), "lifelihoodC2023.exe")
  } else {
    expected_path <- file.path(here("src", "compiled"), "lifelihoodC2023")
  }

  # Check if the file exists (assuming the compiled program is present)
  expect_true(file.exists(expected_path))
})
