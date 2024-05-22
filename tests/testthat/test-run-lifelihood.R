library(testthat)
library(mockery)
library(Lifelihood)

# Mocking the detect_os function
mock_detect_os <- function(os) {
  force(os)
  function() os
}
mock_detect_os("Unix")()

test_that("run_lifelihood constructs argument string correctly and calls the executable on Unix", {

  # Mocking the system function
  assignInNamespace("detect_os", mock_detect_os("Unix"), ns = "Lifelihood")
  system_mock <- mock()
  stub(run_lifelihood, "system", system_mock)
  
  # Setting input parameters
  input_file <- "data/input.txt"
  custom_file <- "data/custom.txt"
  GbyG <- 1
  MCMC <- 1
  interval <- 30
  SEcal <- 1
  saveprobevent <- 1
  fitness <- 1
  r <- 1
  seed1 <- 100
  seed2 <- 101
  seed3 <- 102
  seed4 <- 103
  ntr <- 3
  nst <- 4
  To <- 100
  Tf <- 5
  climbrate <- 2
  precision <- 0.01

  # Expected argument string
  expected_arg_string <- paste(
    input_file, custom_file, GbyG, MCMC, interval, SEcal, saveprobevent, fitness,
    r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
  )
  
  # Run the function
  run_lifelihood(
    input_file, custom_file, GbyG, MCMC, interval, SEcal, saveprobevent, fitness,
    r, seed1, seed2, seed3, seed4, ntr, nst, To, Tf, climbrate, precision
  )

  # Verify that the system function was called once
  expect_called(system_mock, 1)
  
  # Retrieve the arguments passed to the system function
  system_call <- mock_args(system_mock)[[1]]
  
  # Check the executable path and input string
  expect_equal(system_call[[1]], file.path(here("src", "compiled"), "lifelihoodC2023"))
  expect_equal(system_call[[2]], expected_arg_string)
})
