fake_lifelihood_binary <- function(output, status = 0) {
  path <- tempfile(
    "lifelihood-",
    fileext = if (.Platform$OS.type == "windows") ".bat" else ".sh"
  )

  if (.Platform$OS.type == "windows") {
    script <- c(
      "@echo off",
      paste0("echo ", output),
      paste0("exit /b ", status)
    )
  } else {
    script <- c(
      "#!/bin/sh",
      paste0("printf '%s\\n' ", shQuote(output)),
      paste0("exit ", status)
    )
  }

  writeLines(script, path)
  Sys.chmod(path, "0755")
  path
}

run_fake_lifelihood_binary <- function(path) {
  execute_bin(
    path_to_Lifelihood = path,
    path_input_data = "input.txt",
    path_param_bounds = "bounds.txt",
    MCMC = 0,
    interval = 0,
    se.fit = FALSE,
    saveprobevent = FALSE,
    fitness = FALSE,
    r = 0,
    seed1 = 1,
    seed2 = 2,
    seed3 = 3,
    seed4 = 4,
    ratiomax = 10,
    tc = 0,
    tinf = 100,
    sub_interval = 1,
    path_continuous_var = "",
    ntr = 1,
    nst = 1,
    To = 1,
    Tf = 0,
    climbrate = 0.95,
    precision = 1e-6
  )
}

test_that("execute_bin reports Pascal sentinel errors", {
  path <- fake_lifelihood_binary(c(
    "before error",
    "Lifelihood Pascal error",
    "invalid parameter bounds"
  ))

  expect_error(
    run_fake_lifelihood_binary(path),
    "invalid parameter bounds"
  )
})

test_that("execute_bin reports nonzero Pascal exit status", {
  path <- fake_lifelihood_binary("failed without sentinel", status = 2)

  expect_error(
    run_fake_lifelihood_binary(path),
    "failed without sentinel"
  )
})
