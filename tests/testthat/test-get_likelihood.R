test_that("Parsing the likelihood value.", {
  lines <- c(
    "---------------------------",
    "",
    "datafile= /Users/josephbarbier/Desktop/Lifelihood/lifelihood_11_22_33_44/temp_file_data_lifelihood.txt",
    "seed1= 11 seed2= 22 seed3= 33 seed4= 44",
    "#parameters= 51",
    "Likelihood_max= -34138.741217",
    "int_expt_death -0.83129999 0.00000000",
    "eff_expt_death_geno1 0.92947536 0.00000000",
    "eff_expt_death_type1 1.30265247 0.00000000",
    "eff_expt_death_type2 -0.73748941 0.00000000",
    "int_survival_param2 -3.40298455 0.00000000",
    "eff_survival_param2_geno1 0.02515092 0.00000000",
    "eff_survival_param2_type1 0.92915890 0.00000000",
    "eff_survival_param2_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_likelihood(lines)
  expect_equal(result, -34138.741217)

  lines <- c(
    "---------------------------",
    "",
    "datafile= /Users/josephbarbier/Desktop/Lifelihood/lifelihood_11_22_33_44/temp_file_data_lifelihood.txt",
    "seed1= 11 seed2= 4 seed3= 1 seed4= 5",
    "#parameters= 51",
    "Likelihood_max= -1.0",
    "int_expt_death -0.83129999 0.00000000",
    "eff_expt_death_geno1 0.92947536 0.00000000",
    "eff_expt_death_type1 1.30265247 0.00000000",
    "eff_expt_death_type2 -0.73748941 0.00000000",
    "int_survival_param2 -3.40298455 0.00000000",
    "eff_survival_param2_geno1 0.02515092 0.00000000",
    "eff_survival_param2_type1 0.92915890 0.00000000",
    "eff_survival_param2_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_likelihood(lines)
  expect_equal(result, -1)

  lines <- c(
    "---------------------------",
    "",
    "datafile= /Users/josephbarbier/Desktop/Lifelihood/lifelihood_11_22_33_44/temp_file_data_lifelihood.txt",
    "seed1= 100000 seed2= 15 seed3= 3 seed4= 2",
    "#parameters= 51",
    "Likelihood_max= 42.3333",
    "int_expt_death -0.83129999 0.00000000",
    "eff_expt_death_geno1 0.92947536 0.00000000",
    "eff_expt_death_type1 1.30265247 0.00000000",
    "eff_expt_death_type2 -0.73748941 0.00000000",
    "int_survival_param2 -3.40298455 0.00000000",
    "eff_survival_param2_geno1 0.02515092 0.00000000",
    "eff_survival_param2_type1 0.92915890 0.00000000",
    "eff_survival_param2_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_likelihood(lines)
  expect_equal(result, 42.3333)

  lines <- c(
    "---------------------------",
    "",
    "datafile= /Users/josephbarbier/Desktop/Lifelihood/lifelihood_11_22_33_44/temp_file_data_lifelihood.txt",
    "seed1= 100000 seed2= 15 seed3= 3 seed4= 2",
    "#parameters= 51",
    "Likelihood_max= 10000.000",
    "int_expt_death -0.83129999 0.00000000",
    "eff_expt_death_geno1 0.92947536 0.00000000",
    "eff_expt_death_type1 1.30265247 0.00000000",
    "eff_expt_death_type2 -0.73748941 0.00000000",
    "int_survival_param2 -3.40298455 0.00000000",
    "eff_survival_param2_geno1 0.02515092 0.00000000",
    "eff_survival_param2_type1 0.92915890 0.00000000",
    "eff_survival_param2_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_likelihood(lines)
  expect_equal(result, 10000.000)
})
