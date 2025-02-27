test_that("Parsing the seed values.", {
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
    "int_survival_shape -3.40298455 0.00000000",
    "eff_survival_shape_geno1 0.02515092 0.00000000",
    "eff_survival_shape_type1 0.92915890 0.00000000",
    "eff_survival_shape_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_seeds(lines)
  expect_equal(result, c(11, 22, 33, 44))

  lines <- c(
    "---------------------------",
    "",
    "datafile= /Users/josephbarbier/Desktop/Lifelihood/lifelihood_11_22_33_44/temp_file_data_lifelihood.txt",
    "seed1= 11 seed2= 4 seed3= 1 seed4= 5",
    "#parameters= 51",
    "Likelihood_max= -34138.741217",
    "int_expt_death -0.83129999 0.00000000",
    "eff_expt_death_geno1 0.92947536 0.00000000",
    "eff_expt_death_type1 1.30265247 0.00000000",
    "eff_expt_death_type2 -0.73748941 0.00000000",
    "int_survival_shape -3.40298455 0.00000000",
    "eff_survival_shape_geno1 0.02515092 0.00000000",
    "eff_survival_shape_type1 0.92915890 0.00000000",
    "eff_survival_shape_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_seeds(lines)
  expect_equal(result, c(11, 4, 1, 5))

  lines <- c(
    "---------------------------",
    "",
    "datafile= /Users/josephbarbier/Desktop/Lifelihood/lifelihood_11_22_33_44/temp_file_data_lifelihood.txt",
    "seed1= 100000 seed2= 15 seed3= 3 seed4= 2",
    "#parameters= 51",
    "Likelihood_max= -34138.741217",
    "int_expt_death -0.83129999 0.00000000",
    "eff_expt_death_geno1 0.92947536 0.00000000",
    "eff_expt_death_type1 1.30265247 0.00000000",
    "eff_expt_death_type2 -0.73748941 0.00000000",
    "int_survival_shape -3.40298455 0.00000000",
    "eff_survival_shape_geno1 0.02515092 0.00000000",
    "eff_survival_shape_type1 0.92915890 0.00000000",
    "eff_survival_shape_type2 0.16760327 0.00000000"
  )
  result <- lifelihood:::get_seeds(lines)
  expect_equal(result, c(100000, 15, 3, 2))
})
