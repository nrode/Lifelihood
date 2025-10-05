test_that("Mapping effect name.", {
  result <- lifelihood:::map_parameter_name("eff_survival_param2_type2")
  expect_equal(result, "survival_param2")

  result <- lifelihood:::map_parameter_name("int_ratio_expt_maturity")
  expect_equal(result, "ratio_expt_maturity")

  result <- lifelihood:::map_parameter_name(
    "eff_increase_death_hazard_type2:geno1"
  )
  expect_equal(result, "increase_death_hazard")

  result <- lifelihood:::map_parameter_name(
    "eff_lin_decrease_hazard_type2:geno1"
  )
  expect_equal(result, "lin_decrease_hazard")

  result <- lifelihood:::map_parameter_name("eff_quad_change_n_offspring_geno1")
  expect_equal(result, "quad_change_n_offspring")
})
