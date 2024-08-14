test_that("Finding parameter kind.", {
  result <- lifelihood:::find_parameter_kind("eff_survival_shape_type2")
  expect_equal(result, "coefficient")


  result <- lifelihood:::find_parameter_kind("int_quad_senescence")
  expect_equal(result, "intercept")


  result <- lifelihood:::find_parameter_kind("eff_ratio_expt_death_geno1")
  expect_equal(result, "coefficient")


  result <- lifelihood:::find_parameter_kind("int_prob_death")
  expect_equal(result, "intercept")


  result <- lifelihood:::find_parameter_kind("eff_increase_death_hazard_type2:geno1")
  expect_equal(result, "coefficient")
})
