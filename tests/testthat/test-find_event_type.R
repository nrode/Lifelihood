test_that("Finding event type.", {
  result <- lifelihood:::find_event_type("expt_death")
  expect_equal(result, "mortality")

  result <- lifelihood:::find_event_type("survival_param2")
  expect_equal(result, "mortality")

  result <- lifelihood:::find_event_type("prob_death")
  expect_equal(result, "mortality")

  result <- lifelihood:::find_event_type("expt_reproduction")
  expect_equal(result, "reproduction")

  result <- lifelihood:::find_event_type("reproduction_param2")
  expect_equal(result, "reproduction")

  result <- lifelihood:::find_event_type("n_offspring")
  expect_equal(result, "reproduction")

  result <- lifelihood:::find_event_type("expt_maturity")
  expect_equal(result, "maturity")

  result <- lifelihood:::find_event_type("maturity_param2")
  expect_equal(result, "maturity")

  result <- lifelihood:::find_event_type("ratio_expt_maturity")
  expect_equal(result, "maturity")
})
