test_that("reproduction visit masks merge clutches within intervals", {
  lifelihoodData <- list(
    block = "block",
    df = tibble(block = c("a", "a", "b", "b"))
  )
  visits <- tibble(
    block = c(rep("a", 3), rep("b", 3)),
    visit = c(0, 5, 10, 0, 10, 20)
  )
  simulated <- tibble(
    mortality = c(20, 20, 20, 20),
    clutch_1 = c(2, 2, NA, 12),
    clutch_size_1 = c(2L, NA_integer_, NA_integer_, 4L),
    clutch_2 = c(4, 7, NA, 15),
    clutch_size_2 = c(3L, 5L, NA_integer_, NA_integer_),
    clutch_3 = c(8, NA, NA, NA),
    clutch_size_3 = c(4L, NA_integer_, NA_integer_, NA_integer_)
  )

  masked <- add_visit_masks(
    simul_df = simulated,
    lifelihoodData = lifelihoodData,
    event = "reproduction",
    visits = visits
  )

  expect_equal(masked$mortality, simulated$mortality)
  expect_equal(masked$clutch_1, c(2, 2, NA, 12))
  expect_equal(masked$clutch_start_1, c(0, 0, NA, 10))
  expect_equal(masked$clutch_end_1, c(5, 5, NA, 20))
  expect_equal(masked$clutch_size_1, c(5L, NA_integer_, NA_integer_, 4L))
  expect_equal(masked$clutch_2, c(8, 7, NA, NA))
  expect_equal(masked$clutch_start_2, c(5, 5, NA, NA))
  expect_equal(masked$clutch_end_2, c(10, 10, NA, NA))
  expect_equal(masked$clutch_size_2, c(4L, 5L, NA_integer_, NA_integer_))
  expect_false(any(grepl("^clutch_3$", names(masked))))
})

test_that("reproduction visit masks reject ages outside visits", {
  lifelihoodData <- list(
    block = "block",
    df = tibble(block = "a")
  )
  simulated <- tibble(
    clutch_1 = -1,
    clutch_size_1 = 2L,
    clutch_2 = 12,
    clutch_size_2 = 3L
  )

  expect_error(
    add_visit_masks(
      simul_df = simulated,
      lifelihoodData = lifelihoodData,
      event = "reproduction",
      visits = tibble(block = "a", visit = c(0, 5, 10))
    ),
    "reproduction.*block `a`.*outside.*0.*10.*-1"
  )

  expect_error(
    add_visit_masks(
      simul_df = tibble(clutch_1 = 12, clutch_size_1 = 3L),
      lifelihoodData = lifelihoodData,
      event = "reproduction",
      visits = tibble(block = "a", visit = c(0, 5, 10))
    ),
    "reproduction.*block `a`.*outside.*0.*10.*12"
  )
})

test_that("scalar visit masks preserve input row order across blocks", {
  lifelihoodData <- list(
    block = "block",
    df = tibble(block = c("b", "a", "b", "a"))
  )
  visits <- tibble(
    block = c("a", "a", "b", "b"),
    visit = c(0, 5, 0, 10)
  )
  simulated <- tibble(mortality = c(3, 2, 7, 4))

  masked <- add_visit_masks(
    simul_df = simulated,
    lifelihoodData = lifelihoodData,
    event = "mortality",
    visits = visits,
    block_values = c("b", "a", "b", "a")
  )

  # Rows must stay in input order, not be re-sorted by block.
  expect_equal(masked$mortality, c(3, 2, 7, 4))
  # Bounds are computed against each row's own block visits.
  expect_equal(masked$mortality_start, c(0, 0, 0, 0))
  expect_equal(masked$mortality_end, c(10, 5, 10, 5))
})

test_that("scalar visit masks handle NA event ages", {
  lifelihoodData <- list(
    block = "block",
    df = tibble(block = c("a", "a"))
  )
  visits <- tibble(block = c("a", "a", "a"), visit = c(0, 5, 10))
  simulated <- tibble(mortality = c(NA_real_, 7))

  masked <- add_visit_masks(
    simul_df = simulated,
    lifelihoodData = lifelihoodData,
    event = "mortality",
    visits = visits,
    block_values = c("a", "a")
  )

  expect_true(is.na(masked$mortality_start[1]))
  expect_true(is.na(masked$mortality_end[1]))
  expect_equal(masked$mortality_start[2], 5)
  expect_equal(masked$mortality_end[2], 10)
})

test_that("visit masks allow event ages at visit boundaries", {
  lifelihoodData <- list(
    block = "block",
    df = tibble(block = c("a", "a"))
  )
  masked <- add_visit_masks(
    simul_df = tibble(mortality = c(0, 10)),
    lifelihoodData = lifelihoodData,
    event = "mortality",
    visits = tibble(block = "a", visit = c(0, 5, 10))
  )

  expect_equal(masked$mortality_start, c(0, 10))
  expect_equal(masked$mortality_end, c(5, NA_real_))
})

test_that("scalar visit masks reject ages outside visits", {
  lifelihoodData <- list(
    block = "block",
    df = tibble(block = "a")
  )
  visits <- tibble(block = "a", visit = c(0, 5, 10))

  expect_error(
    add_visit_masks(
      simul_df = tibble(mortality = -1),
      lifelihoodData = lifelihoodData,
      event = "mortality",
      visits = visits
    ),
    "mortality.*block `a`.*outside.*0.*10.*-1"
  )

  expect_error(
    add_visit_masks(
      simul_df = tibble(mortality = 11),
      lifelihoodData = lifelihoodData,
      event = "mortality",
      visits = visits
    ),
    "mortality.*block `a`.*outside.*0.*10.*11"
  )
})
