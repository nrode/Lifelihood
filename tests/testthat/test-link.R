test_that("link and delink are inverses", {
  min <- 0
  max <- 10
  estimates <- c(-5, 0, 5, 10)

  for (est in estimates) {
    linked <- link(est, min, max)
    unlinked <- delink(linked, min, max)
    expect_equal(unlinked, est, tolerance = 1e-8)
  }
})

test_that("delink and link are inverses", {
  min <- -2
  max <- 4
  original_values <- c(-1.5, 0, 1, 3.5)

  for (obs in original_values) {
    est <- delink(obs, min, max)
    back <- link(est, min, max)
    expect_equal(back, obs, tolerance = 1e-8)
  }
})

test_that("link behaves correctly at extremes", {
  expect_equal(link(-Inf, 0, 1), 0)
  expect_equal(link(Inf, 0, 1), 1)
  expect_equal(link(0, 0, 1), 0.5)
})

test_that("delink behaves correctly at boundaries", {
  expect_equal(delink(0.5, 0, 1), 0)
  expect_equal(delink(0.7310586, 0, 1), 1, tolerance = 1e-6)
})

test_that("derivLink matches numerical derivative of link", {
  min <- 0
  max <- 1
  est <- 0.5
  h <- 1e-6

  numeric_derivative <- (link(est + h, min, max) - link(est - h, min, max)) /
    (2 * h)
  analytical_derivative <- derivLink(est, min, max)

  expect_equal(analytical_derivative, numeric_derivative, tolerance = 1e-6)
})

test_that("link returns value in [min, max]", {
  min <- 2
  max <- 5
  ests <- c(-100, 0, 100)

  for (est in ests) {
    val <- link(est, min, max)
    expect_gte(val, min)
    expect_lte(val, max)
  }
})
