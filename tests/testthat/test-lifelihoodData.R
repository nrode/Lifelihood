test_that("as_lifelihoodData works", {
  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    )

  clutchs <- generate_clutch_vector(28)

  lifelihoodData <- as_lifelihoodData(
    df = df,
    matclutch = FALSE,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("par", "spore"),
    dist = data.frame(mortality = "wei", maturity = "gam", reproduction = "exp")
  )

  expect_true(!is.null(lifelihoodData$df))
  expect_true(lifelihoodData$sex == "sex")
  expect_true(lifelihoodData$sex_start == "sex_start")
  expect_true(lifelihoodData$sex_end == "sex_end")
  expect_true(lifelihoodData$maturity_start == "mat_start")
  expect_true(lifelihoodData$maturity_end == "mat_end")
  expect_true(all(lifelihoodData$clutchs == clutchs))
  expect_true(lifelihoodData$death_start == "death_start")
  expect_true(lifelihoodData$death_end == "death_end")
  expect_identical(
    lifelihoodData$dist,
    data.frame(mortality = "wei", maturity = "gam", reproduction = "exp")
  )
  expect_true(all(lifelihoodData$covariates == c("par", "spore")))
  expect_true(!lifelihoodData$matclutch)
  expect_true(lifelihoodData$right_censoring_date == 1000)
  expect_true(lifelihoodData$critical_age == 20)
  expect_true(lifelihoodData$ratiomax == 10)
  expect_null(lifelihoodData$block)
})

test_that("distribution data frames are validated and ordered by event", {
  dist <- data.frame(
    reproduction = factor(c("exp", "lgn")),
    maturity = c("gam", "wei"),
    mortality = c("wei", "gam")
  )
  expect_identical(
    validate_dist(dist),
    data.frame(
      mortality = c("wei", "gam"),
      maturity = c("gam", "wei"),
      reproduction = c("exp", "lgn")
    )
  )

  invalid <- list(
    c(mortality = "wei", maturity = "gam", reproduction = "exp"),
    dist[FALSE, ],
    dist[, FALSE],
    transform(dist, extra = "wei"),
    setNames(dist, c("mortality", "mortality", "reproduction")),
    setNames(dist, c("reproduction", "maturity", "other")),
    transform(dist, mortality = c("wei", "bad")),
    transform(dist, mortality = c("wei", NA_character_)),
    transform(dist, mortality = 1)
  )
  for (value in invalid) {
    expect_error(
      validate_dist(value),
      "`dist` must be a non-empty data frame",
      fixed = TRUE
    )
  }
})

test_that("distribution data frames may select a subset of events", {
  dist <- data.frame(
    reproduction = "wei",
    mortality = "gam"
  )

  expect_identical(
    validate_dist(dist),
    data.frame(mortality = "gam", reproduction = "wei")
  )
  expect_identical(
    validate_dist(data.frame(maturity = "exp")),
    data.frame(maturity = "exp")
  )
  expect_true(
    is.list(validate_config_input(list(), data.frame(mortality = "wei")))
  )
})

test_that("event columns are required only for fitted events and dependencies", {
  df <- data.frame(
    death_start = 1,
    death_end = 2,
    clutch_start = 1,
    clutch_end = 2,
    clutch_size = 1
  )
  common <- list(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    covariates = character(),
    matclutch = FALSE
  )

  mortality_data <- do.call(
    as_lifelihoodData,
    c(
      common,
      list(
        dist = data.frame(mortality = "wei"),
        death_start = "death_start",
        death_end = "death_end"
      )
    )
  )
  expect_null(mortality_data$maturity_start)
  expect_null(mortality_data$clutchs)

  expect_error(
    do.call(
      as_lifelihoodData,
      c(
        common,
        list(
          dist = data.frame(maturity = "wei")
        )
      )
    ),
    "Both interval column names are required when fitting maturity",
    fixed = TRUE
  )

  expect_error(
    do.call(
      as_lifelihoodData,
      c(
        common,
        list(
          dist = data.frame(reproduction = "wei"),
          maturity_start = "maturity_start",
          maturity_end = "maturity_end",
          death_start = "death_start",
          death_end = "death_end",
          clutchs = c("clutch_start", "clutch_end", "clutch_size")
        )
      )
    ),
    "Column(s) required for fitting maturity are missing from `df`",
    fixed = TRUE
  )
})

test_that("as_lifelihoodData validates matclutch_size", {
  df <- data.frame(
    matclutch_size = 2,
    maturity_start = 1,
    maturity_end = 1,
    death_start = 1,
    death_end = 1
  )
  args <- list(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "maturity_start",
    maturity_end = "maturity_end",
    clutchs = character(),
    death_start = "death_start",
    death_end = "death_end",
    dist = data.frame(mortality = "wei", maturity = "gam"),
    covariates = character()
  )

  expect_warning(
    do.call(
      as_lifelihoodData,
      c(args, list(matclutch = FALSE, matclutch_size = "matclutch_size"))
    ),
    "ignored when `matclutch` is FALSE",
    fixed = TRUE
  )

  expect_error(
    do.call(as_lifelihoodData, c(args, list(matclutch = TRUE))),
    "cannot be NULL",
    fixed = TRUE
  )

  expect_error(
    do.call(
      as_lifelihoodData,
      c(args, list(matclutch = TRUE, matclutch_size = "unknown"))
    ),
    "must name a column in `df`",
    fixed = TRUE
  )

  args$df$matclutch_size <- as.character(args$df$matclutch_size)
  expect_error(
    do.call(
      as_lifelihoodData,
      c(args, list(matclutch = TRUE, matclutch_size = "matclutch_size"))
    ),
    "must refer to a numeric column in `df`",
    fixed = TRUE
  )
})

test_that("matclutch is retained when reproduction requires maturity", {
  df <- data.frame(
    sex = 1,
    sex_start = 0,
    sex_end = 1,
    maturity_start = 2,
    maturity_end = 2,
    death_start = 3,
    death_end = 3,
    clutch_start = 2,
    clutch_end = 2,
    clutch_size = 1
  )
  data <- as_lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "maturity_start",
    maturity_end = "maturity_end",
    death_start = "death_start",
    death_end = "death_end",
    clutchs = c("clutch_start", "clutch_end", "clutch_size"),
    matclutch = TRUE,
    matclutch_size = "clutch_size",
    covariates = character(),
    dist = data.frame(mortality = "wei", reproduction = "wei")
  )

  expect_true(data$matclutch)
  expect_identical(data$matclutch_size, "clutch_size")
  expect_error(
    as_lifelihoodData(
      df = df,
      sex = "sex",
      sex_start = "sex_start",
      sex_end = "sex_end",
      maturity_start = "maturity_start",
      maturity_end = "maturity_end",
      death_start = "death_start",
      death_end = "death_end",
      clutchs = c("clutch_start", "clutch_end", "clutch_size"),
      matclutch = TRUE,
      matclutch_size = NULL,
      covariates = character(),
      dist = data.frame(mortality = "wei", reproduction = "wei")
    ),
    "cannot be NULL",
    fixed = TRUE
  )

  expect_warning(
    mortality_data <- as_lifelihoodData(
      df = df,
      sex = "sex",
      sex_start = "sex_start",
      sex_end = "sex_end",
      death_start = "death_start",
      death_end = "death_end",
      matclutch = TRUE,
      matclutch_size = "clutch_size",
      covariates = character(),
      dist = data.frame(mortality = "wei")
    ),
    "neither maturity nor reproduction is fitted",
    fixed = TRUE
  )
  expect_false(mortality_data$matclutch)
})

test_that("the intermediate file includes matclutch_size", {
  df <- data.frame(
    par = 0,
    sex_start = 0,
    sex_end = 1,
    sex = 1,
    maturity_start = 2,
    maturity_end = 3,
    matclutch_size = 4,
    clutch_start = 5,
    clutch_end = 6,
    clutch_size = 7,
    death_start = 8,
    death_end = 9
  )
  temp_dir <- tempfile("lifelihood-matclutch-")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  path <- format_dataframe_to_txt(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "maturity_start",
    maturity_end = "maturity_end",
    matclutch = TRUE,
    matclutch_size = "matclutch_size",
    clutchs = c("clutch_start", "clutch_end", "clutch_size"),
    death_start = "death_start",
    death_end = "death_end",
    covariates = "par",
    dist = data.frame(
      mortality = "wei",
      maturity = "gam",
      reproduction = "lgn"
    ),
    config = validate_config_input(list(
      mortality = list(expt_death = "par", survival_param2 = 1),
      maturity = list(expt_maturity = 1, maturity_param2 = 1),
      reproduction = list(
        expt_reproduction = 1,
        reproduction_param2 = 1,
        n_offspring = 1
      )
    )),
    temp_dir = temp_dir
  )

  expect_identical(
    tail(readLines(path), 1),
    "0 sex 0 1 1 mat 2 3 4 pon 5 6 7 mor 8 9"
  )
})

test_that("the intermediate file fills absent event slots", {
  df <- data.frame(
    sex = 1,
    sex_start = 0,
    sex_end = 1,
    death_start = 8,
    death_end = 9
  )
  temp_dir <- tempfile("lifelihood-optional-events-")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  path <- format_dataframe_to_txt(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = NULL,
    maturity_end = NULL,
    matclutch = FALSE,
    matclutch_size = NULL,
    clutchs = NULL,
    death_start = "death_start",
    death_end = "death_end",
    covariates = character(),
    dist = data.frame(mortality = "wei"),
    config = validate_config_input(list()),
    temp_dir = temp_dir,
    right_censoring_date = 100
  )

  lines <- readLines(path)
  expect_true(any(lines == "wei exp exp"))
  expect_match(tail(lines, 1), "mat 100 100 mor 8 9$", perl = TRUE)
})
