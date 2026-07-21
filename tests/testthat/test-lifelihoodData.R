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
    dist = c("wei", "gam", "exp")
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
  expect_true(all(lifelihoodData$dist == c("wei", "gam", "exp")))
  expect_true(all(lifelihoodData$covariates == c("par", "spore")))
  expect_true(!lifelihoodData$matclutch)
  expect_true(lifelihoodData$right_censoring_date == 1000)
  expect_true(lifelihoodData$critical_age == 20)
  expect_true(lifelihoodData$ratiomax == 10)
  expect_null(lifelihoodData$block)
})

test_that("as_lifelihoodData validates matclutch_size", {
  df <- data.frame(matclutch_size = 2)
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
    dist = c("wei", "gam", "lgn"),
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
    dist = c("wei", "gam", "lgn"),
    path_config = use_test_config("config_pierrick"),
    temp_dir = temp_dir
  )

  expect_identical(
    tail(readLines(path), 1),
    "0 sex 0 1 1 mat 2 3 4 pon 5 6 7 mor 8 9"
  )
})
