
test_that("Default bounds df.", {
   df <- read.csv(here::here("data/fake_sample.csv"))
   df$type <- as.factor(df$type)
   df$geno <- as.factor(df$geno)

   clutchs <- c(
      "clutch_start1", "clutch_end1", "clutch_size1",
      "clutch_start2", "clutch_end2", "clutch_size2"
   )
   data <- lifelihoodData(
      df = df,
      sex = "sex",
      sex_start = "sex_start",
      sex_end = "sex_end",
      maturity_start = "mat_start",
      maturity_end = "mat_end",
      clutchs = clutchs,
      death_start = "mor_start",
      death_end = "mor_end",
      covariates = c("geno", "type"),
      model_specs = c("gam", "lgn", "wei")
   )

   bounds_df <- default_bounds_df(data)
   max_death <- as.numeric(bounds_df[bounds_df$param == "expt_death", "max"])
   expect_equal(max_death, 40.0)
})
