#' @title Prediction with lifelihood estimations
#' @description S3 method to use to make prediction using fitted results from [lifelihood()].
#' @name predict
#' @inheritParams check_valid_estimation
#' @param newdata Data for prediction. If absent, predictions are for the subjects used in the original fit.
#' @param type The type of the predicted value: if "response," it is on the original data scale; if "link," it is on the lifelihood scale.
#' @param se.fit Whether or not to include standard errors in the prediction.
#' @return prediction
#' @examples
#' df <- read.csv(here::here("data/fake_sample.csv"))
#' df$type <- as.factor(df$type)
#' df$geno <- as.factor(df$geno)
#'
#' clutchs <- c(
#'   "clutch_start1", "clutch_end1", "clutch_size1",
#'   "clutch_start2", "clutch_end2", "clutch_size2"
#' )
#'
#' dataLFH <- lifelihoodData(
#'   df = df,
#'   sex = "sex",
#'   sex_start = "sex_start",
#'   sex_end = "sex_end",
#'   maturity_start = "mat_start",
#'   maturity_end = "mat_end",
#'   clutchs = clutchs,
#'   death_start = "mor_start",
#'   death_end = "mor_end",
#'   covariates = c("geno", "type"),
#'   model_specs = c("gam", "lgn", "wei")
#' )
#'
#' results <- lifelihood(
#'   lifelihoodData = dataLFH,
#'   path_config = here::here("config2.yaml"),
#'   seeds = c(1, 2, 3, 4),
#'   raise_estimation_warning = FALSE
#' )
#'
#' predict(results, "expt_death")
#' predict(results, "expt_death", type = "response")
#'
#' # predict on new data
#' newdata <- data.frame(
#'   type = c(1, 2, 0, 1, 2, 0),
#'   geno = c(0, 1, 0, 1, 0, 1)
#' )
#' newdata$type <- factor(newdata$type)
#' newdata$geno <- factor(newdata$geno)
#' predict(results, "expt_death", newdata = newdata)
#' predict(results, "expt_death", newdata = newdata, type = "response")
#' @export
predict.LifelihoodResults <- function(
    lifelihoodResults,
    metric_name,
    newdata = NULL,
    type = c("link", "response"),
    se.fit = FALSE) {
  if (!inherits(lifelihoodResults, "LifelihoodResults")) {
    stop("lifelihoodResults must be of class LifelihoodResults")
  }

  type <- match.arg(type)

  df <- if (is.null(newdata)) lifelihoodResults$lifelihoodData$df else newdata
  df_train <- lifelihoodResults$lifelihoodData$df

  covariates <- lifelihoodResults$covariates

  if (!has_valid_factor_levels(df_train, df, covariates)) {
    stop("Invalid factor levels in newdata.")
  } else {
    effects <- lifelihoodResults$effects
    n_estimated <- nrow(subset(effects, metric == metric_name))

    metric_data <- which(effects$metric == metric_name)
    start <- metric_data[1]
    end <- tail(metric_data, n = 1)
    if (start == end) {
      range <- start
    } else {
      range <- start:end
    }

    fml <- read_formula(lifelihoodResults$config, metric_name)
    fml <- formula(paste("~ ", fml))
    m <- model.frame(fml, data = df)
    Terms <- stats::terms(m)
    predictions <- stats::model.matrix(Terms, m) %*% effects$estimation[range]

    if (type == "link") {
      pred <- predictions
    } else if (type == "response") {
      pred <- link(predictions, min_and_max = c(0.001, 40))
    }
  }
  return(pred)
}

#' @title Check for valid factor levels
#' @description The purpose of this function is to ensure that when a user makes a prediction with [lifelihood::predict()], the `newdata` contains the same factor levels for its covariates as the training data.
#'
#' If any levels are missing or mismatched, it raises an error and displays a warning.
#' @keywords internal
#' @param df_train Training set passed to [lifelihoodData()] (`df` arg).
#' @param newdata New data passed to [lifelihood::predict()] (`newdata` arg).
#' @param covariates Covariates passed to [lifelihoodData()] (`covariates` arg).
#' @name has_valid_factor_levels
#' @return TRUE if all factor levels of each covariate in `newdata` passed to [lifelihood::predict()] are present in the training data, FALSE otherwise.
has_valid_factor_levels <- function(df_train, newdata, covariates) {
  for (covariate in covariates) {
    if (covariate %in% names(df_train) && covariate %in% names(newdata)) {
      if (is.factor(df_train[[covariate]]) && is.factor(newdata[[covariate]])) {
        levels_train <- levels(df_train[[covariate]])
        levels_newdata <- levels(newdata[[covariate]])

        if (!all(levels_train %in% levels_newdata)) {
          return(FALSE)
        }
      } else {
        warning(paste("Column", covariate, "is not a factor in one of the dataframes."))
      }
    } else {
      warning(paste("Covariate", covariate, "not found in both dataframes."))
    }
  }

  return(TRUE)
}
