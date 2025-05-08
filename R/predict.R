#' @title Prediction with lifelihood estimations
#' @description S3 method to use to make prediction using fitted results from [lifelihood()].
#' @name predict
#' @inheritParams check_valid_estimation
#' @param parameter_name A string specifying the name of the parameter for which to make the prediction. Must be one of `unique(lifelihoodResults$effects$parameter)`.
#' @param newdata Data for prediction. If absent, predictions are for the subjects used in the original fit.
#' @param type The type of the predicted value: if "response," it is on the original data scale; if "link," it is on the lifelihood scale.
#' @param se.fit Whether or not to include standard errors in the prediction.
#' @return A vector containing the predicted values for the parameter.
#' @examples
#' df <- read.csv(here::here("data_internals/fake_sample.csv"))
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
#'   death_start = "death_start",
#'   death_end = "death_end",
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
#' predict(results, "expt_death", newdata)
#' predict(results, "expt_death", newdata, type = "response")
#' @export
predict.lifelihoodResults <- function(
  lifelihoodResults,
  parameter_name,
  newdata = NULL,
  type = c("link", "response"),
  se.fit = FALSE
) {
  if (!inherits(lifelihoodResults, "lifelihoodResults")) {
    stop("lifelihoodResults must be of class lifelihoodResults")
  }

  type <- match.arg(type)

  df <- if (is.null(newdata)) lifelihoodResults$lifelihoodData$df else newdata
  original_df <- lifelihoodResults$lifelihoodData$df

  covariates <- lifelihoodResults$covariates

  if (!has_valid_factor_levels(original_df, df, covariates)) {
    stop(
      "Invalid factor levels in new data.
      This error occurs when the factor levels in the
      covariate columns of the new data differ from
      those in the data used to fit the model."
    )
  } else {
    effects <- lifelihoodResults$effects

    parameter_data <- which(effects$parameter == parameter_name)
    range <- parameter_data[1]:parameter_data[length(parameter_data)]

    fml <- read_formula(lifelihoodResults$config, parameter_name)
    fml <- formula(paste("~ ", fml))
    m <- model.frame(fml, data = df)
    Terms <- stats::terms(m)
    x <- stats::model.matrix(Terms, m)
    predictions <- x %*% effects$estimation[range]

    if (se.fit) {
      vcov <- lifelihoodResults$vcov
      print("dim vcov")
      print(dim(vcov))
      cat("\n")
      print("dim x")
      print(dim(x))
      cat("\n")
      print("x")
      print(x)
      if (type == "link") {
        se <- sqrt(diag(x %*% vcov %*% t(x)))
      } else {
        se <- sqrt(diag(x %*% vv %*% t(x)) * (derivLink(estimate, min, max)^2))
      }
    }

    if (type == "link") {
      pred <- predictions
    } else if (type == "response") {
      bounds_df <- lifelihoodResults$param_bounds_df
      parameter_bounds <- subset(bounds_df, param == parameter_name)
      pred <- link(
        predictions,
        min = as.numeric(parameter_bounds$min),
        max = as.numeric(parameter_bounds$max)
      )
    }
  }
  return(pred)
}

#' @title Check for valid factor levels
#' @description The purpose of this function is to ensure that when a user makes a prediction with [lifelihood::predict()], the `newdata` contains the same factor levels for its covariates as the training data.
#'
#' If any levels are missing or mismatched, it raises an error and displays a warning.
#' @keywords internal
#' @param original_df Training set passed to [lifelihoodData()] (`df` arg).
#' @param newdata New data passed to [lifelihood::predict()] (`newdata` arg).
#' @param covariates Covariates passed to [lifelihoodData()] (`covariates` arg).
#' @name has_valid_factor_levels
#' @return TRUE if all factor levels of each covariate in `newdata` passed to [lifelihood::predict()] are present in the training data, FALSE otherwise.
has_valid_factor_levels <- function(original_df, newdata, covariates) {
  if (length(covariates) == 0) {
    warning("covariates argument is empty")
    return(FALSE)
  }

  # for (colname in names(newdata)) {
  #   if (!(colname %in% covariates)) {
  #     stop(paste("Unknown column in newdata: ", colname))
  #   }
  # }

  for (covariate in covariates) {
    levels_train <- levels(original_df[[covariate]])
    levels_newdata <- levels(newdata[[covariate]])
    if (!all(levels_newdata %in% levels_train)) {
      return(FALSE)
    }
  }

  return(TRUE)
}
