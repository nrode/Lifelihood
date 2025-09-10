#' @title Prediction with lifelihood estimations
#'
#' @description
#' S3 method to use to make prediction using fitted results from [lifelihood()].
#'
#' @param object output of [lifelihood()]
#' @param parameter_name A string specifying the name of the parameter for which to make the prediction. Must be one of `unique(lifelihoodResults$effects$parameter)`.
#' @param newdata Data for prediction. If absent, predictions are for the subjects used in the original fit.
#' @param type The type of the predicted value: if "response," it is on the original data scale; if "link," it is on the lifelihood scale.
#' @param se.fit Whether or not to include standard errors in the prediction.
#'
#' @return A vector containing the predicted values for the parameter.
#'
#' @importFrom stats as.formula formula model.frame model.matrix terms
#'
#' @examples
#' df <- fakesample |>
#'   mutate(
#'     geno = as.factor(geno),
#'     type = as.factor(type)
#'   )
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
#'   path_config = get_config_path("config2"),
#'   seeds = c(1, 2, 3, 4),
#'   raise_estimation_warning = FALSE
#' )
#'
#' prediction(results, "expt_death")
#' prediction(results, "expt_death", type = "response")
#'
#' # predict on new data
#' newdata <- data.frame(
#'   type = c(1, 2, 0, 1, 2, 0),
#'   geno = c(0, 1, 0, 1, 0, 1)
#' )
#' newdata$type <- factor(newdata$type)
#' newdata$geno <- factor(newdata$geno)
#' prediction(results, "expt_death", newdata)
#' prediction(results, "expt_death", newdata, type = "response")
#' @export
prediction <- function(
  object,
  parameter_name,
  newdata = NULL,
  type = c("link", "response"),
  se.fit = FALSE
) {
  check_valid_lifelihoodResults(object)

  type <- match.arg(type)

  df <- if (is.null(newdata)) object$lifelihoodData$df else newdata
  original_df <- object$lifelihoodData$df

  covariates <- object$formula[[parameter_name]]

  if (!has_valid_factor_levels(original_df, df, covariates)) {
    stop(
      "Invalid factor levels in new data.
      This error occurs when the factor levels in the
      covariate columns of the new data differ from
      those in the data used to fit the model."
    )
  } else {
    effects <- object$effects

    parameter_data <- which(effects$parameter == parameter_name)
    range <- which(effects$parameter == parameter_name)

    fml <- read_formula(config = object$config, parameter = parameter_name)
    fml <- formula(paste("~ ", fml))
    m <- model.frame(fml, data = df)
    Terms <- terms(m)
    x <- model.matrix(Terms, m)
    coef_vector <- effects$estimation[range]

    # the case where newdata does not contain all
    # possible factors: we add them and put to 0.
    if (ncol(x) != length(coef_vector)) {
      orig_m <- model.frame(fml, data = original_df)
      orig_x <- model.matrix(Terms, orig_m)
      missing_cols <- setdiff(colnames(orig_x), colnames(x))
      for (col in missing_cols) {
        x <- cbind(x, rep(0, nrow(x)))
        colnames(x)[ncol(x)] <- col
      }
      x <- x[, colnames(orig_x), drop = FALSE]
      if (ncol(x) != length(coef_vector)) {
        stop(
          paste0(
            "Dimension mismatch after adding missing factor levels: design matrix has ",
            ncol(x),
            " columns but coefficient vector has ",
            length(coef_vector),
            " elements."
          )
        )
      }
    }
    predictions <- x %*% coef_vector

    if (se.fit) {
      vcov <- object$vcov
      if (type == "link") {
        se <- sqrt(diag(x %*% vcov %*% t(x)))
      } else {
        se <- sqrt(diag(x %*% vv %*% t(x)) * (derivLink(estimate, min, max)^2))
      }
    }

    if (type == "link") {
      pred <- predictions
    } else if (type == "response") {
      bounds_df <- object$param_bounds_df
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
#'
#' @description
#' The purpose of this function is to ensure that when a user
#' makes a prediction with [lifelihood::predict()], the `newdata`
#' contains the same factor levels for its covariates as the
#' training data.
#' If any levels are missing or mismatched, it raises an error
#' and displays a warning.
#'
#' @keywords internal
#'
#' @param original_df Training set passed to [lifelihoodData()] (`df` arg).
#' @param newdata New data passed to [lifelihood::predict()] (`newdata` arg).
#' @param covariates Covariates passed to [lifelihoodData()] (`covariates` arg).
#'
#' @return TRUE if all factor levels of each covariate in `newdata` passed to [lifelihood::predict()] are present in the training data, FALSE otherwise.
has_valid_factor_levels <- function(original_df, newdata, covariates) {
  if (length(covariates) == 0) {
    warning("covariates argument is empty")
    return(FALSE)
  }

  for (covariate in covariates) {
    levels_train <- levels(as.factor(original_df[[covariate]]))
    levels_newdata <- levels(newdata[[covariate]])
    if (!all(levels_newdata %in% levels_train)) {
      return(FALSE)
    }
  }

  return(TRUE)
}
