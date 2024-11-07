#' @title Prediction with lifelihood
#' @name predict
#' @param newdata jeu de données avec une ou plusieurs lignes à prédire, avec les mêmes nom de variables. Si facteur, il faut les mêmes les niveaux de facteurs (renvoyer erreur si inconnu: pas présent de base dans les données d'entrainement/initiales).
#' @param type Si "link": format de lifelihood, si "response": format original.
#' @return prediction
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
  return(pred)
}

# produit matriciel matrice de design et coefficients (échelle lifelihood)
# X <- model.matrix(Terms, m)

# drop
# stats::predict.glm

# predict.lm()

# terms(m)
