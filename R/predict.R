#' @name predict
#' @param newdata jeu de données avec une ou plusieurs lignes à prédire, avec les mêmes nom de variables. Si facteur, il faut les mêmes les niveaux de facteurs (renvoyer erreur si inconnu: pas présent de base dans les données d'entrainement/initiales).
#' @param type Si "link": format de lifelihood, si "response": format original.
#' @title prediction
predict.LifelihoodResults <- function(
    lifelihoodResults,
    newdata = NULL,
    type = c("link", "response"),
    se.fit = FALSE) {
  if (!inherits(lifelihoodResults, "lifelihoodResults")) {
    stop("lifelihoodResults must be of class lifelihoodResults")
  }

  type <- match.arg(type)
  data <- ifelse(is.null(newdata), lifelihoodResults$lifelihoodData, newdata)

  effects <- lifelihoodResults$effects
  fitted_metrics <- unique(effects$metric)

  break_points <- c()
  for (metric_name in fitted_metrics) {
    break_points <- c(break_points, nrow(subset(effects, metric == metric_name)))
  }
  print(break_points)

  m <- model.frame(~ geno * type, data = df)
  Terms <- terms(m)
  predicted <- model.matrix(Terms, m) %*% results$effects$estimation[1:6]
  pred_expdeath <- link(predicted, min_and_max = c(0.001, 40))
}

# produit matriciel matrice de design et coefficients (échelle lifelihood)
# X <- model.matrix(Terms, m)

# drop
# stats::predict.glm

# predict.lm()

# terms(m)
