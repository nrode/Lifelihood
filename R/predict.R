#' @name predict
#' @param newdata jeu de données avec une ou plusieurs lignes à prédire, avec les mêmes nom de variables. Si facteur, il faut les mêmes les niveaux de facteurs (renvoyer erreur si inconnu: pas présent de base dans les données d'entrainement/initiales).
#' @param type Si link: format de lifelihood, si response: format delink.
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
}

stats::predict.glm
