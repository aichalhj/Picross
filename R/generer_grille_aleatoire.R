
#' @importFrom stats rbinom
#'@param taille taille sélectionnner par le user
#'@param p proba
#'@return grille
#' @export
generer_grille_aleatoire <- function(taille, p) {
  matrix(rbinom(taille^2, 1, p), nrow = taille)
}
