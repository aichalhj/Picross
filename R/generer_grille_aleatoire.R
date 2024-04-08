
#' @importFrom stats rbinom
#'
#' @export
generer_grille_aleatoire <- function(taille, p) {
  matrix(rbinom(taille^2, 1, p), nrow = taille)
}
