#' Cette fonction génère une grille de Picross aléatoire avec le nombre spécifié de lignes et de colonnes.
#'
#' @param n_lignes Un nombre entier, le nombre de lignes dans la grille.
#' @param n_colonnes Un nombre entier, le nombre de colonnes dans la grille.
#'
#' @return Une matrice représentant la grille de Picross.
#' @export
#'
#' @examples
#' creer_grille(10, 10)
creer_grille <- function(n_lignes, n_colonnes) {
  grille <- matrix(sample(c(0, 1), n_lignes * n_colonnes, replace = TRUE), nrow = n_lignes)
  return(grille)
}

