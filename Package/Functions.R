#' Function to generate a random grid with specified probability
#' @param taille Taille de la grille
#' @param p Probabilité
#' @return Grille aléatoire
generer_grille_aleatoire <- function(taille, p) {
  matrix(rbinom(taille^2, 1, p), nrow = taille)
}

#' Function to obtain row indices
#' @param ligne Vecteur représentant une ligne de la grille
#' @return Indices des groupes de cases noires dans la ligne
obtenir_indices_ligne <- function(ligne) {
  consecutive_ones <- rle(ligne)$lengths[rle(ligne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

#' Function to compare two matrices element-wise
#' @param mat1 Première matrice
#' @param mat2 Deuxième matrice
#' @return Matrice de comparaison
compare_matrices <- function(mat1, mat2) {
  if (!identical(dim(mat1), dim(mat2))) {
    stop("Les dimensions des matrices ne correspondent pas.")
  }
  
  rows <- nrow(mat1)
  cols <- ncol(mat1)
  
  comparison <- matrix(NA, nrow = rows, ncol = cols)
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      comparison[i, j] <- ifelse(mat1[i, j] == mat2[i, j], TRUE, FALSE)
    }
  }
  
  return(comparison)
}