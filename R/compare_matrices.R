#' Compare deux matrices
#'
#' Cette fonction compare deux matrices et retourne une matrice de comparaison.
#'
#' @param mat1 Première matrice à comparer
#' @param mat2 Deuxième matrice à comparer
#' @return Matrice de comparaison
#' @export

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
