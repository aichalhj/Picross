#' Obtient les indices de ligne contigus
#'
#' Cette fonction obtient les indices des lignes contigus dans une matrice.
#'
#' @param ligne Vecteur de la ligne à analyser
#' @return Indices des lignes contigus
#' @export

obtenir_indices_ligne <- function(ligne) {
  consecutive_ones <- rle(ligne)$lengths[rle(ligne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}
