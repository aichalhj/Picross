#' Obtient les indices de colonne contigus
#'
#' Cette fonction obtient les indices des colonnes contigus dans une matrice.
#'
#' @param colonne Vecteur de la colonne à analyser
#' @return Indices des colonnes contigus
#' @export

obtenir_indices_colonne <- function(colonne) {
  consecutive_ones <- rle(colonne)$lengths[rle(colonne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}
