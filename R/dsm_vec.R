#' Vectorize Dissimilarity Matrix
#'
#' Convert dissimilarity matrix into off-diagonal vector
#' @param dsm a single matrix
#' @param lower logical indicating selection of lower or upper diagonal
#' @author Robert S. Chavez
#' @export
#' @examples
#' cars_dsm <- 1-cor(mtcars)
#' dsm_vec(cars_dsm)


# Convert dissimilarity matrix into off-diagonal vector -------------------
dsm_vec <- function(dsm, lower=TRUE){

  if(lower==TRUE){
    mat <- as.matrix(dsm)
    as.vector(mat[lower.tri(mat)])
  }
  else {
    mat <- as.matrix(dsm)
    as.vector(mat[upper.tri(mat)])
  }
}
