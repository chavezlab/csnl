#' Mask disimilarity matrix with NAs for the off-diagonal elements
#'
#' This function will subsitute NAs in the off diagonal elements of a dissimilarity matrix. Will output melted matrix for convienent ggploting, or can simply mask an existing matrix.
#' @param dsm a single matrix
#' @param lower logical indicating selection of lower or upper diagonal
#' @param melt logical indicating whether or not to melt the matrix into a data frame
#' @author Robert S. Chavez
#' @examples
#' cars_dsm <- 1-cor(mtcars)
#' dsm_mask(cars_dsm)


dsm_mask <- function(dsm, lower=FALSE, melt=TRUE){
  require(reshape2)

  # Don't melt output
  if(melt==FALSE){
    if(lower==FALSE){
      mat <- as.matrix(dsm)
      mat[lower.tri(mat,diag = TRUE)] <- NA
      mat
    }
    else{
      mat <- as.matrix(dsm)
      mat[upper.tri(mat,diag = TRUE)] <- NA
      mat
    }
  }

  # Melt output for plotting
  else{
    if(lower==FALSE){
      mat <- as.matrix(dsm)
      mat[lower.tri(mat,diag = TRUE)] <- NA
      mat_m <- melt(mat)
      mat_m$Var1 <- as.factor(mat_m$Var1)
      mat_m$Var2 <- as.factor(mat_m$Var2)
      mat_m$Var2 <- factor(mat_m$Var2, levels = rev(levels(mat_m$Var2)))
      mat_m
    }
    else{
      mat <- as.matrix(dsm)
      mat[upper.tri(mat,diag = TRUE)] <- NA
      mat_m <- melt(mat)
      mat_m$Var1 <- as.factor(mat_m$Var1)
      mat_m$Var2 <- as.factor(mat_m$Var2)
      mat_m$Var2 <- factor(mat_m$Var2, levels = rev(levels(mat_m$Var2)))
      mat_m
    }
  }
}
