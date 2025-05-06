#' Off-diagonal Plotting Function for Dissimlarity Matricies
#'
#' This function will plot the off-diagonal elements of a dissmilarity matrix.
#' @param dsm a (dis)similarity matrix
#' @param col_n number of colors used in palette
#' @param color_dir direction of the color ramp. 1 == forward, -1 == reverse
#' @param rev_fac logical indicating whether to reverse the factor level order
#' @param melt logical indicating whether or not to melt the matrix into a data frame prior to plotting
#' @author Robert S. Chavez
#' @export
#' @examples
#' cars_dsm <- 1-cor(mtcars)
#' dsm_plot(cars_dsm)

dsm_plot <- function(dsm, col_n = 8, color_dir = 1, rev_fac=FALSE, melt=TRUE){

  library(ggplot2)
  library(colorRamps)
  library(viridis)
  if (melt == TRUE) {
    df <- dsm_mask(dsm)
  } else{

    df <- dsm
  }
  if (rev_fac == FALSE) {
    df <- df[which(is.na(df[, 3]) == FALSE), ]
    df[, 1] <- factor(df[, 1])
    df[, 2] <- factor(df[, 2])
    df$Var1 <- df[, 1]
    df$Var2 <- df[, 2]
    df$value <- df[, 3]
    mytheme <- theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank())
    ggplot(df, aes(Var1, Var2, fill = value)) + geom_raster() +
      theme_minimal() + mytheme + scale_fill_gradientn(colors = magma(col_n,
                                                                      direction = color_dir), name = "Dissimilarity") +
      xlab("") + ylab("")
  } else {
    df <- df[which(is.na(df[, 3]) == FALSE), ]
    df[, 1] <- factor(df[, 1])
    df[, 2] <- factor(df[, 2])
    df[, 2] <- factor(df[, 2], levels = rev(levels(df[, 2])))
    df$Var1 <- df[, 1]
    df$Var2 <- df[, 2]
    df$value <- df[, 3]
    mytheme <- theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank(), axis.text.x = element_text(angle = 300,
                                                                                    hjust = 0))
    ggplot(df, aes(Var1, Var2, fill = value)) + geom_raster() +
      theme_minimal() + mytheme + scale_fill_gradientn(colors = magma(col_n,
                                                                      direction = color_dir), name = "Dissimilarity") +
      xlab("") + ylab("")
  }
}
