#' Reverse Score Survey Item
#'
#' Calculates the reverse scoring of a standard survey item. You must specify the maximum value of the survey item if the values are all postive (e.g. 7 on a 1 to 7 scale). You may also specify if a scale's range include negative values that straddle zero (e.g. -5 to 5 scales).
#' @param x numeric vector
#' @param xmax maximum value of scale item
#' @param negvals logical indicating if the scale contain negative values
#' @author Robert S. Chavez
#' @export
#' @examples
#' x <- sample(1:7, 10, replace = TRUE)
#' reverse_score(x, 7)
#'
#' y <- sample(-5:5, 10, replace = TRUE)
#' reverse_score(y, negvals = TRUE)

reverse_score <- function(x, xmax, negvals = FALSE){

  if(negvals == FALSE){
    out <- (x*-1)+(xmax+1)
    return(out)
  }

  if(negvals == TRUE){
    xmax <- 0   # just a place holder value
    out <- (x*-1)
    return(out)
  }

}
