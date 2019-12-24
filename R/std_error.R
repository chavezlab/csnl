#' Standard Error Calculator
#'
#' Calculates standard error of the mean for numeric vector.
#' @param x Numeric vector.
#' @param na.rm logical indicating removal of NAs before calculation.
#' @author Robert S. Chavez
#' @examples
#' x <- rnorm(100,100,15)
#' std_error(x)

std_error <- function (x, na.rm = FALSE)
  {
    if (na.rm)
      x <- x[!is.na(x)]
    sd(x)/sqrt(length(x))
  }
