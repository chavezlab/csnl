#' Quick correlation with plot
#'
#' Calculates standard error of the mean for numeric vector.
#' @param x First numeric vector.
#' @param y Second numeric vector.
#' @author Robert S. Chavez
#' @export
#' @examples
#' a <- rnorm(100,100,15)
#' b <- rnorm(100,90,15)
#' qcor(a,b)

qcor <- function (x, y)
{
  library(ggplot2)
  labx <- deparse(substitute(x))
  laby <- deparse(substitute(y))
  NEWDAT <- data.frame(x = x, y = y)
  NEWDAT <- na.exclude(NEWDAT)
  model <- lm(y ~ x, data = NEWDAT)
  pl <- qplot(x, y, data = NEWDAT) + stat_smooth(method = "lm") +
    xlab(labx) + ylab(laby) + theme_minimal()
  modelout <- summary(model)
  corout <- cor(x, y, use = "complete.obs")
  out <- list(modelout, corout, pl)
  print(out)
}
