#' Summarize data for between-subjects designs.
#'
#' Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95 percent)
#' @param data A data frame.
#' @param measurevar The name of a column that contains the variable to be summariezed.
#' @param betweenvars A vector containing names of columns that are between-subjects variables.
#' @param idvar The name of a column that identifies each subject (or matched subjects).
#' @param na.rm A boolean that indicates whether to ignore NA's.
#' @param conf.interval	The percent range of the confidence interval, 95 percent default.
#' @author Winston Chang
#' @note See : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @examples
#' x <- rnorm(100,100,15)
#' y <- sample(c("Group 1","Group 2"),100,TRUE)
#' df <- data.frame(x,y)
#' summarySE(data=df, measurevar="x", betweenvars="y")

summarySE <- function (data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                       conf.interval = 0.95, .drop = TRUE)
{
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm)
      sum(!is.na(x))
    else length(x)
  }
  datac <- plyr::ddply(data, groupvars, .drop = .drop, .fun = function(xx,
                                                                       col) {
    c(N = length2(xx[[col]], na.rm = na.rm), mean = mean(xx[[col]],
                                                         na.rm = na.rm), sd = sd(xx[[col]], na.rm = na.rm))
  }, measurevar)
  datac <- plyr::rename(datac, c(mean = measurevar))
  datac$se <- datac$sd/sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + 0.5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

