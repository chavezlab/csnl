#' Summarize data for within-subjects designs.
#'
#' Summarizes data, handling within-subjects variables by removing inter-subject variability. It will still work if there are no within-S variables. Gives count, un-normed mean, normed mean (with same between-group mean), standard deviation, standard error of the mean, and confidence interval. If there are within-subject variables, calculate adjusted values using method from Morey (2008).
#' @param data A data frame.
#' @param measurevar The name of a column that contains the variable to be summariezed.
#' @param betweenvars A vector containing names of columns that are between-subjects variables.
#' @param withinvars A vector containing names of columns that are within-subjects variables
#' @param idvar The name of a column that identifies each subject (or matched subjects).
#' @param na.rm A boolean that indicates whether to ignore NA's.
#' @param conf.interval	The percent range of the confidence interval, 95 percent default.
#' @author Winston Chang
#' @export
#' @note See : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @examples
#' x <- rnorm(100,100,15)
#' y <- sample(c("Group 1","Group 2"),100,TRUE)
#' z <- sample(c("Low","Med","High"),100,TRUE)
#' subject <- rep(1:5,20)
#' df <- data.frame(x,y,z,subject)
#' summarySEwithin(data=df, measurevar="x", betweenvars="y", withinvars="z", idvar="subject")

summarySEwithin <- function (data = NULL, measurevar, betweenvars = NULL, withinvars = NULL,
          idvar = NULL, na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
  {
    factorvars <- vapply(data[, c(betweenvars, withinvars), drop = FALSE],
                         FUN = is.factor, FUN.VALUE = logical(1))
    if (!all(factorvars)) {
      nonfactorvars <- names(factorvars)[!factorvars]
      message("Automatically converting the following non-factors to factors: ",
              paste(nonfactorvars, collapse = ", "))
      data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
    }
    datac <- summarySE(data, measurevar, groupvars = c(betweenvars,
                                                       withinvars), na.rm = na.rm, conf.interval = conf.interval,
                       .drop = .drop)
    datac$sd <- NULL
    datac$se <- NULL
    datac$ci <- NULL
    ndata <- normDataWithin(data, idvar, measurevar, betweenvars,
                            na.rm, .drop = .drop)
    measurevar_n <- paste(measurevar, "_norm", sep = "")
    ndatac <- summarySE(ndata, measurevar_n, groupvars = c(betweenvars,
                                                           withinvars), na.rm = na.rm, conf.interval = conf.interval,
                        .drop = .drop)
    nWithinGroups <- prod(vapply(ndatac[, withinvars, drop = FALSE],
                                 FUN = nlevels, FUN.VALUE = numeric(1)))
    correctionFactor <- sqrt(nWithinGroups/(nWithinGroups - 1))
    ndatac$sd <- ndatac$sd * correctionFactor
    ndatac$se <- ndatac$se * correctionFactor
    ndatac$ci <- ndatac$ci * correctionFactor
    merge(datac, ndatac)
  }
