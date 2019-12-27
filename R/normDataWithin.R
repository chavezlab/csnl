#' Normalizing helper function for within-subjects data..
#'
#' Norms the data within specified groups in a data frame; it normalizes each.
#' @param data A data frame.
#' @param idvar The name of a column that identifies each subject (or matched subjects).
#' @param measurevar The name of a column that contains the variable to be summariezed.
#' @param betweenvars A vector containing names of columns that are between-subjects variables.
#' @param na.rm A boolean that indicates whether to ignore NA's.
#' @author Winston Chang
#' @export
#' @note See : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @examples
#' x <- rnorm(100,100,15)
#' y <- sample(c("Group 1","Group 2"),100,TRUE)
#' z <- sample(c("Low","Med","High"),100,TRUE)
#' subject <- rep(1:5,20)
#' df <- data.frame(x,y,z,subject)
#'
#' summarySEwithin(data=df, measurevar="x", betweenvars="y", withinvars="z", idvar="subject")


normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {

  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                               .fun = function(xx, col, na.rm) {
                                 c(subjMean = mean(xx[,col], na.rm=na.rm))
                               },
                               measurevar,
                               na.rm
  )

  # Put the subject means with original data
  data <- merge(data, data.subjMean)

  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)

  # Remove this subject mean column
  data$subjMean <- NULL

  return(data)
}
