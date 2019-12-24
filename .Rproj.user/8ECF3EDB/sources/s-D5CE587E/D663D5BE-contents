#' Easy Bar Graph Maker for ggplot2
#'
#' Makes quick and easy bar graphs for ggplot2 without pre-summarizing.
#' @param dv Dependent variable vector
#' @param groups Groups or first factor variable.
#' @param factor Second factor variable.
#' @param id The name of a varible that identifieseach subject for withins-subject designs.
#' @param groups_between The 'groups' variable is a between subjects factor.
#' @param palette Select RColorBrewer palette.
#' @author Robert S. Chavez
#' @note You must manually adjust y-axis range using the ggplot2 function coord_cartesian().
#' @examples
#' # Between subject design.
#' x <- rnorm(100,100,15)
#' y <- sample(c("Group 1","Group 2"),100,TRUE)
#' z <- sample(c("Low","Med","High"),100,TRUE)
#' # Only one factor.
#' ggbarmaker(x,y)
#'
#' # Two factors.
#' ggbar.maker(x,y,z)
#'
#'
#'
#' # Within subject design
#' x <- rnorm(100,100,15)
#' y <- sample(c("Group 1","Group 2"),100,TRUE)
#' z <- sample(c("Low","Med","High"),100,TRUE)
#' subject <- rep(1:5,20)
#'
#' # No between subjects factors.
#' ggbarmaker(x,y,z, id =subject)
#'
#' # Grouping factor is between subjects.
#' ggbarmaker(x,y,z, id =subject, groups_between=TRUE)

ggbarmaker <- function (dv, groups, factor = NULL, id = NULL, groups_between = FALSE,
                        palette = "Set1")
{
  require(ggplot2)
  require(RColorBrewer)
  if (is.null(id)) {
    if (groups_between == TRUE) {
      warning("No within subject variable. Using between-subject SEM error bars")
    }
    print("Using between-subject SEM error bars")
    if (is.null(factor)) {
      mean <- as.numeric(tapply(dv, groups, mean, na.rm = TRUE))
      se <- as.numeric(tapply(dv, groups, std_error, na.rm = TRUE))
      lev <- as.factor(groups)
      fact <- levels(lev)
      eb_h <- mean + se
      eb_l <- mean - se
      df <- data.frame(mean, se, fact, eb_h, eb_l)
      labx <- deparse(substitute(groups))
      laby <- deparse(substitute(dv))
      ggplot(df, aes(fact, mean, fill = fact)) + geom_bar(position = position_dodge(0.9),
                                                          stat = "identity", colour = "black", size = 0.5) +
        theme(legend.position = "none") + scale_fill_brewer(palette = palette) +
        geom_errorbar(ymax = eb_h, ymin = eb_l, size = 0.5,
                      width = 0.3) + xlab(labx) + ylab(laby)
    }
    else {
      factor2 <- as.factor(factor)
      df <- aggregate(dv, by = list(groups, factor2), FUN = mean)
      df$se <- aggregate(dv, by = list(groups, factor2),
                         FUN = std_error)[[3]]
      df$eb_h <- as.numeric(df$x + df$se)
      df$eb_l <- as.numeric(df$x - df$se)
      labx <- deparse(substitute(groups))
      laby <- deparse(substitute(dv))
      ggplot(df, aes(Group.1, x, fill = Group.2)) + geom_bar(position = position_dodge(0.9),
                                                             stat = "identity", size = 0.5) + geom_bar(position = position_dodge(0.9),
                                                                                                       stat = "identity", colour = "black", size = 0.5,
                                                                                                       show.legend = FALSE) + scale_fill_brewer(palette = palette) +
        geom_errorbar(aes(ymin = eb_l, ymax = eb_h),
                      size = 0.5, width = 0.3, position = position_dodge(0.9)) +
        xlab(labx) + ylab(laby) + guides(fill = guide_legend(title = deparse(substitute(factor))))
    }
  }
  else {
    if (is.null(factor)) {
      if (groups_between == TRUE) {
        warning("No within subject variable. Using between-subject SEM error bars")
        mean <- as.numeric(tapply(dv, groups, mean, na.rm = TRUE))
        se <- as.numeric(tapply(dv, groups, std_error, na.rm = TRUE))
        lev <- as.factor(groups)
        fact <- levels(lev)
        eb_h <- mean + se
        eb_l <- mean - se
        df <- data.frame(mean, se, fact, eb_h, eb_l)
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        ggplot(df, aes(fact, mean, fill = fact)) + geom_bar(position = position_dodge(0.9),
                                                            stat = "identity", colour = "black", size = 0.5) +
          theme(legend.position = "none") + scale_fill_brewer(palette = palette) +
          geom_errorbar(ymax = eb_h, ymin = eb_l, size = 0.5,
                        width = 0.3) + xlab(labx) + ylab(laby)
      }
      else {
        print("Using within-subject 95% confidence interval error bars")
        require(plyr)
        xydf <- data.frame(dv, groups, id)
        dfwc <- summarySEwithin(data = xydf, measurevar = "dv",
                                withinvars = "groups", idvar = "id")
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        ggplot(dfwc, aes(groups, dv, fill = groups)) +
          geom_bar(position = position_dodge(), stat = "identity",
                   color = "black", size = 0.5) + theme(legend.position = "none") +
          scale_fill_brewer(palette = palette) + geom_errorbar(aes(ymin = dv -
                                                                     ci, ymax = dv + ci), width = 0.3, size = 0.3,
                                                               position = position_dodge(0.9)) + xlab(labx) +
          ylab(laby)
      }
    }
    else {
      print("Using within-subject 95% confidence interval error bars")
      if (groups_between == FALSE) {
        require(plyr)
        xydf <- data.frame(dv, groups, factor, id)
        dfwc <- summarySEwithin(data = xydf, measurevar = "dv",
                                withinvars = c("groups", "factor"), idvar = "id")
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        lab_fac <- deparse(substitute(factor))
        ggplot(dfwc, aes(groups, dv, fill = factor)) +
          geom_bar(position = position_dodge(), stat = "identity",
                   size = 0.5) + geom_bar(position = position_dodge(),
                                          stat = "identity", color = "black", size = 0.5,
                                          show_guide = FALSE) + scale_fill_brewer(palette = palette,
                                                                                  name = lab_fac) + geom_errorbar(aes(ymin = dv -
                                                                                                                        ci, ymax = dv + ci), width = 0.3, size = 0.3,
                                                                                                                  position = position_dodge(0.9)) + xlab(labx) +
          ylab(laby)
      }
      else {
        require(plyr)
        xydf <- data.frame(dv, groups, factor, id)
        dfwc <- summarySEwithin(data = xydf, measurevar = "dv",
                                withinvars = "factor", betweenvars = "groups",
                                idvar = "id")
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        lab_fac <- deparse(substitute(factor))
        ggplot(dfwc, aes(groups, dv, fill = factor)) +
          geom_bar(position = position_dodge(), stat = "identity",  size = 0.5) +
          geom_bar(position = position_dodge(), stat = "identity", color = "black", size = 0.5, show.legend = FALSE) +
          scale_fill_brewer(palette = palette, name = lab_fac) +
          geom_errorbar(aes(ymin = dv - ci, ymax = dv + ci), width = 0.3, size = 0.3, position = position_dodge(0.9)) + xlab(labx) +
          ylab(laby)
      }
    }
  }
}
