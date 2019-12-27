#' Multiple ggplots in a single plot
#'
#' Print multiple graphs from ggplot2 on one page.
#' @param ... ggplot objects
#' @param plotlist Use a list of ggplot objects.
#' @param file Use ggplots from file.
#' @param cols Number of columns in multiplot.
#' @param layout Order layout.
#' @export
#' @author Winston Chang
#' @note See : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @examples
#' x <- rnorm(100,100,15)
#' y <- sample(c("Group 1","Group 2"),100,TRUE)
#' p1 <- qplot(x)
#' p2 <- qplot(y)
#' multiplot(p1,p2)

mulitplot <- function (..., plotlist = NULL, file, cols = 1, layout = NULL)
{
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  }
  else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout),
                                               ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
