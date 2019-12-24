#' Reverse Factor Levels
#'
#' This function reverses factor levels for using in plotting.
#' @param x A single factor vector.
#' @keywords reverse factor levels
#' @export
#' @examples
#' rev_factor_levels(iris$Species)

rev_factor_levels <- function(x) {
  factor(x, levels = rev(levels(x)))
}
