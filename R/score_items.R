#' Survey Item Scorer
#'
#' Calculates summary scores of survey items into a vector. Targeted survey item names within a data frame must be named with a common string for filtering (e.g. "extraversion_1", "extraversion_2"). These common strings can be anywhere within the name.
#' @param df data frame input
#' @param selector text string to select column names
#' @param func function to apply to data (default is `sum`)
#' @param selector_position dictates which string selecting function to use for column names. Must be either "containts", "starts_with", "ends_with".
#' @author Robert S. Chavez
#' @export
#' @examples
#' score_items(mtcars, "ar", func = "mean")

score_items <- function(df, selector, func = "sum", selector_position = "contains"){
  library(dplyr)

  if(selector_position == "contains"){
    df2 <- df %>% select(contains(selector))
    x <- df2 %>% apply(., 1, func)
    return(x)
  }

    if(selector_position == "starts"){
  df2 <- df %>% select(starts_with(selector))
  x <- df2 %>% apply(., 1, func)
  return(x)
  }

  if(selector_position == "ends"){
    df2 <- df %>% select(ends_with(selector))
    x <- df2 %>% apply(., 1, func)
    return(x)
  }

}
