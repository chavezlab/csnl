% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_items.R
\name{score_items}
\alias{score_items}
\title{Survey Item Scorer}
\usage{
score_items(df, selector, func = "sum", selector_position = "contains")
}
\arguments{
\item{df}{data frame input}

\item{selector}{text string to select column names}

\item{func}{function to apply to data (default is `sum`)}

\item{selector_position}{dictates which string selecting function to use for column names. Must be either "containts", "starts_with", "ends_with".}
}
\description{
Calculates summary scores of survey items into a vector. Targeted survey item names within a data frame must be named with a common string for filtering (e.g. "extraversion_1", "extraversion_2"). These common strings can be anywhere within the name.
}
\examples{
score_items(mtcars, "ar", func = "mean")
}
\author{
Robert S. Chavez
}
