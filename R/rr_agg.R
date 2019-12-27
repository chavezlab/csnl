#' Round-robin single subject aggregate correlation distance calculator
#'
#' Calculates the correlation distance between subject variables and target variables
#' after aggregated across raters. Self-to-self ratings are not included in the aggregated calculations.
#' This function works on a single subject basis, which is sometimes useful. It is also called
#' in rr_agg_all() for use over the whole data frame.
#' @param self The 'subject' column from the rr_selfonly data frame from rr_df_maker.
#' @param tar The 'target' column from the rr_noself_noNA data frame from rr_df_maker.
#' @param group The 'group' column from the rr_noself_noNA data frame from rr_df_maker.
#' @param cor_meth Correlation method from cor() used in calculation. Default = 'spearman'.
#' @author Robert S. Chavez
#' @export
#' @note This function relies on the output from rr_df_maker().
#' @examples
#' rr_agg(1,1,1) # self-congruent
#' rr_agg(1,2,1) # self-incongruent

rr_agg <- function(self, tar, groups, cor_meth ="spearman"){

  s <- rr_selfonly %>%
    filter(subject== self, target == self, group == groups) %>%
    select(-group, -subject, -target) %>%
    t

  temp <- rr_noself_noNA %>%
    filter(subject != self, target == tar, group == groups)  %>%
    select(-group, -subject, -target)

  agg_values <- apply(temp, 2, mean, na.rm=TRUE)


  cv <- 1-cor(s[,1],agg_values, method = cor_meth)

  return(cv)

}
