#' Round-robin whole data frame aggregate correlation distance analysis
#'
#' Calculates the correlation distance between subject variables and target variables
#' after aggregated across raters for an entire subject list in a data frame. Outputs
#' a data frame with aggregated correlation information and congruent/incongruent information.
#' Self-to-self ratings are not included in the aggregated calculations. This function relies
#' on the output from rr_df_maker().
#' @param subs_df The 'subs' data frame from rr_df_maker() output.
#' @param cor_meth Correlation method from cor() used in calculation. Default = 'spearman'.
#' @author Robert S. Chavez
#' @export
#' @note This function relies on the output from rr_df_maker().
#' @examples
#' rr_agg_all(subs, cor_meth = spearman)


rr_agg_all <- function(subs_df, cor_meth = "spearman"){

  subs <- subs_df

  # run function over
  cor_vec <- vector()

  for(i in 1:length(subs[,1])){
    agg <- rr_agg(self = subs[i,2], tar = subs[i,3], groups = subs[i,1])
    cor_vec <- c(cor_vec,agg)
  }


  consensus <- rr_df %>% select(group, subject, target)
  consensus$correlation <- cor_vec

  consensus$condition <- ifelse(rr_df$subject == rr_df$target, "congruent", "incongruent")
  consensus$condition <- factor(consensus$condition, levels = c("congruent", "incongruent"))
  consensus$congruent_corr <- ifelse(consensus$condition == "congruent",consensus$correlation,NA)
  consensus$incongruent_corr <- ifelse(consensus$condition == "congruent",NA,consensus$correlation)
  consensus$subject <- factor(consensus$subject, levels=levels(reorder(consensus$subject, -consensus$correlation)))

  consensus_df <<- consensus


}
