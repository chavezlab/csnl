#' Round-robin aggregated plot maker
#'
#' Creates plot for aggregated round-robin recapitulation analysis
#' @param consensus_df data frame in the form from rr_agg_all().
#' @author Robert S. Chavez
#' @note Requires output from rr_agg_all()
#' @examples
#' rr_agg_plot(consensus_df)

rr_agg_plot <- function(consensus_df){

  consensus <- consensus_df

  df <- consensus %>% group_by(group, subject, condition) %>% summarise(mean = mean(correlation), se=std_error(correlation))
  df$subject <- factor(df$subject, levels=levels(reorder(df$subject, df$mean)))
  df$condition <- ifelse(df$condition == "congruent","Self-Congruent Target","Self-Incongruent Target")


  # Plot
  ggplot(df,aes(as.factor(subject),mean)) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.3) +
    geom_point(color='black',size=3) +
    geom_point(aes(color=as.factor(condition)),size=2) +
    scale_color_brewer(palette = "Dark2", name=element_blank()) +
    theme_minimal() + #scale_y_continuous(breaks=seq(-.35,.50,.1)) +
    theme(legend.position = 'bottom') +
    labs(x = "Target ID", y= "correlation distance") +
    facet_wrap(~group)

}
