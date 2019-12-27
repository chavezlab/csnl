#' Round-robin data frame maker
#'
#' Creates a set of data frames for use in a CSNL round-robin analyses of fMRI data.
#' Output from this function will be necessary for out rr_* functions in this package.
#' @param fsl_zstats A path to a text file from fslmeants output.
#' @param subs_csv A .csv file with group, subject, and target columns that will be used in the analysis.
#' @author Robert S. Chavez
#' @examples
#' rr_df_maker("~/data/fsl_output.txt", "~/data/subect_list.csv")

rr_df_maker <- function(fsl_zstats, subs_csv){

  raw_zstats <<- read.table(fsl_zstats, quote="\"", comment.char="",skip = 3)

  subs <<- read.csv(subs_csv)

  rr_df <<- cbind(subs, raw_zstats)

  rr_noself <<- rr_df
  for(i in 1:length(rr_noself$subject)){
    if(rr_noself[i,2] == rr_noself[i,3]){
      rr_noself[i,] <-c(rr_noself[i,1], rr_noself[i,2], rr_noself[i,3],rep(NA, length(rr_noself[1,])-3))
    }
  }

  rr_noself_noNA <<- rr_df %>% filter(subject != target)
  rr_selfonly <<- rr_df %>% filter(subject == target)

}

