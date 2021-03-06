source("R/internal.R")

#' min_n
#'
#' Keep variables with a minimum of n samples
#' @param df data frame
#' @param n minimum sample size desired
#' @param skip list of column names that are not filtered
#' @param only list of column names that are filtered, any others are skipped
#' @return data frame containing only those variables with at least n samples
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' ncol(min_n(NHANES, n = 1000))

min_n <- function(df, n=200, skip=NULL, only=NULL){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  # Create a list of "ignored" columns which are never filtered
  ignored <- get_unfiltered_cols(df, skip, only)

  # Keep columns that are ignored or that have >= n values that aren't NA
  keep <- (colSums(!is.na(df)) >= n) | ignored  # Boolean vector
  filtered_df <- df[, keep, drop=FALSE]

  # Log how many were removed
  print(paste(sum(!keep), "of", length(keep), "columns removed due to <", n, "non-NA observations (ignoring", sum(ignored), "columns)", sep=" "))

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(filtered_df)
}

