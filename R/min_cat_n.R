source("R/internal.R")

#' min_cat_n
#'
#' Keep variables with a minimum of n samples
#' @param df data frame
#' @param n minimum sample size desired
#' @param skip list of column names that are not filtered
#' @param only list of column names that are filtered, any others are skipped
#' @return data frame containing only those variables with at least n samples per factor level
#' @export
#' @family filter functions
#' @family categorical variables functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' ncol(min_cat_n(get_categorical(NHANES)))

min_cat_n <- function(df, n=200, skip=NULL, only=NULL){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  # Create a list of "ignored" columns which are never filtered
  ignored <- process_skip_only(df, skip, only)

  # Keep columns that have >= n unique values
  keep <- sapply(df, function(v) (min(table(v)) >= n)) | ignored  # Boolean vector
  filtered_df <- df[, keep, drop=FALSE]

  # Log how many were removed
  print(paste(sum(!keep), "of", length(keep), "columns removed due to one or more categories having <", n, "samples (ignoring", sum(ignored), "columns)", sep=" "))

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(filtered_df)
}
