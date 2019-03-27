source("R/internal.R")

#' remove_outliers
#'
#' Remove outliers from data frame\cr
#' This function does not remove the entire row of data, only replaces the outlier value with NA in a particular column
#' @param df data frame with first column as ID
#' @param x number of standard deviations from the mean to qualify as an outlier
#' @param skip list of column names that are not processed for outliers
#' @param only list of column names that are processed for outliers, any others are skipped
#' @return data frame with outlier values within each column replaced with NA
#' @export
#' @family filter functions
#' @family continuous variable functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' summary(remove_outliers(get_continuous(NHANES), x=3))

remove_outliers <- function(df, x=2.5, skip=NULL, only=NULL){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  # Create a list of "ignored" columns which are never filtered
  ignored <- process_skip_only(df, skip, only)

  before_nas <- colSums(is.na(df))

  calc <- function(v, threshold){
    m <- mean(v, na.rm=TRUE)
    s <- stats::sd(v, na.rm=TRUE)
    ceil <- m + threshold*s
    flor <- m - threshold*s
    v[v > ceil | v < flor] <- NA
    return(v)
  }

  # Save original column order
  order <- colnames(df)

  # Combine the ignored columns (which always starts with ID) with the updated columns
  processed_df <- cbind(df[ignored], as.data.frame(sapply(df[!ignored], calc, threshold=x)))
  after_nas <- colSums(is.na(processed_df))
  replaced_outliers = sum(after_nas-before_nas)
  cols_with_replaced = sum(after_nas-before_nas > 0)
  
  print(paste("Removed", replaced_outliers, "outlier(s) from", cols_with_replaced, "column(s) (ignoring", sum(ignored), "columns)", sep=" "))

  # Set back to the original order
  processed_df <- processed_df[,order]

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(processed_df)
}
