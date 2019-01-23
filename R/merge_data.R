#' merge_data
#'
#' Merge two datasets
#' @param df1 first dataframe
#' @param df2 second dataframe
#' @param union boolean argument, if TRUE will add NA values, if FALSE will take intersect, default=TRUE
#' @return data frame containing merged data
#' @export
#' @examples
#' \dontrun{
#' merge_data(df1, df2, union=TRUE)
#' }

merge_data <- function(df1, df2, union=TRUE){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df1))==FALSE | is.element('ID', names(df2))==FALSE){
    stop("Please add ID to both dataframes as column 1")
  }

  if(union==TRUE){
    mergeddf <- merge(df1, df2, by="ID", all=TRUE)
  } else {
    mergeddf <- merge(df1, df2, by="ID")
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(mergeddf)
}
