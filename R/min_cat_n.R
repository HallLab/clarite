#' min_cat_n
#'
#' Keep variables with a minimum of n samples
#' @param df data frame
#' @param n minimum sample size desired
#' @return data frame containing only those variables with at least n samples per factor level
#' @export
#' @examples
#' min_cat_n(df, n)


min_cat_n <- function(df, n=200){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  keep <- df[, sapply(df, function(v) (min(table(v)) > n)), drop=FALSE]

  if(!"ID" %in% colnames(keep)) {
    keep <- cbind(df$ID, keep)
    names(keep)[[1]] <- "ID"
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(keep)
}
