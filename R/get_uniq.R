#' get_uniq
#'
#' Get number of unique values in each column, not including 'NA'
#' @param df data frame
#' @return data frame with two columns, one for variable name and one for number of unique values
#' @export
#' @examples
#' get_uniq(df)

get_uniq <- function(df) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_levels <- as.data.frame(sapply(df, function(x) (length(unique(x)))))

  df_levels <- cbind(rownames(df_levels), data.frame(df_levels, row.names=NULL))
  names(df_levels) <- c("Variable", "Unique_Values")

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df_levels)
}
