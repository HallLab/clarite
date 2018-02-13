#' get_continuous
#'
#' Identify continuous variables from a data frame of mixed variable types
#' Comparison operators used are >=
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lower minimum number of unique values to be considered continuous
#' @return data frame containing only IID and variables meeting the criteria
#' @export
#' @examples
#' get_continuous(df, lower=18)

get_continuous <- function(df, lower=18) {

  df_cont <- data.frame()
  df_cont <- df[, sapply(df, function(col) length(unique(col[!is.na(col)])) >= lower), drop=FALSE]

  if(!"IID" %in% colnames(df_cont) & "IID" %in% colnames(df)){
    df_cont <- cbind(df$IID, df_cont)
    names(df_cont)[[1]] <- "IID"
  }

  return(df_cont)
}
