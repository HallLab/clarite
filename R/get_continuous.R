#' get_continuous
#'
#' Identify continuous variables from a data frame of mixed variable types
#' Comparison operators used are >=
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lower minimum number of unique values to be considered continuous
#' @return data frame containing only ID and variables meeting the criteria
#' @export
#' @examples
#' get_continuous(df, lower=15)

get_continuous <- function(df, lower=15) {

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_cont <- data.frame()
  df_cont <- df[, sapply(df, function(col) length(unique(col[!is.na(col)])) >= lower), drop=FALSE]

  if(!"ID" %in% colnames(df_cont) & "ID" %in% colnames(df)){
    df_cont <- cbind(df$ID, df_cont)
    names(df_cont)[[1]] <- "ID"
  }

  return(df_cont)
}
