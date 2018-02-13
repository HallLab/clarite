#' get_categorical
#'
#' Identify categorical variables from a data frame of mixed variable types
#' Comparison operators used are <= and >=
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lower minimum number of unique values to be considered categorical
#' @param upper maximum number of unique values to be considered categorical
#' @return data frame containing only IID and variables meeting the criteria
#' @export
#' @examples
#' get_categorical(df, lower=3, upper=6)

get_categorical <- function(df, lower=3, upper=6) {

  df_cat <- data.frame()
  df_cat <- df[, sapply(df, function(col) (length(unique(col[!is.na(col)])) >= lower & length(unique(col[!is.na(col)])) <= upper)), drop=FALSE]

  if(!"IID" %in% colnames(df_cat) & "IID" %in% colnames(df)) {
    df_cat <- cbind(df$IID, df_cat)
    names(df_cat)[[1]] <- "IID"
  }

  return(df_cat)

}
