#' get_check
#'
#' Identify ambiguous type variables from a data frame of mixed variable types
#' Comparison operators used are < and >
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lev number of levels desired
#' @return data frame containing only IID and variables meeting criteria, which should then be assessed manually
#' @export
#' @examples
#' get_check(df, lower=6, upper=18)

get_check <- function(df, lower=6, upper=18) {

  df_check <- data.frame()
  df_check <- df[, sapply(df, function(col) (length(unique(col[!is.na(col)])) > lower & length(unique(col[!is.na(col)])) < upper)), drop=FALSE]

  if(!"IID" %in% colnames(df_check) & "IID" %in% colnames(df)) {
    df_check <- cbind(df$IID, df_check)
    names(df_check)[[1]] <- "IID"
  }

  return(df_check)
}

