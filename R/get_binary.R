#' get_binary
#'
#' Identify binary variables from a data frame of mixed variable types
#' Comparison operator used is =
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lev number of levels desired
#' @return data frame containing only IID and variables with the number of levels specified
#' @export
#' @examples
#' get_binary(df, lev=2)

get_binary <- function(df, lev=2){

  df_bin <- data.frame()
  df_bin <- df[, sapply(df, function(col) length(unique(col[!is.na(col)])) == lev), drop=FALSE]

  if(!"IID" %in% colnames(df_bin) & "IID" %in% colnames(df)) {
    df_bin <- cbind(df$IID, df_bin)
    names(df_bin)[[1]] <- "IID"
  }

  return(df_bin)

}
