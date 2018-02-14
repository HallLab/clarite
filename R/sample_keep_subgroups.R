#' sample_keep_subgroups
#'
#' Keep variables with a minimum of n samples
#' @param df data frame
#' @param n minimum sample size desired
#' @return data frame containing only those variables with at least n samples per factor level
#' @export
#' @examples
#' sample_keep_subgroups(df, n)


sample_keep_subgroups <- function(df, n=1){

  keep <- df[, sapply(df, function(col) length(col[!is.na(col)])) >= n, drop=FALSE]

  return(keep)
}

