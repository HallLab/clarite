#' sample_keep
#'
#' Keep variables with a minimum of n samples
#' @param df data frame
#' @param n minimum sample size desired
#' @return data frame containing only those variables with at least n samples
#' @export
#' @examples
#' sample_keep(df, n)


sample_keep <- function(df, n=200){

  keep <- df[, sapply(df, function(col) length(col[!is.na(col)])) >= n, drop=FALSE]

  return(keep)
}

