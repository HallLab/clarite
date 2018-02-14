#' sample_size
#'
#' Create list of sample_sizes for each variable
#' @param df data frame with IID as first column
#' @return data frame with two columns, "Variable" and "Sample_Size"
#' @export
#' @examples
#' sample_size(df)

sample_size <- function(df) {

  df_n <- as.data.frame(sapply(df, function(x) (length(na.omit(x)))))
  df_n <- cbind(rownames(df_n), data.frame(df_n, row.names=NULL))
  names(df_n) <- c("Variable", "N")

  return(df_n)
}
