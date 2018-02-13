#' get_levels
#'
#' Get number of unique values in each column, not including 'NA'
#' @param df data frame
#' @return data frame with two columns, one for variable name and one for number of unique values
#' @export
#' @examples
#' get_levels(df)

get_levels <- function(df) {

  df_levels <- as.data.frame(sapply(df, function(x) (length(unique(x)))))

  df_levels <- cbind(rownames(df_levels), data.frame(df_levels, row.names=NULL))
  names(df_levels) <- c("Variable", "Levels")

  return(df_levels)
}
