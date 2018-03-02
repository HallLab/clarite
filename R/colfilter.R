#' colfilter
#'
#' Subset data by column name
#' @param d data frame
#' @param cols list of variable names to subset
#' @param exclude boolean, keep or exclude
#' @return dataframe
#' @export
#' @examples
#' colfilter(d, cols, exclude=FALSE)


colfilter <- function(d, cols, exclude=FALSE){
  if(exclude==FALSE){
    subd <- d[, colnames(d) %in% cols, drop=FALSE]
  } else {
    subd <- d[, !colnames(d) %in% cols, drop=FALSE]
  }

  return(subd)
}
