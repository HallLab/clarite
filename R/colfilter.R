#' colfilter
#'
#' Subset data by column name
#' @param d data frame
#' @param cols list or dataframe of variable names to subset
#' @param exclude boolean, keep or exclude
#' @return dataframe
#' @export
#' @examples
#' colfilter(d, cols, exclude=FALSE)


colfilter <- function(d, cols, exclude=FALSE){
  if(exclude==FALSE){
    if(is.data.frame(cols)==TRUE){
      subd <- d[, colnames(d) %in% cols[,1], drop=FALSE]
    } else {
      subd <- d[, colnames(d) %in% cols, drop=FALSE]
    }
  } else {
    if(is.data.frame(cols)==TRUE){
      subd <- d[, !colnames(d) %in% cols[,1], drop=FALSE]
    } else {
      subd <- d[, !colnames(d) %in% cols, drop=FALSE]
    }
  }

  return(subd)
}
