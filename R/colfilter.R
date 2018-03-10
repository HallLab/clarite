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
  if(is.element('ID', names(d))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

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
  if(is.element('ID', names(subd))==FALSE){
    subd <- data.frame(d$ID, subd)
    colnames(subd)[1] <- "ID"
  }
  return(subd)
}
