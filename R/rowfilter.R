#' rowfilter
#'
#' Subset data by sample name
#' @param d data frame, with first column as ID
#' @param samples list or dataframe of samples to subset
#' @param exclude boolean, keep or exclude
#' @return dataframe
#' @export
#' @examples
#' rowfilter(d, samples, exclude=FALSE)


rowfilter <- function(d, samples, exclude=FALSE){
  if(is.element('ID', names(d))==FALSE){
    stop("Please add ID to d as column 1")
  }

  if(exclude==FALSE){
    if(is.data.frame(samples)==TRUE){
      subd <- d[d$ID %in% samples[,1], , drop=FALSE]
    } else {
      subd <- d[d$ID %in% samples, , drop=FALSE]
    }
  } else {
    if(is.data.frame(samples)==TRUE){
      subd <- d[!d$ID %in% samples[,1], , drop=FALSE]
    } else {
      subd <- d[!d$ID %in% samples, , drop=FALSE]
    }
  }

  return(subd)
}
