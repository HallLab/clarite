#' samplefilter
#'
#' Subset data by sample name
#' @param d data frame, with first column as IID
#' @param samples list of samples to subset
#' @param exclude boolean, keep or exclude
#' @return dataframe
#' @export
#' @examples
#' samplefilter(d, samples, exclude=FALSE)


samplefilter <- function(d, samples, exclude=FALSE){
  if(is.element('IID', names(d))==FALSE){
    stop("Please add IID to d as column 1")
  }
  if(exclude==FALSE){
    subd <- d[d$IID %in% samples, , drop=FALSE]
  } else {
    subd <- d[!d$IID %in% samples, , drop=FALSE]
  }

  return(subd)
}
