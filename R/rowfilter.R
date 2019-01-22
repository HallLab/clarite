#' rowfilter
#'
#' Subset data by sample name
#' @param d data frame, with first column as ID
#' @param samples list or dataframe of samples to subset
#' @param exclude boolean, keep or exclude
#' @return dataframe
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' ids <- c("51624", "51625")
#' head(rowfilter(NHANES, samples = ids))

rowfilter <- function(d, samples, exclude=FALSE){
  t1 <- Sys.time()
  print("Running...")

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

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(subd)
}
