#' colfilter
#'
#' Subset data by column name
#' @param d data frame
#' @param cols list or dataframe of variable names to subset
#' @param exclude boolean, keep or exclude
#' @return dataframe
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' cols <- c("SurveyYr","Gender", "Age")
#' head(colfilter(NHANES, cols=cols))

colfilter <- function(d, cols, exclude=FALSE){
  t1 <- Sys.time()
  print("Running...")

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

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(subd)
}
