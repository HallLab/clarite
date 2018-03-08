#' remove_outliers
#'
#' Remove outliers from data frame
#' This function does not remove the entire row of data, only replaces the outlier value with NA in a particular column
#' @param df data frame with first column as ID
#' @param x number of standard deviations away from the mean to qualify as an outlier
#' @return data frame with outlier values within each column replaced with NA
#' @export
#' @examples
#' remove_outliers(df, x=2.5)

remove_outliers <- function(df, x=2.5){

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }
  calc <- function(v, threshold){
    m <- mean(v, na.rm=TRUE)
    s <- sd(v, na.rm=TRUE)
    ceil <- m + threshold*s
    flor <- m - threshold*s
    v[v > ceil | v < flor] <- NA
    return(v)
  }

  sumdf <- cbind(df$ID, as.data.frame(sapply(df[,-1], calc, threshold=x)))
  colnames(sumdf)[1] <- "ID"

  return(sumdf)
}
