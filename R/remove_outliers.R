#' remove_outliers
#'
#' Remove outliers from data frame
#' This function does not remove the entire row of data, only replaces the outlier value with NA in a particular column
#' @param df data frame with first column as ID
#' @param x number of standard deviations from the mean to qualify as an outlier
#' @return data frame with outlier values within each column replaced with NA
#' @export
#' @family filter functions
#' @family continuous variable functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' summary(remove_outliers(get_continuous(NHANES), x=3))

remove_outliers <- function(df, x=2.5){
  t1 <- Sys.time()
  print("Running...")

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

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(sumdf)
}
