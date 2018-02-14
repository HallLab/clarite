#' outlier_threshold
#'
#' Create data frame with outliers and summary statistics for each variable
#' @param df data frame with IID as first column
#' @param x number of standard deviations away from the mean to qualify as an outlier
#' @return data frame consisting of summary statistics for the input dataframe and potential dataframe if outliers were to be removed at that threshold
#' @export
#' @examples
#' outlier_threshold(df, x=2.5)

outlier_threshold <- function(df, x=2.5) {
  if(is.element('IID', names(df))==FALSE){
    stop("Please add IID to dataframe as column 1")
  }
  
  calcs <- function(v, threshold){
    pdf <- v
    s <- summary(v)
    std <- sd(v, na.rm=TRUE)
    ceil <- s[[4]] + threshold*std
    flor <- s[[4]] - threshold*std
    nr <- length(na.omit(v))
    #Execute on potential dataframe
    pdf[pdf > ceil | pdf < flor] <- NA
    ps <- summary(pdf)
    pstd <- sd(pdf, na.rm=TRUE)
    pnr <- length(na.omit(pdf))
    out <- rbind(N=nr, Min=s[[1]], Median=s[[3]], Mean=s[[4]], Max=s[[6]], SD=std, Outlier_SD=threshold, Pros_N=pnr, Pros_Min=ps[[1]], Pros_Median=ps[[3]], Pros_Mean=ps[[4]], Pros_Max=ps[[6]], Pros_SD=pstd)
    return(out)
  }
  
  sumdf <- as.data.frame(t(sapply(df[, -1], calcs, threshold=x)))
  sumdf <- cbind(rownames(sumdf), data.frame(sumdf, row.names=NULL))
  names(sumdf) <- c("Variable","N","Min","Median","Mean","Max","SD","Outlier_SD","Pros_N","Pros_Min","Pros_Median","Pros_Mean","Pros_Max","Pros_SD" )
  return(sumdf)
}
    


