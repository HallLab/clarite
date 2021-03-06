#' outlier_impact
#'
#' Create data frame with outliers and summary statistics for each variable
#' @param df data frame with ID as first column
#' @param x number of standard deviations away from the mean to qualify as an outlier
#' @return data frame consisting of summary statistics for the input dataframe and potential dataframe if outliers were to be removed at that threshold
#' @export
#' @family summary functions
#' @family continuous variable functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' outlier_impact(get_continuous(NHANES), x=3)

outlier_impact <- function(df, x=2.5) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  calcs <- function(v, threshold){
    pdf <- v
    s <- summary(v)
    std <- stats::sd(v, na.rm=TRUE)
    ceil <- s[[4]] + threshold*std
    flor <- s[[4]] - threshold*std
    nr <- length(stats::na.omit(v))
    #Execute on potential dataframe
    pdf[pdf > ceil | pdf < flor] <- NA
    ps <- summary(pdf)
    pstd <- stats::sd(pdf, na.rm=TRUE)
    pnr <- length(stats::na.omit(pdf))
    out <- rbind(N=nr, Min=s[[1]], Median=s[[3]], Mean=s[[4]], Max=s[[6]], SD=std, Outlier_SD=threshold, Pros_N=pnr, Pros_Min=ps[[1]], Pros_Median=ps[[3]], Pros_Mean=ps[[4]], Pros_Max=ps[[6]], Pros_SD=pstd)
    return(out)
  }

  sumdf <- as.data.frame(t(sapply(df[, -1], calcs, threshold=x)))
  sumdf <- cbind(rownames(sumdf), data.frame(sumdf, row.names=NULL))
  names(sumdf) <- c("Variable","N","Min","Median","Mean","Max","SD","Outlier_SD","Pros_N","Pros_Min","Pros_Median","Pros_Mean","Pros_Max","Pros_SD" )

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(sumdf)
}



