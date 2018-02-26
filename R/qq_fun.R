#' qq_fun
#'
#' Create qqplots and choose number to display per page
#' Note: There is an issue with dev.off() if using RStudio
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param i column name
#' @param annotate optional data frame of annotation information
#' @return ggplot object
#' @export
#' @examples
#' qq_fun(d, i, annotate)
library(ggplot2)
library(gridExtra)
qq_fun <- function(d, i) {

  v <- names(d[i])
  smpls <- length(d[[v]][!is.na(d[[v]])])
  std <- sd(d[[i]], na.rm=TRUE)
  sumstr <- paste("Sample Size = ", smpls, ", Std.Dev. = ", format(std, digits=4), sep="")
  y <- quantile(d[[v]][!is.na(d[[v]])], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
        
  #Explicitly set aes to look in local environment due to bug in ggplot  
  q <- ggplot(d, aes(sample=d[[v]]), environment=environment()) + geom_point(stat="qq", na.rm=TRUE) + labs(x="Sample", y="Theoretical") 
  q <- q + ggtitle(bquote(atop(.(v), atop(.(sumstr), "")))) + stat_qq(alpha=0.5) + geom_abline(slope=slope, intercept=int, colour="black") + theme(plot.title=element_text(size=9), axis.title=element_text(size=8))

  print(q)
  return(q)
}
