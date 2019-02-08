#' qq_fun
#'
#' Create qqplots and choose number to display per page\cr
#' Note: Call quatrz() prior to plottting in RStudio\cr
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param i column name
#' @return ggplot object
#' @family plot functions
#' @family continuous variable functions

qq_fun <- function(d, i) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 and gridExtra to create visualization.", call. = FALSE)
  } #else {
    #packages = c("ggplot2", "gridExtra")
    #lapply(packages, library, character.only = TRUE)
  #}

  v <- names(d[i])
  smpls <- length(d[[v]][!is.na(d[[v]])])
  std <- stats::sd(d[[i]], na.rm=TRUE)
  sumstr <- paste("Sample Size = ", smpls, ", Std.Dev. = ", format(std, digits=4), sep="")
  y <- stats::quantile(d[[v]][!is.na(d[[v]])], c(0.25, 0.75))
  x <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  #Explicitly set aes to look in local environment due to bug in ggplot
  q <- ggplot2::ggplot(d, aes(sample=d[[v]]), environment=environment()) + ggplot2::geom_point(stat="qq", na.rm=TRUE) + ggplot2::labs(x="Sample", y="Theoretical")
  q <- q + ggplot2::ggtitle(bquote(atop(.(v), atop(.(sumstr), "")))) + ggplot2::stat_qq(alpha=0.5) + ggplot2::geom_abline(slope=slope, intercept=int, colour="black") + ggplot2::theme(plot.title=ggplot2::element_text(size=9), axis.title=ggplot2::element_text(size=8))

  print(q)
  return(q)
}

