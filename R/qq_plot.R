#' qq_plot
#'
#' Create qqplots and choose number to display per page\cr
#' Note: Call quartz() prior to plotting if using RStudio\cr
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param n number of plots to display per page
#' @param nrow number of rows to display per page
#' @param ncol number of columns to display per page
#' @param wi width of plot
#' @param hgt height of plot
#' @param res resolution of plot
#' @param file filename of output
#' @return png image(s)
#' @export
#' @family plot functions
#' @family continuous variable functions
#' @examples
#' \dontrun{
#' qq_plot(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=210)
#' }

qq_plot <- function(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=300) {
  t1 <- Sys.time()

  if (!requireNamespace("ggplot2", quietly = TRUE)==TRUE|!requireNamespace("gridExtra", quietly = TRUE)==TRUE) {
    stop("Please install ggplot2 and gridExtra to create visualization.", call. = FALSE)
  } #else {
    #packages = c("ggplot2", "gridExtra")
    #lapply(packages, library, character.only = TRUE)
  #}

  if(is.element('ID', names(d))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  d <- d[, -1]

  if(n > ncol(d)){
    stop("Number of plots per page (", n, ") is larger than number of variables to plot (", ncol(d), ")")
  }

  iter <- ceiling(ncol(d)/n)
  k <- 1
  inc <- n

  print("Starting qqplots")

  for(j in 1:iter){

    print(paste("Creating image", j, "of", iter, sep=" "))
    plots <- list()
    lst_indx <- 1

    for(i in k:inc){

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
      plots[[lst_indx]] <- gridExtra::grid.arrange(q, ncol=1)
      lst_indx <- lst_indx + 1
    }

    grDevices::png(paste(file, "_qqplot_", j, ".png", sep=""), width=wi, height=hgt, units="in", res=res, pointsize=4)
    do.call(eval(parse(text="gridExtra::grid.arrange")), c(plots, ncol=ncol))
    grDevices::dev.off()
    print(paste("Printing image", j, "of", iter, sep=" "))

    k <- inc + 1
    inc <- if(j==iter-1) ncol(d) else inc + n
    #Adjust height for last plot
    if(inc==ncol(d)) {
      n_row <- ceiling(((inc-k)+1)/ncol)
      hgt <- (hgt/nrow)*n_row
    }
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))
}

