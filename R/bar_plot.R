#' bar_plot
#'
#' Create barcharts and choose number to display per page\cr
#' Note: Call quatrz() prior to plottting in RStudio\cr
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
#' @family categorical variable functions
#' @examples
#' \dontrun{
#' bar_plot(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=210)
#' }

bar_plot <- function(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=300) {
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
  print("Starting bar charts")

  for(j in 1:iter){

    print(paste("Creating image", j, "of", iter, sep=" "))
    plots <- list()
    lst_indx <- 1

    for(i in k:inc){

      v <- names(d[i])
      smpls <- length(d[[v]][!is.na(d[[v]])])
      sumstr <- paste("Sample Size = ", smpls, sep="")

      #Explicitly set aes to look in local environment due to bug in ggplot
      b <- ggplot2::ggplot(d, aes(x=factor(d[[v]])), environment=environment()) + ggplot2::geom_bar(fill="cadetblue4", colour="white", width=.5) + ggplot2::labs(x="Category", y="Count")
      b <- b + ggplot2::ggtitle(bquote(atop(.(v), atop(.(sumstr), ""))))
      print(b)
      plots[[lst_indx]] <- gridExtra::grid.arrange(b, ncol=1)
      lst_indx <- lst_indx + 1
    }

    grDevices::png(paste(file, "_barchart_", j, ".png", sep=""), width=wi, height=hgt, units="in", res=res, pointsize=4)
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

