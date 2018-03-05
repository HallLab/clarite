#' bar_plot
#'
#' Create barcharts and choose number to display per page
#' Note: There is an issue with dev.off() if using RStudio
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param n number of plots to display per page
#' @param nrow number of rows to display per page
#' @param ncol number of columns to display per page
#' @param wi width of plot
#' @param hgt height of plot
#' @param res resolution of plot
#' @return png image(s)
#' @export
#' @examples
#' bar_plot(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=210)

bar_plot <- function(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=210) {
  if (!requireNamespace(c("ggplot2", "gridExtra"), quietly = TRUE)) {
    stop("Please install ggplot2 and gridExtra to create visualization.", call. = FALSE)
  }

  #n_row <- floor(sqrt(n))
  #n_col <- ceiling(n/n_row)
  #wi <- 4.5*n_col
  #hgt <- 3*n_row
    
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
      b <- ggplot(d, aes(x=factor(d[[v]])), environment=environment()) + geom_bar(fill="cadetblue4", colour="white", width=.5) + labs(x="Category", y="Count")
      b <- b + ggtitle(bquote(atop(.(v), atop(.(sumstr), ""))))
      print(b)
      plots[[lst_indx]] <- grid.arrange(b, ncol=1)
      lst_indx <- lst_indx + 1
    }
      
    png(paste(file, "_barchart_", j, ".png", sep=""), width=wi, height=hgt, units="in", res=210, pointsize=4)
    do.call("grid.arrange", c(plots, ncol=ncol))
    dev.off()
    print(paste("Printing image", j, "of", iter, sep=" "))
      
    k <- inc + 1
    inc <- if(j==iter-1) ncol(d) else inc + n
    #Adjust height for last plot
    if(inc==ncol(d)) {
      n_row <- ceiling(((inc-k)+1)/ncol)
      hgt <- (hgt/nrow)*n_row
    }
  }
    
    print("Finished creating barcharts")
}

