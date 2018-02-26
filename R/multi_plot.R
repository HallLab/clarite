#' multi_plot
#'
#' Create multiple types of plots on the same page
#' Note: There is an issue with dev.off() if using RStudio
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param n number of variables to display per page
#' @param nrow number of variables per row
#' @param ncol number of variables per column
#' @param wi width of plot
#' @param hgt height of plot
#' @param res resolution of plot
#' @param type sepcify plot type 'hist-qq, box-qq, hist-box, hist-box-qq'
#' @param annotate optional data frame with clinical lab information
#' @return png image(s)
#' @export
#' @examples
#' multi_plot(d, n=6, file="plot", nrow=3, ncol=2, wi=13.5, hgt=9, res=210, type="hist-qq", annotate)
library(ggplot2)
library(gridExtra)
multi_plot <- function(d, n=6, file="plot", nrow=3, ncol=2, wi=13.5, hgt=9, res=210, type, annotate) {

  if(n > ncol(d)){
    stop("Number of plots per page (", n, ") is larger than number of variables to plot (", ncol(d), ")")
  }

  if(missing(type)){
    stop("Please specify plot type")
  }	

  iter <- ceiling(ncol(d)/n)
  k <- 1
  inc <- n
 
  if(!missing(annotate)){
    #Add clinical lab information
    c <- setNames(data.frame(t(annotate[,-1])), annotate[,1])
  } else {
    print("No annotations available")
  }

  for(j in 1:iter){
    print(paste("Creating image", j, "of", iter, sep=" "))
    plots <- list()
    lst_indx <- 1
    for(i in k:inc){
      if(length(grep("hist", type))!=0){
        if(!missing(annotate)){
          h <- hist_fun(d=d, i=i, annotate=annotate)
        } else {
          h <- hist_fun(d=d, i=i)
	}
      }
      if(length(grep("box", type))!=0){
        if(!missing(annotate)){
          b <- box_fun(d=d, i=i, annotate=annotate)
	} else {
	  b <- box_fun(d=d, i=i)
	}
      }
      if(length(grep("qq", type))!=0){
	q <- qq_fun(d=d, i=i)
      }
      
      if(length(grep("hist-qq", type))!=0){
	plots[[lst_indx]] <- grid.arrange(h, q, ncol=2)
      }
      if(length(grep("hist-box", type))!=0){
	plots[[lst_indx]] <- grid.arrange(h, b, ncol=2)
      }
      if(length(grep("box-qq", type))!=0){
	plots[[lst_indx]] <- grid.arrange(b, q, ncol=2)
      }
      if(length(grep("hist-box-qq", type))!=0){
        plots[[lst_indx]] <- grid.arrange(h, b, q, ncol=3)
      }
      
      lst_indx <- lst_indx + 1
    }

    png(paste(file, "_", type, "_", j, ".png", sep=""), width=wi, height=hgt, units="in", res=res, pointsize=4)
    do.call("grid.arrange", c(plots, ncol=ncol))
    dev.off()
    print(paste("Printing image", j, "of", iter, sep=" "))
    
    k <- inc + 1
    inc <- if(j==iter-1) ncol(d) else inc + n
    #Adjust height for last plot
    if(inc==ncol(d)){
	n_row <- ceiling(((inc-k)+1)/ncol)
	hgt <- (hgt/nrow)*n_row
    }
  }
}