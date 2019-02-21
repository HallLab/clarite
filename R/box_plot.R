#' box_plot
#'
#' Create boxplots and choose number to display per page\cr
#' Note: Call quatrz() prior to plottting in RStudio\cr
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param n number of plots to display per page
#' @param nrow number of rows to display per page
#' @param ncol number of columns to display per page
#' @param wi width of plot
#' @param hgt height of plot
#' @param res resolution of plot
#' @param annotate optional data frame with clinical lab information
#' @param file filename of output
#' @return png image(s)
#' @export
#' @family plot functions
#' @family continuous variable functions
#' @examples
#' \dontrun{
#' box_plot(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=210, annotate)
#' }

box_plot <- function(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=300, annotate) {
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

  if(!missing(annotate)){
    #Add clinical lab information
    c <- stats::setNames(data.frame(t(annotate[,-1])), annotate[,1])
  } else {
    print("No annotations available")
  }

  print("Starting boxplots")

  for(j in 1:iter){

    print(paste("Creating image", j, "of", iter, sep=" "))
    plots <- list()
    lst_indx <- 1

    for(i in k:inc){

      v <- names(d[i])
      smpls <- length(d[[v]][!is.na(d[[v]])])
      std <- stats::sd(d[[i]], na.rm=TRUE)
      avg <- mean(d[[i]], na.rm=TRUE)
      sumstr <- paste("Sample Size = ", smpls, ", Std.Dev. = ", format(std, digits=4), sep="")

      #Explicitly set aes to look in local environment due to bug in ggplot
      b <- ggplot2::ggplot(data=d, aes(x=factor(0), y=d[[v]]), environment=environment()) + ggplot2::geom_boxplot(width=0.5, outlier.shape=1, outlier.colour="red", fill="cadetblue4")
      b <- b + ggplot2::labs(y="Value") + ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.title.x=ggplot2::element_blank(), axis.ticks.x=ggplot2::element_blank(), plot.title=ggplot2::element_text(size=9), axis.title=ggplot2::element_text(size=8))
      b <- b + ggplot2::stat_summary(fun.y=mean,geom="point", shape=18, colour="yellow") + ggplot2::guides(fill=FALSE) + ggplot2::ggtitle(bquote(atop(.(v), atop(.(sumstr), ""))))

      if(!missing(annotate)){
        if(v %in% names(c)){
          #[1,] = normal min, [2,] = normal max, [3,] = physiological min, [4,] = physiological max
          if(!is.na(c[v][1,])){
            b <- b + ggplot2::geom_hline(yintercept=c[v][1,], colour="grey12", linetype="dashed")
          }
          if(!is.na(c[v][2,])){
            b <- b + ggplot2::geom_hline(yintercept=c[v][2,], colour="grey12", linetype="dashed")
          }
          if(!is.na(c[v][3,])){
            b <- b + ggplot2::geom_hline(yintercept=c[v][3,], colour="red")
          }
          if(!is.na(c[v][4,])){
            b <- b + ggplot2::geom_hline(yintercept=c[v][4,], colour="red")
          }
        }
      }

      print(b)
      plots[[lst_indx]] <- gridExtra::grid.arrange(b, ncol=1)
      lst_indx <- lst_indx + 1
    }

    grDevices::png(paste(file, "_boxplot_", j, ".png", sep=""), width=wi, height=hgt, units="in", res=res, pointsize=4)
    do.call("gridExtra::grid.arrange", c(plots, ncol=ncol))
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

