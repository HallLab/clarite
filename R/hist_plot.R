#' hist_plot
#'
#' Create histograms for each column of a data frame\cr
#' Note: Call quatrz() prior to plottting in RStudio\cr
#' Dependencies: ggplot2, gridExtra
#' @param d data frame with variables only
#' @param n number of plots to display per page
#' @param nrow number of rows to display per page
#' @param ncol number of columns to display per page
#' @param wi width of plot
#' @param hgt height of plot
#' @param res resolution of plot
#' @param annotate optional data frame containing annotation information
#' @param file filename of output
#' @return png image(s)
#' @export
#' @family plot functions
#' @family continuous data functions
#' @examples
#' \dontrun{
#' hist_plot(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=210, annotate)
#' }

hist_plot <- function(d, n=12, file="plot", nrow=4, ncol=3, wi=13.5, hgt=12, res=300, annotate) {
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

  print("Starting histograms")

  for(j in 1:iter){

    print(paste("Creating image", j, "of", iter, sep=" "))
    plots <- list()
    lst_indx <- 1

    for(i in k:inc){

      v <- names(d[i])
      smpls <- length(d[[v]][!is.na(d[[v]])])
      std <- stats::sd(d[[i]], na.rm=TRUE)
      sumstr <- paste("Sample Size = ", smpls, ", Std.Dev. = ", format(std, digits=4), sep="")

      #Explicitly set aes to look in local environment due to bug in ggplot
      h <- ggplot2::ggplot(data=d, aes(x=d[[v]]), environment=environment()) + ggplot2::labs(y="Count", x="Values") + ggplot2::geom_histogram(colour="white", fill=("cadetblue4"))
      h <- h + ggplot2::ggtitle(bquote(atop(.(v), atop(.(sumstr), "")))) + ggplot2::theme(plot.title=ggplot2::element_text(size=9), axis.title=ggplot2::element_text(size=8))

      if(!missing(annotate)){

        if(v %in% names(c)){
          #[1,] = normal min, [2,] = normal max, [3,] = physiological min, [4,] = physiological max
          if(!is.na(c[v][1,])){
            h <- h + ggplot2::geom_vline(xintercept=c[v][1,], colour="grey12", linetype="dashed")
          }
          if(!is.na(c[v][2,])){
            h <- h + ggplot2::geom_vline(xintercept=c[v][2,], colour="grey12", linetype="dashed")
          }
          if(!is.na(c[v][3,])){
            h <- h + ggplot2::geom_vline(xintercept=c[v][3,], colour="red")
          }
          if(!is.na(c[v][4,])){
            h <- h + ggplot2::geom_vline(xintercept=c[v][4,], colour="red")
          }
        }
      }

      print(h)
      plots[[lst_indx]] <- gridExtra::grid.arrange(h, ncol=1)
      lst_indx <- lst_indx + 1
    }

    grDevices::png(paste(file, "_histogram_", j, ".png", sep=""), width=wi, height=hgt, units="in", res=res, pointsize=4)
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

