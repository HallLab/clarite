#' hist_fun
#'
#' Create histograms and choose number to display per page\cr
#' Note: Call quatrz() prior to plottting in RStudio\cr
#' Dependencies: ggplot2
#' @param d data frame
#' @param i column name
#' @param annotate optional data frame of annotation information in the following format (order is important): Variable, Norm_min, Norm_max, Min, Max
#' @return ggplot object
#' @family plot functions
#' @family continuous variable functions

hist_fun <- function(d, i, annotate) {
  if (!requireNamespace("ggplot2", quietly = TRUE)==TRUE) {
    stop("Please install ggplot2 to create visualization.", call. = FALSE)
  } #else {
    #packages = c("ggplot2", "gridExtra")
    #lapply(packages, library, character.only = TRUE)
  #}

  if(!missing(annotate)){
    #Add clinical lab information
    c <- stats::setNames(data.frame(t(annotate[,-1])), annotate[,1])
  } else {
    print("No annotations available")
  }

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
  return(h)
}

