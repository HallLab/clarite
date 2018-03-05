#' box_fun
#'
#' Create boxplots and choose number to display per page
#' Note: There is an issue with dev.off() if using RStudio
#' Dependencies: ggplot2, gridExtra
#' @param d data frame
#' @param i column index
#' @param annotate optional data frame with annotation information in the following format (order is important): Variable, Norm_min, Norm_max, Min, Max
#' @return png image(s)
#' @export
#' @examples
#' box_fun(d, i, annotate)
box_fun <- function(d, i, annotate) {
  if (!requireNamespace(c("ggplot2", "gridExtra"), quietly = TRUE)) {
    stop("Please install ggplot2 and gridExtra to create visualization.", call. = FALSE)
  }

  if(!missing(annotate)){
    #Add clinical lab information
    c <- setNames(data.frame(t(annotate[,-1])), annotate[,1])
  } else {
    print("No annotations available")
  }
      
  v <- names(d[i])
  smpls <- length(d[[v]][!is.na(d[[v]])])
  std <- sd(d[[i]], na.rm=TRUE)
  avg <- mean(d[[i]], na.rm=TRUE)
  sumstr <- paste("Sample Size = ", smpls, ", Std.Dev. = ", format(std, digits=4), sep="")

  #Explicitly set aes to look in local environment due to bug in ggplot  
  b <- ggplot(data=d, aes(x=factor(0), y=d[[v]]), environment=environment()) + geom_boxplot(width=0.5, outlier.shape=1, outlier.colour="red", fill="cadetblue4") 
  b <- b + labs(y="Value") + theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title=element_text(size=9), axis.title=element_text(size=8)) 
  b <- b + stat_summary(fun.y=mean,geom="point", shape=18, colour="yellow") + guides(fill=FALSE) + ggtitle(bquote(atop(.(v), atop(.(sumstr), "")))) 
          
  if(!missing(annotate)){    
    if(v %in% names(c)){
      #[1,] = normal min, [2,] = normal max, [3,] = physiological min, [4,] = physiological max
      if(!is.na(c[v][1,])){
        b <- b + geom_hline(yintercept=c[v][1,], colour="grey12", linetype="dashed")
      }
      if(!is.na(c[v][2,])){
        b <- b + geom_hline(yintercept=c[v][2,], colour="grey12", linetype="dashed")
      }
      if(!is.na(c[v][3,])){
        b <- b + geom_hline(yintercept=c[v][3,], colour="red")
      }
      if(!is.na(c[v][4,])){
        b <- b + geom_hline(yintercept=c[v][4,], colour="red")
      }
    }
  }

  print(b)
  return(b)
}
      

