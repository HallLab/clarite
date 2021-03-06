#' eman
#'
#' Create Manhattan plots for EWAS\cr
#' Note: There is an issue with dev.off() if using RStudio\cr
#' Dependencies: ggplot2\cr
#' Suggested: RColorBrewer
#' @param d data frame, if ewas=FALSE columns one and two must be Variable and pvalue, Shape is optional 3rd column
#' @param ewas boolean if d frame is in ewas output format, default TRUE
#' @param groups optional file containing variable grouping information
#' @param line optional pvalue threshold to draw red line at
#' @param title optional string for plot title
#' @param morecolors boolean, expand color palette, requires RColorBrewer package, default FALSE
#' @param file file name of saved image
#' @param hgt height of plot in inches
#' @param wi width of plot in inches
#' @param res resolution of plot in pixels per inch
#' @return png image(s)
#' @export
#' @family plot functions
#' @examples
#' \dontrun{
#' eman(d, ewas=TRUE, groups=NULL, line, title=NULL, morecolors=FALSE,
#'	file="eman", hgt=7, wi=12, res=300 )
#' }

eman <- function(d, ewas=TRUE, groups=NULL, line=NULL, title=NULL, morecolors=FALSE, file="eman", hgt=7, wi=12, res=300){
  t1 <- Sys.time()
  print("Running...")

  if (!requireNamespace(c("ggplot2"), quietly = TRUE)==TRUE) {
    stop("Please install ggplot2 to create visualization.", call. = FALSE)
  } #else {
    #require("ggplot2")
  #}

  # Specify the number of 'positions' between each category
  gap_size <- 4

  if(ewas==TRUE){
    if("Variable_pvalue" %in% names(d) & "LRT_pvalue" %in% names(d)){
      d$pvalue <- ifelse(!is.na(d$Variable_pvalue), d$Variable_pvalue, ifelse(!is.na(d$LRT_pvalue), d$LRT_pvalue, NA))
    } else if("Variable_pvalue" %in% names(d)){
      d$pvalue <- d$Variable_pvalue
    } else if("LRT_pvalue" %in% names(d)){
      d$pvalue <- d$LRT_pvalue
    }
  }

  if(is.null(groups)==TRUE){
    if("Shape" %in% names(d)){
      p <- ggplot2::ggplot() + ggplot2::geom_point(data=d, aes(x=factor(Variable), y=-log10(pvalue), shape=factor(Shape)))
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, hjust=0.95, vjust=0.2),
                              axis.title.x=ggplot2::element_blank(),
                              legend.position="bottom",
                              legend.title=ggplot2::element_blank())
    } else {
      p <- ggplot2::ggplot(d, aes(x=factor(Variable), y=-log10(pvalue)))
      p <- p + ggplot2::geom_point()
      p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90),
                              axis.title.x=ggplot2::element_blank())
    }
  } else {

    subd <- d[, colnames(d) %in% c("Variable", "pvalue", "Shape")]
    colnames(groups)[2] <- "Color"
    dg <- merge(subd, groups, by="Variable")

    #Order variables according to group
    dg_order <- dg[order(dg$Color, dg$Variable), ]

    # Make factor, preserving order
    dg_order$Variable <- factor(dg_order$Variable, levels=unique(dg_order$Variable))
    dg_order$Color <- factor(dg_order$Color, levels=unique(dg_order$Color))
    
    # Sequential positions with padding between each group
    dg_order$pos_index <- as.integer(dg_order$Variable) + (gap_size * as.integer(dg_order$Color))

    #Set up dataframe with color and position info
    maxRows <- by(dg_order, dg_order$Color, function(x) x[which.max(x$pos_index),])
    minRows <- by(dg_order, dg_order$Color, function(x) x[which.min(x$pos_index),])
    milimits <- do.call(rbind, minRows)
    malimits <- do.call(rbind, maxRows)
    lims <- merge(milimits, malimits, by="Color")
    if("Shape" %in% names(dg)){
      names(lims) <- c("Color", "Varx", "px", "Shapex", "posmin", "Vary", "py", "Shapey", "posmax")
    } else {
      names(lims) <- c("Color", "Varx", "px", "posmin", "Vary", "py", "posmax")
    }
    lims$av <- (lims$posmin + lims$posmax)/2
    lims$shademap <- rep(c("shade_ebebeb","shade_fffff"), each=1, length=nrow(lims))

    #Set up color palette
    ncolors <- nlevels(lims$Color)
    if(morecolors==TRUE){
      if (!requireNamespace(c("RColorBrewer"), quietly = TRUE)==TRUE) {
        stop("Please install RColorBrewer to add color attribute.", call. = FALSE)
      }#else {
       # require("RColorBrewer")
      #}
      getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
      newcols <- c(getPalette(ncolors), "#EBEBEB", "#FFFFFF")
    } else {
      newcols <-c(rep(x=c("#53868B", "#4D4D4D"), length.out=ncolors, each=1), "#EBEBEB", "#FFFFFF")
    }
    names(newcols) <-c(levels(factor(lims$Color)), levels(factor(lims$shademap)))

    #Start plotting
    p <- ggplot2::ggplot() + ggplot2::geom_rect(data = lims,
                                                aes(xmin = posmin - gap_size/2 - 0.5, xmax = posmax + gap_size/2 + 0.5, ymin = 0, ymax = Inf, fill=factor(shademap)),
                                                alpha = 0.7)
    #Add shape info if available
    if("Shape" %in% names(dg)){
      p <- p + ggplot2::geom_point(data=dg_order, aes(x=pos_index, y=-log10(pvalue), color=Color, shape=factor(Shape)))
    } else {
      p <- p + ggplot2::geom_point(data=dg_order, aes(x=pos_index, y=-log10(pvalue), color=Color))
    }
    p <- p + ggplot2::scale_x_continuous(breaks=lims$av, labels=lims$Color, expand=c(0,0))
    # Plot category colors at the bottom
    p <- p + ggplot2::geom_rect(data = lims,
                                aes(xmin = posmin - gap_size/2 - 0.5, xmax = posmax + gap_size/2 + 0.5, ymin = -Inf, ymax = 0, fill=Color),
                                alpha = 1)
    p <- p + ggplot2::scale_colour_manual(name = "Color",values = newcols, ggplot2::guides(alpha=FALSE))
    p <- p + ggplot2::scale_fill_manual(name = "Color",values = newcols, ggplot2::guides(alpha=FALSE))
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, hjust=0.95, vjust=0.2),
                            panel.grid.minor.x = ggplot2::element_blank(),
                            panel.grid.major.x=ggplot2::element_blank(),
                            axis.title.x=ggplot2::element_blank(),
                            legend.position="bottom",
                            legend.title=ggplot2::element_blank())
  }

  #Add title and y axis title
  p <- p + ggplot2::ggtitle(title) + ggplot2::ylab(expression(paste("-log"[10], "(p-value)", sep="")))

  #Add pvalue threshold line
  if(!is.null(line)){
    p <- p + ggplot2::geom_hline(yintercept = -log10(line), colour="red")
  }
  print(paste("Saving plot to ", file, ".png", sep=""))
  ggplot2::ggsave(p, filename=paste(file, ".png", sep=""), dpi=res, units="in", height=hgt, width=wi)
  print(p)

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(p)
}

