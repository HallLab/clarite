#' correlations
#'
#' Identify variables that are correlated above a certain threshold
#' Note: Uses Pearson/pairwise.complete.obs in the base R cor function
#' Note: A threshold of 0.75 also includes variables with a negative correlation stronger than -0.5
#' @param df data frame
#' @param x correlation threshold, -1 <= x <= 1
#' @return data frame with three columns, Variable 1, Variable 2, and the correlation between Variable 1 and Variable 2
#' @export
#' @examples
#' correlations(df, x=0.75)

correlations <- function(df, x=0.75){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }
  df <- as.matrix(df[, -1])
  class(df) <- "numeric"

  #Make correlation matrix
  m <- cor(df, use="pairwise.complete.obs")
  d <- data.frame(Cor=as.vector(m), Var1=rownames(m), Var2=rep(rownames(m), each=nrow(m)))
  #Only use the upper triangle
  d <- d[as.vector(upper.tri(m, TRUE)),]

  c <- abs(x)
  #Discard correlations of 1
  d <- d[(d$Cor > c | d$Cor < -c) & (d$Cor !=1 & d$Cor !=-1), ]
  d <- d[!is.na(d$Cor), ]

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(d)

}
