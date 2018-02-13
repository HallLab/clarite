#' correlations
#'
#' Identify variables that are correlated above a certain threshold
#' Note: Uses the pairwise.complete.obs in the base R cor function
#' Note: A threshold of 0.6 also includes variables with a negative correlation stronger than -0.6
#' @param df data frame
#' @param x correlation threshold, -1 <= x <= 1
#' @return data frame with three columns, Variable 1, Variable 2, and the correlation between Variable 1 and Variable 2
#' @export
#' @examples
#' correlations(df, x=0)

correlations <- function(df, x=0){

  df <- as.matrix(df)
  class(df) <- "numeric"

  #Make correlation matrix
  m <- cor(df, use="pairwise.complete.obs")
  d <- data.frame(Cor=as.vector(m), Var_1=rownames(m), Var_2=rep(rownames(m), each=nrow(m)))
  #Only use the upper triangle
  d <- d[as.vector(upper.tri(m, TRUE)),]

  c <- abs(x)
  #Discard correlations of 1
  d <- d[(d$Cor > c | d$Cor < -c) & (d$Cor !=1 & d$Cor !=-1), ]
  d <- d[!is.na(d$Cor), ]

  return(d)

}
