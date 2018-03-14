#' chisq_tests
#'
#' Create table of chi-square tests for each variable
#' @param df data frame, with ID as first column
#' @return data frame with two columns, Variable and P.Value
#' @export
#' @examples
#' chisq_tests(df)

chisq_tests <- function(df) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  chi <- function(v){
    freq_tab <- table(v)
    chisq <- chisq.test(freq_tab)
    return(chisq$p.value)
  }

  out <- as.data.frame(sapply(df[,-1], chi))
  out <- cbind(rownames(out), data.frame(out, row.names=NULL))
  names(out) <- c("Variable", "p-value")

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(out)
}
