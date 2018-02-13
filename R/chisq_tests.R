#' chisq_tests
#'
#' Create table of chi-square tests for each variable
#' @param df data frame, with IID as first column
#' @return data frame with two columns, Variable and P.Value
#' @export
#' @examples
#' chisq_tests(df)

chisq_tests <- function(df) {

  chi <- function(v){
    freq_tab <- table(v)
    chisq <- chisq.test(freq_tab)
    return(chisq$p.value)
  }

  out <- as.data.frame(sapply(df[,-1], chi))
  out <- cbind(rownames(out), data.frame(out, row.names=NULL))
  names(out) <- c("Variable", "p-value")

  return(out)
}
