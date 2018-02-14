#' freq_tables
#'
#' Create frequency tables for each variable
#' Gives the number of observations in each category for all variables in file
#' @param df data frame with IID as first column
#' @return data frame with three columns, Variable, Category, and N
#' @export
#' @examples
#' freq_tables(df)

freq_tables <- function(df) {

  if(is.element('IID', names(df))==FALSE){
    stop("Please add IID to dataframe as column 1")
  }

  tab_n <- lapply(df[, -1], function(v){
    as.data.frame(table(v, exclude=NULL))
  })

  tab_n <- do.call(rbind, tab_n)
  tab_n <- cbind(rownames(tab_n), data.frame(tab_n, row.names=NULL))
  names(tab_n) <- c("Variable", "Value", "N")
  tab_n$Variable<- sub("^(.*)[.].*", "\\1", tab_n$Variable)

  return(tab_n)

}
