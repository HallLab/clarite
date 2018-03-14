#' freq_tables
#'
#' Create frequency tables for each variable
#' Gives the number of observations in each category for all variables in file
#' @param df data frame with ID as first column
#' @return data frame with three columns, Variable, Category, and N
#' @export
#' @examples
#' freq_tables(df)

freq_tables <- function(df) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  tab_n <- lapply(df[, -1], function(v){
    as.data.frame(table(v, exclude=NULL))
  })

  tab_n <- do.call(rbind, tab_n)
  tab_n <- cbind(rownames(tab_n), data.frame(tab_n, row.names=NULL))
  names(tab_n) <- c("Variable", "Value", "N")
  tab_n$Variable<- sub("^(.*)[.].*", "\\1", tab_n$Variable)

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(tab_n)

}
a0 = read.delim('/Users/deven/Desktop/NHANES.txt', header=TRUE)
newdata <- freq_tables(a0)
write.table(newdata,file='GUI_Output/NHANES_Freq.txt',sep='\t', row.names=FALSE, quote=FALSE)
