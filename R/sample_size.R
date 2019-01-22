#' sample_size
#'
#' Create list of sample_sizes for each variable
#' @param df data frame with ID as first column
#' @return data frame with two columns, "Variable" and "Sample_Size"
#' @export
#' @family summary functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' sample_size(NHANES)

sample_size <- function(df) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_n <- as.data.frame(sapply(df, function(x) (length(na.omit(x)))))
  df_n <- cbind(rownames(df_n), data.frame(df_n, row.names=NULL))
  names(df_n) <- c("Variable", "N")

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df_n)
}
