#' get_binary
#'
#' Identify binary variables from a data frame of mixed variable types\cr
#' Comparison operator used is =\cr
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lev number of levels desired
#' @return data frame containing only ID and variables with the number of levels specified
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' head(get_binary(NHANES))

get_binary <- function(df, lev=2){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_bin <- data.frame()
  df_bin <- df[, sapply(df, function(col) length(unique(col[!is.na(col)])) == lev), drop=FALSE]

  if(!"ID" %in% colnames(df_bin) & "ID" %in% colnames(df)) {
    df_bin <- cbind(df$ID, df_bin)
    names(df_bin)[[1]] <- "ID"
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df_bin)

}
