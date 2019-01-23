#' get_check
#'
#' Identify ambiguous type variables from a data frame of mixed variable types
#' Comparison operators used are =
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lower lower bound of unique values
#' @param upper upper bound of unique values
#' @return data frame containing only ID and variables meeting criteria, which should then be assessed manually
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES) 
#' head(get_check(NHANES))

get_check <- function(df, lower=6, upper=15) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_check <- data.frame()
  df_check <- df[, sapply(df, function(col) (length(unique(col[!is.na(col)])) > lower & length(unique(col[!is.na(col)])) < upper)), drop=FALSE]

  if(!"ID" %in% colnames(df_check) & "ID" %in% colnames(df)) {
    df_check <- cbind(df$ID, df_check)
    names(df_check)[[1]] <- "ID"
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df_check)
}

