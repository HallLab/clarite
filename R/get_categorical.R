#' get_categorical
#'
#' Identify categorical variables from a data frame of mixed variable types
#' Comparison operators used are <= and >=
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lower minimum number of unique values to be considered categorical
#' @param upper maximum number of unique values to be considered categorical
#' @return data frame containing only ID and variables meeting the criteria
#' @export
#' @examples
#' get_categorical(df, lower=3, upper=6)

get_categorical <- function(df, lower=3, upper=6) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_cat <- data.frame()
  df_cat <- df[, sapply(df, function(col) (length(unique(col[!is.na(col)])) >= lower & length(unique(col[!is.na(col)])) <= upper)), drop=FALSE]

  if(!"ID" %in% colnames(df_cat) & "ID" %in% colnames(df)) {
    df_cat <- cbind(df$ID, df_cat)
    names(df_cat)[[1]] <- "ID"
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df_cat)

}
