#' get_continuous
#'
#' Identify continuous variables from a data frame of mixed variable types\cr
#' Comparison operators used are >=\cr
#' NA values do not contribute to level counts
#' @param df data frame of mixed variable types
#' @param lower minimum number of unique values to be considered continuous
#' @return data frame containing only ID and variables meeting the criteria
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' head(get_continuous(NHANES))

get_continuous <- function(df, lower=15) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  df_cont <- data.frame()
  df_cont <- df[, sapply(df, function(col) length(unique(col[!is.na(col)])) >= lower), drop=FALSE]

  if(!"ID" %in% colnames(df_cont) & "ID" %in% colnames(df)){
    df_cont <- cbind(df$ID, df_cont)
    names(df_cont)[[1]] <- "ID"
  }

  typchk <- as.data.frame(sapply(df_cont[, -1], is.numeric))
  typchk <- cbind(rownames(typchk), data.frame(typchk, row.names=NULL))
  names(typchk) <- c("Variable", "Numeric")
  nn <- typchk[typchk$Numeric=="FALSE",]
  if(nrow(nn)>0){
    print(paste("Warning:", paste(nn$Variable, collapse=", "), "may contain non-numeric values."))
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df_cont)
}
