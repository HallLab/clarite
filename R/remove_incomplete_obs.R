#' remove_incomplete_obs
#'
#' Remove incomplete observations from a data frame\cr
#' This function removes any rows that have an NA value in one or more of the specified columns
#' @param d data frame with first column as ID
#' @param cols list of columns that cannot contain NA values
#' @return data frame with rows removed
#' @export
#' @family filter functions
#' @examples
#' \dontrun{
#' result <- remove_incomplete_obs(d, c("column1", "column2"))
#' }

remove_incomplete_obs <- function(d, cols){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(d))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  # Ensure all specified columns exist in the dataframe
  missing_cols <- setdiff(cols, names(d))
  if(length(missing_cols)>0) {
      stop("Some specified columns were not found in the dataframe: ", paste(missing_cols, collapse=", "))
  }
  
  subset <- d[, cols]
  complete <- complete.cases(subset)

  print(paste(sum(!complete), "of", nrow(d), "rows removed due to NA values in the specified columns", sep=" "))
  
  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(d[complete,])
}
