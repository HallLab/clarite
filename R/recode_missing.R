#' recode_missing
#'
#' Recode if there is one missing value for entire dataset\cr
#' Empty strings ("") and "NA" will be replaced by NA regardless of keys\cr
#' \strong{Note}: Replaced values must be a factor, integer, or numeric (other values are ignored).  Float values may fail to convert due to rounding.
#' @param df data frame
#' @param na_val value to replace with NA
#' @return data frame with missing value recoded to NA
#' @export
#' @family functions for recoding missing values
#' @examples
#' # Create an example dataset
#' df <- data.frame("ID" = 1:3,
#'                  "Age" = c(21,15, 34),
#'                  "Name" = c("John","Jane", "Jill"),
#'                  "Value1" = c("a", "b", "NA"),
#'                  "Value2" = c(1, 2, 99))
#' # Recode 99 and "NA" to NA
#' recoded <- recode_missing(df, 99)

recode_missing <- function(df, na_val) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }
  #Replace user value, missing, and empty cells with NA
  df[df==na_val] <- NA
  df[df==""|df=="NA"] <-NA

  #Drop factor levels
  df <- droplevels(df)

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df)
}


