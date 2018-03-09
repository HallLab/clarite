#' recode_missing
#'
#' Recode if there is one missing value for entire dataset
#' Note: this is not recommended if the value to be changed to NA is a float
#' @param df data frame
#' @param na_val value to replace with NA
#' @return data frame with missing value recoded to NA
#' @export
#' @examples
#' recode_missing(df, na_val)

recode_missing <- function(df, na_val) {
  
  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }
  #Replace user value, missing, and empty cells with NA
  df[df==na_val] <- NA
  df[df==""|df=="NA"] <-NA

  #Drop factor levels
  df <- droplevels(df)

  return(df)
}


