#' recode_key
#'
#' Recode multiple values to NA based on a data key listing values specific to each column.\cr
#' Empty strings ("") and "NA" will be replaced by NA regardless of keys\cr
#' \strong{Note}: Replaced values must be a factor, integer, or numeric (other values are ignored).  Float values may fail to convert due to rounding.
#' @param df data frame
#' @param key data frame with two columns, "Variable" and "Missing.Value"
#' @return data frame with all missing values recoded to NA
#' @export
#' @family functions for recoding missing values
#' @examples
#' # Create an example dataset
#' df <- data.frame("ID" = 1:3,
#'                  "Age" = c(21,15, 34),
#'                  "Name" = c("John","Jane", "Jill"),
#'                  "Value1" = c("a", "b", "NA"),
#'                  "Value2" = c(1, 98, 99))
#' # Recode 'Value1' ("NA" is NA) and 'Value2' (98 and 99 are both NA)
#' key <- data.frame("Variable" = c("Value2", "Value2"),
#'                   "Missing Value" = c(98, 99))
#' # Run the function
#' recoded <- recode_key(df, key)

recode_key <- function(df, key) {
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }
  if(any(names(key) != c("Variable", "Missing.Value"))){
    stop("Columns in the key must be 'Variable' and 'Missing Value'")
  }
  
  for (row in 1:nrow(key)) {
    variable_name <- as.character(key[row, "Variable"])
    replaced_value  <- key[row, "Missing.Value"]
    replaced_value_kind <- class(utils::type.convert(as.character(replaced_value)))

	  if(replaced_value_kind=="factor"){
	    replaced_value <- factor(replaced_value, levels=levels(df[[variable_name]]))
    	df[[variable_name]][df[[variable_name]]==replaced_value] <- NA
	  } else if (replaced_value_kind=="integer" | replaced_value_kind=="numeric") {
	    df[[variable_name]][df[[variable_name]]==as.numeric(as.character(replaced_value))] <- NA
	  } else {
	    print(paste(c("Skipped converting", variable_name, "since it was a", replaced_value_kind)))
	  }
  }
  
  # Replace empty and "NA" strings
  df[df==""|df=="NA"] <-NA

  #Drop unused factor levels
  df <- droplevels(df)

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df)

}


