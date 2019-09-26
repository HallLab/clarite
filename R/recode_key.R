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
    # Get the variable name where replacement is occuring
    variable_name <- as.character(key[row, "Variable"])
    
    # Ensure the variable name is present in df
    if(!is.element(variable_name, names(df))){
      stop(paste(c(variable_name, " was listed as a variable in the key but isn't a column in df"), sep=" "))
    }

    # Get the replacement value
    replaced_value  <- key[row, "Missing.Value"]

    # Convert the replaced value into a string, since it may be a factor in the key
    # Any other necessary conversion (for comparison to a numeric or factor) will be done automatically
	  replaced_value <- as.character(replaced_value)

    # Replace with NA
	  df[[variable_name]][df[[variable_name]]==replaced_value] <- NA
  }
  
  # Replace empty and "NA" strings
  df[df==""|df=="NA"] <-NA

  #Drop unused factor levels
  df <- droplevels(df)

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(df)

}


