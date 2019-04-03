# This file contains internal functions that are only meant to be used by other functions in the package

# This function handles processing of 'skip' and 'only' parameters for column-filtering functions
get_unfiltered_cols <- function(df, skip, only){
  if(!is.null(skip) & !is.null(only)){
      # Can't specify both parameters at the same time
      stop("'skip' and 'only' may not be used at the same time")
    
  } else if(!is.null(skip)){
    # Ensure all columns exist
    missing_cols <- setdiff(skip, colnames(df))
    if(length(missing_cols)>0) {
      stop("Some specified columns were not found in the dataframe: ", paste(missing_cols, collapse=", "))
    }
    # Ignore columns listed in "skip"
    ignored <- sapply(colnames(df), function(col_name) col_name %in% skip | col_name == "ID")
  
  } else if (!is.null(only)){
    # Ensure all columns exist
    missing_cols <- setdiff(only, colnames(df))
    if(length(missing_cols)>0) {
      stop("Some specified columns were not found in the dataframe: ", paste(missing_cols, collapse=", "))
    }
    # Ignore all columns except those in "only"
    ignored <- sapply(colnames(df), function(col_name) !(col_name %in% only) | col_name == "ID")
  
  } else {
    # Don't ignore any columns except ID
    ignored <- sapply(colnames(df), function(col_name) col_name == "ID")
  }

  return(ignored)
}