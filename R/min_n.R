#' min_n
#'
#' Keep variables with a minimum of n samples
#' @param df data frame
#' @param n minimum sample size desired
#' @param skip list of column names that are not filtered
#' @param only list of column names that are filtered, any others are skipped
#' @return data frame containing only those variables with at least n samples
#' @export
#' @family filter functions
#' @examples
#' require(NHANES)
#' data(NHANES)
#' ncol(min_n(NHANES, n = 1000))


min_n <- function(df, n=200, skip=NULL, only=NULL){
  t1 <- Sys.time()
  print("Running...")

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  # Create a list of "ignored" columns which are never filtered
  if(!is.null(skip) & !is.null(only)){
    # Can't specify both parameters at the same time
    stop("'skip' and 'only' may not be used at the same time")
  } else if(!is.null(skip)){
    # Ignore columns listed in "skip"
    ignored <- skip
  } else if (!is.null(only)){
    # Ignore all columns except those in "only"
    ignored <- setdiff(names(df), only)
  } else {
    # Don't ignore any columns
    ignored <- list()
  }

  # Record how many were present before filtering
  before <- length(df)

  # Keep columns that are ignored or that have >= n values that aren't NA
  keep <- df[, sapply(df, function(col) ((col %in% ignored) | (length(col[!is.na(col)]))) >= n), drop=FALSE]
  removed <- sum(!keep)

  if(!"ID" %in% colnames(keep) & "ID" %in% colnames(df)) {
    keep <- cbind(df$ID, keep)
    names(keep)[[1]] <- "ID"
  }

  # Log how many were removed
  print(paste(removed, "of", before, "columns removed due to <", n, "non-NA observations (ignoring", length(ignored), "columns)", sep=" "))

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(keep)
}

