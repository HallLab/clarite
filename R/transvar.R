#' transvar
#'
#' Transform user list of columns and transform type for each column (sqrt,log)
#' Can mix transform types but can only do one transform per variable at a time
#' @param df data frame
#' @param key data frame with columns for variable and transform type
#' @return data frame with the variables transformed according to the key
#' @export
#' @examples
#' transvar(df, key)

transvar <- function(df, key) {

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  t_key <- data.frame(t(key))
  names(t_key) <- as.character(unlist(t_key[1,]))

  for(n in names(t_key)){
    #Get the function to preform
    fun <- as.character(t_key[[n]][2])
    df[,n] <- do.call(fun, list(df[,n]))
  }

  return(df)

}
