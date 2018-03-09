#' recode_key
#'
#' Recode based on a data key if multiple columns have different missing values
#' Note: this is not recommended if the value to replace is a float
#' @param df data frame
#' @param key data frame with two columns, "Variable" and "Missing Value"
#' @return data frame with all missing values recoded to NA
#' @export
#' @examples
#' recode_key(df, key)

recode_key <- function(df, key) {

  if(is.element('ID', names(df))==FALSE){
    stop("Please add ID to dataframe as column 1")
  }

  t_key <- data.frame(t(key))
  names(t_key) <- as.character(unlist(t_key[1,]))
  t_key <- t_key[-1,]

  for(i in names(t_key)){
	if(class(type.convert(as.character(t_key[[i]])))=="factor"){
		t_key[[i]] <- factor(t_key[[i]], levels=levels(df[[i]]))
    		df[[i]][df[[i]]==t_key[[i]]] <- NA
	}
	if(class(type.convert(as.character(t_key[[i]])))=="integer" | class(type.convert(as.character(t_key[[i]])))=="numeric"){
		df[[i]][df[[i]]==as.numeric(as.character(t_key[[i]]))] <- NA
	}
  }
  df[df==""|df=="NA"] <-NA

  #Drop factor levels
  df <- droplevels(df)

  return(df)

}


