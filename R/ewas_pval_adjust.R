#' ewas_pval_adjust
#'
#' Add corrected p-values to the output of EWAS
#' @param ewas_result data frame of ewas result
#' @param bonferroni boolean - calculate bonferroni correction (default=TRUE)
#' @param fdr boolean - calculate fdr correction (default=TRUE)
#' @return the input data frame with added columns for the adjusted p-values
#' @export
#' @family analysis functions
#' @examples
#' \dontrun{
#' ewas_pval_adjust(ewas_result)
#' }

ewas_pval_adjust <- function(ewas_result, bonferroni=TRUE, fdr=TRUE){
  t1 <- Sys.time()

  # Ensure the input is correct
  expected_col_names <- list("Variable", "N", "Converged",  "Beta", "SE", "Variable_pvalue",
                             "pval", "LRT_pvalue", "Diff_AIC", "phenotype")
  missing_col_names <- setdiff(expected_col_names, names(ewas_result))
  if(length(missing_col_names) > 0){
    stop("Missing expected columns from the input ewas_result dataframe: ",  paste(missing_col_names, collapse=", "))
  }
 
  if(bonferroni){
    ewas_result$pvalue_Bonf <- stats::p.adjust(ewas_result$pval, method="bonferroni")
    ewas_result <- ewas_result[order(ewas_result$pvalue_Bonf), ]
  }
  if(fdr){
      ewas_result$pvalue_FDR <- stats::p.adjust(ewas_result$pval, method="fdr")
      ewas_result <- ewas_result[order(ewas_result$pvalue_FDR), ]
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(ewas_result)
}