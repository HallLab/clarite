% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ewas_pval_adjust.R
\name{ewas_pval_adjust}
\alias{ewas_pval_adjust}
\title{ewas_pval_adjust}
\usage{
ewas_pval_adjust(ewas_result, bonferroni = TRUE, fdr = TRUE)
}
\arguments{
\item{ewas_result}{data frame of ewas result}

\item{bonferroni}{boolean - calculate bonferroni correction (default=TRUE)}

\item{fdr}{boolean - calculate fdr correction (default=TRUE)}
}
\value{
the input data frame with added columns for the adjusted p-values
}
\description{
Add corrected p-values to the output of EWAS
}
\examples{
\dontrun{
ewas_pval_adjust(ewas_result)
}
}
\seealso{
Other analysis functions: \code{\link{ewas}}
}
\concept{analysis functions}
