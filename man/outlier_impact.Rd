% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_impact.R
\name{outlier_impact}
\alias{outlier_impact}
\title{outlier_impact}
\usage{
outlier_impact(df, x = 2.5)
}
\arguments{
\item{df}{data frame with ID as first column}

\item{x}{number of standard deviations away from the mean to qualify as an outlier}
}
\value{
data frame consisting of summary statistics for the input dataframe and potential dataframe if outliers were to be removed at that threshold
}
\description{
Create data frame with outliers and summary statistics for each variable
}
\examples{
require(NHANES)
data(NHANES)
outlier_impact(get_continuous(NHANES), x=3)
}
\seealso{
Other summary functions: \code{\link{chisq_tests}},
  \code{\link{correlations}}, \code{\link{freq_tables}},
  \code{\link{get_uniq}}, \code{\link{sample_size}}

Other continuous variable functions: \code{\link{box_plot}},
  \code{\link{correlations}}, \code{\link{hist_fun}},
  \code{\link{multi_plot}}, \code{\link{qq_fun}},
  \code{\link{qq_plot}}, \code{\link{remove_outliers}}
}
\concept{continuous variable functions}
\concept{summary functions}
