% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/min_n.R
\name{min_n}
\alias{min_n}
\title{min_n}
\usage{
min_n(df, n = 200, skip = NULL, only = NULL)
}
\arguments{
\item{df}{data frame}

\item{n}{minimum sample size desired}

\item{skip}{list of column names that are not filtered}

\item{only}{list of column names that are filtered, any others are skipped}
}
\value{
data frame containing only those variables with at least n samples
}
\description{
Keep variables with a minimum of n samples
}
\examples{
require(NHANES)
data(NHANES)
ncol(min_n(NHANES, n = 1000))
}
\seealso{
Other filter functions: \code{\link{colfilter}},
  \code{\link{get_binary}}, \code{\link{get_categorical}},
  \code{\link{get_check}}, \code{\link{get_continuous}},
  \code{\link{min_cat_n}},
  \code{\link{remove_incomplete_obs}},
  \code{\link{remove_outliers}}, \code{\link{rowfilter}}
}
\concept{filter functions}
