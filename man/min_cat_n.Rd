% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/min_cat_n.R
\name{min_cat_n}
\alias{min_cat_n}
\title{min_cat_n}
\usage{
min_cat_n(df, n = 200, skip = NULL, only = NULL)
}
\arguments{
\item{df}{data frame}

\item{n}{minimum sample size desired}

\item{skip}{list of column names that are not filtered}

\item{only}{list of column names that are filtered, any others are skipped}
}
\value{
data frame containing only those variables with at least n samples per factor level
}
\description{
Keep variables with a minimum of n samples
}
\examples{
require(NHANES)
data(NHANES)
ncol(min_cat_n(get_categorical(NHANES)))
}
\seealso{
Other filter functions: \code{\link{colfilter}},
  \code{\link{get_binary}}, \code{\link{get_categorical}},
  \code{\link{get_check}}, \code{\link{get_continuous}},
  \code{\link{min_n}}, \code{\link{remove_incomplete_obs}},
  \code{\link{remove_outliers}}, \code{\link{rowfilter}}
}
\concept{categorical variables functions}
\concept{filter functions}
