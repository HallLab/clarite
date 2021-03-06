% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_incomplete_obs.R
\name{remove_incomplete_obs}
\alias{remove_incomplete_obs}
\title{remove_incomplete_obs}
\usage{
remove_incomplete_obs(d, cols)
}
\arguments{
\item{d}{data frame with first column as ID}

\item{cols}{list of columns that cannot contain NA values}
}
\value{
data frame with rows removed
}
\description{
Remove incomplete observations from a data frame\cr
This function removes any rows that have an NA value in one or more of the specified columns
}
\examples{
\dontrun{
result <- remove_incomplete_obs(d, c("column1", "column2"))
}
}
\seealso{
Other filter functions: \code{\link{colfilter}},
  \code{\link{get_binary}}, \code{\link{get_categorical}},
  \code{\link{get_check}}, \code{\link{get_continuous}},
  \code{\link{min_cat_n}}, \code{\link{min_n}},
  \code{\link{remove_outliers}}, \code{\link{rowfilter}}
}
\concept{filter functions}
