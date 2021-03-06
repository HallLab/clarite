% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_data.R
\name{merge_data}
\alias{merge_data}
\title{merge_data}
\usage{
merge_data(df1, df2, union = TRUE)
}
\arguments{
\item{df1}{first dataframe}

\item{df2}{second dataframe}

\item{union}{boolean argument, if TRUE will add NA values, if FALSE will take intersect, default=TRUE}
}
\value{
data frame containing merged data
}
\description{
Merge two datasets
}
\examples{
\dontrun{
merge_data(df1, df2, union=TRUE)
}
}
