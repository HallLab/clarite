#' @section Motivation:
#'
#' CLARITE was created to provide an easy-to-use tool for analysis of traits and exposures.
#' It exists in several forms:
#' \itemize{
#'    \item This R package
#'    \item \href{https://www.hall-lab.org/clarite-python/index.html}{A python package} (recommended)
#'    \item \href{https://www.hall-lab.org/clarite-python/cli.html}{A command line program} (part of the python package)
#'    \item \href{https://github.com/HallLab/clarite-gui}{A GUI tool} (coming soon)
#' }
#' 
#' @section Example Analysis:
#' 
#' An example analysis can be viewed here: \url{http://rpubs.com/HallLabDocs/clarite_example_analysis}
#'
#' @section Categorizing variables:
#'
#' It is important to accurately categorize variables in order to correctly utilize them in any QC and analysis steps.
#' Depending on the type of a variable (categorical, continuous, or binary) it should be plotted or analyzed differently.
#' Variables of different types should be separated into individual data.frames manually, or using the heuristic method 
#' provided by the various \emph{get} functions.
#'
#' @section Kinds of functions that are provided:
#' 
#' The CLARITE package provides many useful functions which could be grouped into a few categories:
#' describe, modify, analyze, and plot.
#' 
#' \emph{Describe} functions:
#' These are functions related to summary statistics and tests
#' 
#' \itemize{
#'  \item \code{\link{chisq_tests}}
#'  \item \code{\link{correlations}}
#'  \item \code{\link{freq_tables}}
#'  \item \code{\link{outlier_impact}}
#'  \item \code{\link{sample_size}}
#' }
#'
#' \emph{Modify} functions:
#' These are functions related to manipulating and normalizing data
#' 
#' \itemize{
#'  \item \code{\link{colfilter}}
#'  \item \code{\link{get_binary}}
#'  \item \code{\link{get_categorical}}
#'  \item \code{\link{get_check}}
#'  \item \code{\link{get_continuous}}
#'  \item \code{\link{get_uniq}}
#'  \item \code{\link{merge_data}}
#'  \item \code{\link{min_cat_n}}
#'  \item \code{\link{min_n}}
#'  \item \code{\link{recode_key}}
#'  \item \code{\link{recode_missing}}
#'  \item \code{\link{remove_incomplete_obs}}
#'  \item \code{\link{remove_outliers}}
#'  \item \code{\link{rowfilter}}
#'  \item \code{\link{transvar}}
#' }
#'
#' \emph{Analyze} functions:
#' These are functions related to performing an EWAS analysis using regression
#' 
#' \itemize{
#'  \item \code{\link{ewas}}
#'  \item \code{\link{ewas_pval_adjust}}
#' }
#'
#' \emph{Plot} functions:
#' These are functions related to generating plots
#' 
#' \itemize{
#'  \item \code{\link{bar_plot}}
#'  \item \code{\link{box_plot}}
#'  \item \code{\link{eman}}
#'  \item \code{\link{hist_plot}}
#'  \item \code{\link{multi_plot}}
#'  \item \code{\link{qq_plot}}
#' }
#'
#' @docType package
#' @name clarite
NULL