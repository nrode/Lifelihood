% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lifelihood.R
\name{BIC}
\alias{BIC}
\alias{BIC.lifelihoodResults}
\title{BIC}
\usage{
\method{BIC}{lifelihoodResults}(object, parameter_name)
}
\arguments{
\item{object}{\code{lifelihoodResults} object from \code{\link[=lifelihood]{lifelihood()}}}

\item{parameter_name}{Name of the parameter. All parameters can be found \href{/articles/2-setting-up-the-configuration-file.html#parameters}{here}.}
}
\value{
The BIC.
}
\description{
S3 method to compute BIC (Akaike information criterion).
}
\seealso{
\code{\link{AIC}}
}
