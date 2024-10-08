% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping_effect_names.R
\name{map_metric_name}
\alias{map_metric_name}
\title{Associate the parameter name with the associated metric}
\usage{
map_metric_name(name)
}
\arguments{
\item{name}{A character string representing the element to be mapped.}
}
\value{
A named list containing the mapped value for the input element.
}
\description{
This function takes a name as input and returns its match from a predefined list. If the element is not found in the match, an error is generated. This function is used to determine which metric a parameter estimate is based on.
}
\examples{
map_metric_name("eff_expt_death_geno1")
map_metric_name("eff_ratio_expt_death_geno1")
map_metric_name("int_sex_ratio")
}
\keyword{internal}
