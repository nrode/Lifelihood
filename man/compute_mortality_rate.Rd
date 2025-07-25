% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_mortality_rate.R
\name{compute_mortality_rate}
\alias{compute_mortality_rate}
\title{Compute empirical mortality rate}
\usage{
compute_mortality_rate(
  lifelihoodData,
  interval_width,
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL
)
}
\arguments{
\item{lifelihoodData}{\code{lifelihoodData} object created with \code{\link[=lifelihoodData]{lifelihoodData()}}.}

\item{interval_width}{The interval width used to calculate the
mortality rate. For instance, if the time unit for deaths in
the original dataset is days and \code{interval_width} is set to 10,
the mortality rate will be calculated every 10 days for each group.}

\item{max_time}{The maximum time for calculating the mortality
rate. If set to NULL, the time of the last observed death is used.}

\item{min_sample_size}{The minimum number of individuals alive
at the beggining of a time interval for computing the observed mortality rate}

\item{groupby}{covariate(s) over which mortality rate should be
computed (default is \code{NULL}).
\itemize{
\item If NULL, calculates a single overall mortality rate.
\item If \code{"all"}, calculates mortality rate over each combination
of covariates listed in the\code{lifelihoodData} object provided.
\item Otherwise must be a character (\code{"covariate1"}) or a
character vector (\code{c("covariate1", "covariate2")}).
}}
}
\value{
A dataframe with 3 columns: Interval (time interval, based
on \code{interval_width} value), Group (identifier of a given subgroup,
or "Overall" if groupby = NULL), and MortalityRate (mortality rate
at this time).
}
\description{
Calculate the empirical mortality rate over a given interval.
}
\examples{
library(lifelihood)
library(tidyverse)

df <- fakesample |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  )

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

dataLFH <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

mort_df <- compute_mortality_rate(dataLFH, interval_width = 2)
head(mort_df)

mort_df <- compute_mortality_rate(
  dataLFH,
  interval_width = 2,
  groupby = NULL,
  max_time = 170
)
head(mort_df)
}
