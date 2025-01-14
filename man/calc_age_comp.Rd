% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_age_comp.R
\name{calc_age_comp}
\alias{calc_age_comp}
\title{Calculate total numbers at age.}
\usage{
calc_age_comp(racebase_tables = NULL, size_comp = NULL)
}
\arguments{
\item{racebase_tables}{data object created from \code{AFSC.GAP.DBE::get_data()}}

\item{size_comp}{a dataframe of stratum abundances, result
object from either \code{calc_size_stratum_AIGOA()}
or \code{calc_size_stratum_BS()} depending on the
region.}
}
\description{
To be used for the GOA/AI regions. Estimates are only for the
entire region, not by stratum.
}
