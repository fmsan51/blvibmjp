% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{set_init_chamber_id}
\alias{set_init_chamber_id}
\title{Initial assignment of \code{chamber_id}}
\usage{
set_init_chamber_id(init_cows, area_table, areas)
}
\arguments{
\item{init_cows}{\code{init_cows} component of a result of \code{\link[=setup_cows]{setup_cows()}}.}

\item{area_table}{A result of \code{\link[=setup_area_table]{setup_area_table()}}.}

\item{areas}{A result of \code{\link[=setup_tie_stall_table]{setup_tie_stall_table()}}.}
}
\value{
A list consisted of \code{cows} and \code{areas}.
}
\description{
Initial assignment of \code{chamber_id}
}
