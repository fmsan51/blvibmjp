% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_function.R
\name{infect}
\alias{infect}
\title{Change infection status of new infected cows}
\usage{
infect(cows, areas, area_table, infected_cow_id, cause, i)
}
\arguments{
\item{cows}{See \link{cow_table}.}

\item{areas}{See \link{setup_area_table} and \link{tie_stall_table}.}

\item{area_table}{See \link{area_table}.}

\item{infected_cow_id}{\code{cow_id} of new infected cows.}

\item{cause}{A cause of infection.}

\item{i}{The number of months from the start of the simulation.}
}
\value{
A list composed of \link{cow_table} and \link{areas}.
}
\description{
Update \code{infection_status}, \code{date_ial} and \code{cause_infection} of \link{cow_table} and \code{cow_status} of \link{areas}.
}
