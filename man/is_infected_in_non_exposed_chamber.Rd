% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability_function.R
\name{is_infected_in_non_exposed_chamber}
\alias{is_infected_in_non_exposed_chamber}
\title{Whether cows are infected in chambers not next to infected cows in tie-stall barns}
\usage{
is_infected_in_non_exposed_chamber(n_cows, month, param_sim)
}
\arguments{
\item{n_cows}{The number of cows.}

\item{month}{The current month (1, 2, ..., 12).}

\item{param_sim}{A list which combined \link{param}, a result of \code{\link[=process_param]{process_param()}} and a result of \code{\link[=calc_param]{calc_param()}}.}
}
\value{
A logical vector.
}
\description{
Whether cows are infected in chambers not next to infected cows in tie-stall barns
}
