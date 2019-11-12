% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barn.R
\name{remove_from_areas}
\alias{remove_from_areas}
\title{Remove cows from areas}
\usage{
remove_from_areas(cows, removed_cow_id)
}
\arguments{
\item{cows}{See \link{cow_table}.}

\item{removed_cow_id}{\code{cow_id} of cows to be removed from current areas.}
}
\value{
A \link{cow_table} in which \code{area_id} and \code{chamber_id} of specified cows are set as \code{NA}.
}
\description{
Assign \code{NA}s to \code{area_id} and \code{chamber_id} of specified cows.
}