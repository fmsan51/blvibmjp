% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter.R
\name{process_param}
\alias{process_param}
\title{Calculate parameters based on other parameters}
\usage{
process_param(cows, param)
}
\arguments{
\item{cows}{See \link{cow_table}.}

\item{param}{See \link{param}.}
}
\value{
A list of calculated parameters.
}
\description{
\itemize{
\item \code{param_output_filename}: Name of a file to which output simulation parameters.
\item \code{herd_size_limits}: Lower and upper limits of the number of cattle should be kept in the herd.
\item \code{max_herd_size}: The maximum herd size allowed in a simulation. Used to reserve memory to store cow data while simulation.
\item \code{init_max_cow_id}: The largest \code{cow_id} in \code{cows}.
\item \code{prob_rep}: The result of \code{\link[=set_prob_rep]{set_prob_rep()}}. The probability that a newborn female calf will be a replacement cow.
}
}
\details{
Parameters processed by \code{\link[=process_param]{process_param()}} are deteministic. Parameters calculated by \code{\link[=calc_param]{calc_param()}} are stochastic.
}
