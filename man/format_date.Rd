% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{format_date}
\alias{format_date}
\title{Format and validate date columns}
\usage{
format_date(
  cows,
  date_format = "ymd",
  today = lubridate::today(tzone = "Asia/Tokyo")
)
}
\arguments{
\item{cows}{A \link{cow_table}.}

\item{date_format}{One of "ymd", "ydm", "myd", "mdy", "dym", "dmy".}

\item{today}{A \code{date} class object indicates today.}
}
\value{
A \link{cow_table} whose date columns are formatted.
}
\description{
Format and validate date columns
}
