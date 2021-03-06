% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{classify_date_column}
\alias{classify_date_column}
\title{Check date_xxx columns whether they are numeric, date or invaild value.}
\usage{
classify_date_column(date_col, date_format)
}
\arguments{
\item{date_col}{date_xxx column}

\item{date_format}{See \link{validate_cow_table}.}
}
\value{
A character one of "numeric", "date" or "invalid".
}
\description{
Check date_xxx columns whether they are numeric, date or invaild value.
}
