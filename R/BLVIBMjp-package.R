#' blvibmjp: Simulation model of BLV infection in a Japanese dairy herd.
#'
#' This package provides functions to construct a individual based model of simulation of Bovine Leukemia virus in a tipical Japanese dairy herd.
#'
#' @importFrom data.table data.table fread fwrite
#' @importFrom rlang expr syms !!!
#' @importFrom stats pnorm qnorm rbinom rexp rgamma rnorm runif rweibull
#' @importFrom utils str
"_PACKAGE"


#' @useDynLib blvibmjp
#' @importFrom Rcpp sourceCpp
#' @exportPattern "^[[:alpha:]]+"
NULL
