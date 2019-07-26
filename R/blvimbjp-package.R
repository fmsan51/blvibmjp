#' blvibmjp: Simulation model of BLV infection in a Japanese dairy herd.
#'
#' This package provides functions to construct a individual based model of simulation of Bovine Leukemia virus in a tipical Japanese dairy herd.
#'
#' @importFrom data.table copy data.table dcast.data.table fread fwrite melt.data.table rbindlist shift := .N .SD
#' @importFrom forcats fct_other
#' @importFrom glue glue
#' @importFrom ggplot2 aes geom_area geom_point ggplot labs scale_fill_manual scale_x_continuous xlim ylim
#' @importFrom lubridate day dmy dym interval mdy months myd today tz ydm ymd
#' @importFrom rlang ensyms eval_tidy expr enquos quos_auto_name !!!
#' @importFrom stats pnorm qnorm rbinom rexp rgamma rnorm runif rweibull
#' @importFrom stringr str_replace_all
#' @importFrom tidyr complete
#' @importFrom utils str
"_PACKAGE"

