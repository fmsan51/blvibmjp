#' blvibmjp: Simulation model of BLV infection in a Japanese dairy herd.
#'
#' This package provides functions to construct a individual based model of simulation of Bovine Leukemia virus in a tipical Japanese dairy herd.
#'
#' @importFrom cellranger cell_limits
#' @importFrom data.table as.data.table CJ copy data.table dcast.data.table fifelse fcoalesce fread fwrite melt.data.table rbindlist setorder shift nafill := .N .SD
#' @importFrom dplyr n_distinct
#' @importFrom forcats fct_other
#' @importFrom ggplot2 aes element_blank element_line geom_area geom_point ggplot labs scale_fill_manual scale_color_manual scale_x_continuous theme theme_bw xlab xlim ylab ylim
#' @importFrom ggthemes colorblind_pal
#' @importFrom glue glue
#' @importFrom lubridate day dmy dym interval mdy myd today tz ydm ymd
#' @importFrom readxl read_excel
#' @importFrom rlang ensyms eval_tidy expr enquos quos_auto_name !!!
#' @importFrom stats pnorm qnorm rbinom rexp rgamma rnorm runif rweibull
#' @importFrom stringr str_replace_all
#' @importFrom tidyr complete
#' @importFrom utils menu str
"_PACKAGE"

