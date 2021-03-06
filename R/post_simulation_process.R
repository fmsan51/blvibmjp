#' Read cow information from simulation csvs
#'
#' Read information of cows which owned by a farm at the end of simulations from csv files and redefine infection routes.
#'
#' @param param,output_filename,output_dir See [param].
#' @param i_simulation csvs with this numbers are read.
#'
#' @name read_cows
#' @return A [cow_table] with an additional column `i_simulation`.
read_cows <- function(param,
                      output_filename = param$output_filename,
                      output_dir = param$output_dir,
                      i_simulation = seq_len(param$n_simulation)) {
  all_simulations <- vector("list", length(i_simulation))
  paths <- construct_filepath(output_filename, i_simulation, output_dir)
  for (i in i_simulation) {
    cows <- fread(paths[i],
      # Setting following arguments improve speed of fread() by 1.08 times
      sep = ",", header = T, na.strings = NULL, verbose = F, skip = 0,
      integer64 = "integer64", data.table = T, logical01 = F)
    cows$i_simulation <- i_simulation[i]
    all_simulations[[i]] <- cows
  }
  all_simulations <- rbindlist(all_simulations)
  return(all_simulations)
}


#' @param route_levels,route_labels,drop,gather See [redefine_route_levels].
#' @name read_cows
read_final_cows <- function(param, route_levels = NULL, route_labels = NULL,
                            output_filename = param$output_filename,
                            output_dir = param$output_dir,
                            i_simulation = seq_len(param$n_simulation),
                            drop = T, gather = T) {
  cows <- read_cows(param, output_filename, output_dir, i_simulation)
  cows <- cows[is_owned == T & i_month == max(i_month), ]
  cows <- redefine_route_levels(cows, drop, language = NULL, route_levels,
                                route_labels, gather = gather)
  return(cows)
}


#' Calculate prevalence from `cow_table` or a csv file
#'
#' Calculate monthly prevalences from csv files or list of `cow_table`. Set either one of `output_dir`+`output_filename` or `csv`.
#'
#' @param param,output_filename,output_dir See [param].
#' @param i_simulation csvs with this numbers are used.
#' @param list_cows List consisted of `cow_table`s. Specify one of `output_dir`+`output_filename` or `list_cows`.
#' @param type `prop` means proportion of infected cows. `count` means the number of infected and non-infected cows. `status` means the number of `s` (non-infected), `ial` (asymptomatic), `ipl` (persistent lymphositosis) and `ebl` cows. `route` means the number of infected cows by each infection route.
#' @param gather When `type` is "route", whether infection routes as "tie_exposed_baseline", "tie_exposed_risk", "tie_non_exposed" and "free" should be gathered to one category "insects" or treated as-is.
# @param drop When `type` is "route", whether drop infection routes not contained in a simulation result from a resulting table of the function.
#' @param by_simulation Whether calculate median of all simulations (`FALSE`) or calculate by each simualtion (`TRUE`).
#'
#' @return A [data.table][data.table::data.table] contains monthly prevalences.
#'
#' @export
calc_prev <- function(param, output_filename = param$output_filename,
                      output_dir = param$output_dir,
                      i_simulation = seq_len(param$n_simulation),
                      list_cows = NULL,
                      type = c("prop", "count", "status", "route"),
                      gather = T, drop = T, by_simulation = F) {
  if (is.null(list_cows)) {
    cows <- read_cows(param, output_filename, output_dir, i_simulation)
  } else {
    cows <- rbindlist(list_cows, idcol = "i_simulation")
  }
  cows <- cows[is_owned == T, ]

  type <- match.arg(type)
  if (type == "prop") {
    prevalences <- cows[,
      list(prevalence = sum(.SD$infection_status != "s") / .N),
      by = list(i_month, i_simulation)
      ][!is.na(i_month), ]
    if (!by_simulation) {
      prevalences <- prevalences[,
        list(prevalence = median(prevalence)), by = i_month]
    }
  } else if (type == "count") {
    cows$is_infected <-
      factor(cows$infection_status != "s", levels = c("TRUE", "FALSE"),
             labels = c("inf", "noinf"))
    prevalences <-
      dcast.data.table(cows, i_month + i_simulation ~ is_infected,
                       fun.aggregate = length, drop = F)
    if (!by_simulation) {
      prevalences <-
        prevalences[, lapply(.SD, median), .SDcols = c("inf", "noinf"),
                    by = i_month]
    }
  } else if (type == "status") {
    cows$infection_status <-
      factor(cows$infection_status, levels = c("s", "ial", "ipl", "ebl"))
    prevalences <-
      dcast.data.table(cows, i_month + i_simulation ~ infection_status,
                       fun.aggregate = length, drop = F)
    if (!by_simulation) {
      prevalences <-
        prevalences[, lapply(.SD, median),
                    .SDcols = c("s", "ial", "ipl", "ebl"), by = i_month]
    }
  } else {
    cows <-
      redefine_route_levels(cows, drop = drop, language = NULL, gather = gather)
    prevalences <-
      dcast.data.table(cows, i_month + i_simulation ~ cause_infection,
                       fun.aggregate = length, drop = F)
    if (!by_simulation) {
      prevalences <-
        prevalences[, lapply(.SD, median),
                    .SDcols = setdiff(colnames(prevalences), "i_month"),
                    by = i_month]
    }
  }

  return(prevalences)
}


#' Plot the change in prevalence
#'
#' Plot monthly prevalences from csv files or list of `cow_table`. Set either one of `output_dir`+`output_filename` or `csv`.
#'
#' @param param,output_filename,output_dir See [param].
#' @param i_simulation csvs with this numbers are used.
#' @param list_cows List consisted of `cow_table`s. Specify one of `output_dir`+`output_filename` or `list_cows`.
#' @param language Language to which translate messages. At present, only English and Japanese is implemented.
#' @param title,xlab,ylab logical or character. Plot a title, a label for x-axis and a label for y-axis. When `TRUE`, the default value is used. When `FALSE` or `NULL`, the title or label is not shown. When specified by character, the string is used as the title or the label.
#' @param font Font in a plot. The default is "Meiryo" for Windows and "Hiragino Kaku Gothic Pro W3" for the other OS.
#'
#' @return An scatterplot by [ggplot2::ggplot] object.
#'
#' @export
plot_prev <- function(param,
                      output_filename = param$output_filename,
                      output_dir = param$output_dir,
                      i_simulation = seq_len(param$n_simulation),
                      list_cows = NULL, language = "English",
                      title = T, xlab = T, ylab = T, font = NULL) {
  prevalences <-
    calc_prev(param, output_filename, output_dir, i_simulation, list_cows,
              type = "prop", by_simulation = F)
  orig_msg <- list(title = title, xlab = xlab, ylab = ylab)
  defined_msg <- define_msg(orig_msg, "plot_prev", language)

  if (grepl("Windows", osVersion, fixed = T)) {
    font <- ifelse(is.null(font), "Meiryo", font)
    eval(parse(text = paste0(
      "windowsFonts(`", font, "` = ", "windowsFont('", font, "'))")))
  } else {
    font <- ifelse(is.null(font), "Hiragino Kaku Gothic Pro W3", font)
  }
  gp <- ggplot(prevalences, aes(x = i_month, y = prevalence)) +
    geom_point() +
    ylim(0, 1) +
    scale_x_continuous(breaks = seq.int(0, max(prevalences$i_month), by = 12)) +
    theme_bw(base_family = font) +
    theme(panel.border = element_blank(), axis.line = element_line())

  plot_labs <- list(title = defined_msg$title,
                    x = defined_msg$xlab,
                    y = defined_msg$ylab)
  gp <- gp + labs(!!!plot_labs)

  return(gp)
}


#' Redefine infection routes
#'
#' Recategorize `cause_infection` column in a `cow_table`.
#'
#' @param cows See [cow_table].
#' @param drop Drop infection routes not in `csv` or `cows` from a legend. Ignored when `route_levels` is specified.
#' @param language Language to which translate messages. At present, only English and Japanese is implemented.
#' @param route_levels If specified, infection routes not specified in `route_levels` are coarced into "other" category. See `cause_infection` in [cow_table] to know about default categories.
#' @param route_labels Specify if you want to rename categories.
#' @param gather Whether infection routes as "tie_exposed_baseline", "tie_exposed_risk", "tie_non_exposed" and "free" should be gathered to one category "insects" or treated as-is.
#'
#' @return A [cow_table] with recategorized `cause_infection`.
#'
#' @export
redefine_route_levels <- function(cows,
                                  drop, language = NULL, route_levels = NULL,
                                  route_labels = NULL, gather = T) {
  cows <- copy(cows)

  cows$cause_infection[cows$infection_status == "s"] <- "uninfected"

  gathered_levels <-
    c("tie_exposed_baseline", "tie_exposed_risk", "tie_non_exposed", "free")
  routes <- c("uninfected", "initial", "insects", gathered_levels, "contact",
              "rp", "vertical", "colostrum", "introduced", "pasture")
  if (is.null(route_levels)) {
    if (drop) {
      route_levels <-
        intersect(routes, c(unique(cows$cause_infection), "insects"))
      # Use intersect to organize order of cause_infection
    } else {
      route_levels <- routes
    }
  }
  uninf_and_route_raw <- unique(c("uninfected", route_levels))
  if (gather) {
    uninf_and_route <-
      intersect(routes, setdiff(uninf_and_route_raw, gathered_levels))
    # Use intersect to organize order of cause_infection
    uninf_and_route_w_gathered <- unique(c(uninf_and_route, gathered_levels))
  } else {
    uninf_and_route <- setdiff(uninf_and_route_raw, "insects")
  }

  contains_all_cause <-
    all(unique(cows$cause_infection) %in% uninf_and_route_raw)
  if (contains_all_cause & gather) {
    cows$cause_infection <- fct_collapse(
        factor(cows$cause_infection, levels = uninf_and_route_w_gathered),
        insects = gathered_levels
      )
  } else if (!contains_all_cause) {
    if (gather & any(gathered_levels %in% uninf_and_route)) {
      cows$cause_infection <- fct_other(cows$cause_infection,
                                        uninf_and_route_w_gathered,
                                        other_level = "other")
      cows$cause_infection <- fct_collapse(
          factor(cows$cause_infection, c(uninf_and_route_w_gathered, "other")),
          insects = gathered_levels
        )
    } else {
      cows$cause_infection <-
        fct_other(cows$cause_infection, uninf_and_route, other_level = "other")
    }
    uninf_and_route <- c(uninf_and_route, "other")
  }
  cows$cause_infection <- factor(cows$cause_infection, levels = uninf_and_route)

  if (!is.null(route_labels)) {
    if (length(route_labels) != length(levels(cows$cause_infection))) {
      stop(glue("Length of route_labels is not equals to the number of\\
                 categories in cause_infection.
                 Did't you forget a label for 'others'?"))
    }
    levels(cows$cause_infection) <- route_labels
  } else if (!is.null(language)) {
    translated_msg <- translate_msg("redefine_route_levels", language)
    levels(cows$cause_infection) <- unlist(translated_msg[uninf_and_route])
  }

  return(cows)
}


#' Plot monthly infection routes nicely
#'
#' Plot average (median) monthly prevalences by each infection route from csv files or list of `cow_table`. Set either one of `output_dir`+`output_filename` or `csv`.
#'
#' @param param,output_filename,output_dir See [param].
#' @param i_simulation csvs with this numbers are used.
#' @param list_cows List consisted of `cow_table`s. Specify one of `output_dir`+`output_filename` or `list_cows`.
#' @param language Language to which translate messages. At present, only English and Japanese is implemented.
#' @param drop Drop infection routes not in `csv` or `cows` from a legend.
#' @param route_levels,route_labels,gather See [redefine_route_levels]
#' @param max_ylim Upper limit of the y-axis of the plot.
#' @param title,legend_title,xlab,ylab logical or character. Plot a title, a legend title, a label for x-axis and a label for y-axis. When `TRUE`, the default value is used. When `FALSE` or `NULL`, the title or the label is not shown. When specified by character, the string is used as the title or the label.
#' @param gray When `TRUE`, a plot will be a grayscale image.
#' @param area_color Specify a color palette of a plot.
#' @param border When `TRUE`, each area in a plot will be surrounded by border.
#' @param border_color Specify a color palette for the border.
#' @param font Font in a plot. The default is "Meiryo" for Windows and "Hiragino Kaku Gothic Pro W3" for the other OS.
#'
#' @return A [ggplot2::ggplot] plot.
#'
#' @export
plot_route <- function(param,
                       output_filename = param$output_filename,
                       output_dir = param$output_dir,
                       i_simulation = seq_len(param$n_simulation),
                       list_cows = NULL, language = "English", drop = T,
                       route_levels = NULL, route_labels = NULL, gather = T,
                       max_ylim = NULL, title = T, legend_title = T,
                       xlab = T, ylab = T, gray = F, area_color = NULL,
                       border = F, border_color = NULL, font = NULL) {
  if (is.null(list_cows)) {
    cows <- read_cows(param, output_filename, output_dir, i_simulation)
  } else {
    cows <- rbindlist(list_cows, idcol = "i_simulation")
  }
  cows <- cows[is_owned == T, ]

  cows <-
    redefine_route_levels(cows, drop, language, route_levels, route_labels,
                          gather = gather)
  orig_msg <- list(title = title, legend_title = legend_title,
                   xlab = xlab, ylab = ylab)
  defined_msg <- define_msg(orig_msg, "plot_route", language)
  infection_route <-
    cows[, .N, by = c("cause_infection", "i_month", "i_simulation")][
      , list(N = median(N)), by = c("cause_infection", "i_month")]
  infection_route <-
    complete(infection_route, i_month, cause_infection, fill = list(N = 0))
  n_cause <- n_distinct(infection_route$cause_infection)
  if (gray) {
    color_specification <- c("area_color", "border_color")
    is_color_specified <-
      !vapply(color_specification, function(x) is.null(get(x)), T)
    if (sum(is_color_specified) != 0) {
      specified_color <- color_specification[is_color_specified]
      warning(glue("Argument(s) {paste(specified_color, collapse = ' and ')} \\
                    is ignored because argument gray_scale is TRUE."))
    }
    area_color <- gray.colors(n_cause, 1, 0, gamma = 1)
    border <- T
    border_color <- rep("#000000", n_cause)
  }
  if (is.null(area_color)) {
    if (n_cause <= 8) {
      area_color <- c(gray(0.85), colorblind_pal()(n_cause)[-1])
    } else {
      message(glue(
        "Note: NOT color-blind friendly palette was choosed \\
         because there are 9+ infection routes."
      ))
      area_color <- c(gray(0.85), hue_pal()(n_cause - 1))
    }
  }

  if (border) {
    color <- expr(cause_infection)
  } else {
    color <- expr(NULL)
  }
  if (grepl("Windows", osVersion, fixed = T)) {
    font <- ifelse(is.null(font), "Meiryo", font)
    eval(parse(text = paste0(
      "windowsFonts(`", font, "` = ", "windowsFont('", font, "'))")))
  } else {
    font <- ifelse(is.null(font), "Hiragino Kaku Gothic Pro W3", font)
  }

  if (is.null(max_ylim)) {
    max_ylim <- max(table(cows$i_month))
  }
  gp <- ggplot(infection_route, aes(x = i_month, y = N)) +
    geom_area(aes(fill = cause_infection, color = !!color)) +
    scale_x_continuous(breaks =
                         seq(0, max(infection_route$i_month, na.rm = T), 6),
                       minor_breaks =
                         seq(0, max(infection_route$i_month, na.rm = T), 3)) +
    ylim(0, max_ylim) +
    scale_fill_manual(values = area_color, drop = F)

  plot_labs <- list(title = defined_msg$title,
                    fill = defined_msg$legend_title,
                    color = defined_msg$legend_title,
                    x = defined_msg$xlab,
                    y = defined_msg$ylab)
  gp <- gp + labs(!!!plot_labs)

  if (!is.null(border_color)) {
    if (border) {
      gp <- gp + scale_color_manual(values = border_color, drop = F)
    } else {
      warning(
        "Argument border_color is ignored because argument border is FALSE."
      )
    }
  }
  gp <- gp + theme_bw(base_family = font) +
    theme(panel.border = element_blank(), axis.line = element_line())
  return(gp)
}


#' Summarize infection routes
#'
#' Calculate monthly infection routes at the end of simulations.
#'
#' @inheritParams read_cows
#' @param drop Drop infection routes not in `csv` or `cows` from a legend.
#'
#' @seealso [table_status]
#' @export
table_route <- function(param, route_levels = NULL, route_labels = NULL,
                        output_filename = param$output_filename,
                        output_dir = param$output_dir,
                        i_simulation = seq_len(param$n_simulation), drop = T
                        ) {
  cows <- read_final_cows(param, route_levels, route_labels,
                          output_filename, output_dir, i_simulation, drop)
  table_route <- cows[, .N, by = list(i_simulation, cause_infection)]
  table_route <- dcast.data.table(table_route, i_simulation ~ cause_infection,
                                  value.var = "N", fill = 0, drop = F)
  cols <- colnames(table_route)
  table_route[, `:=`(total = rowSums(.SD)),
               .SDcols = cols[cols != "i_simulation"]]
  table_route[, `:=`(total_inf = total - uninfected)]
  table_route[, `:=`(p_inf = round(total_inf / total * 100, 2))][]
  # [] to print the result to console
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#why-do-i-have-to-type-dt-sometimes-twice-after-using-to-print-the-result-to-console
  return(table_route)
}


#' Summarize infection status
#'
#' Calculate monthly infection status at the end of simulations.
#'
#' @inheritParams read_cows
#' @param drop Drop infection routes not in `csv` or `cows` from a legend.
#'
#' @seealso [table_route]
#' @export
table_status <- function(param, route_levels = NULL, route_labels = NULL,
                         output_filename = param$output_filename,
                         output_dir = param$output_dir,
                         i_simulation = seq_len(param$n_simulation), drop = T
                         ) {
  cows <- read_final_cows(param, route_levels, route_labels,
                          output_filename, output_dir, i_simulation, drop)
  cows$infection_status <-
    factor(cows$infection_status, levels = c("s", "ial", "ipl", "ebl"))
  table_status <- cows[, .N, by = list(i_simulation, infection_status)]
  table_status <- dcast.data.table(table_status,
                                   i_simulation ~ infection_status,
                                   value.var = "N", fill = 0, drop = F)
  table_status[, `:=`(total = rowSums(.SD)),
               .SDcols = c("s", "ial", "ipl", "ebl")]
  p_status <- table_status[, lapply(.SD, function(x) round(x / total * 100, 2)),
                           .SDcols = c("s", "ial", "ipl", "ebl")]
  colnames(p_status) <- paste0("p_", colnames(p_status))
  table_status <- cbind(table_status, p_status)
  return(table_status)
}


#' Translate plot title and labels
#'
#' Translate plot title and labels to other languages than English.
#' At present, translation only for Japanese is implemented.
#'
#' @param type Type of massages.
#' @param to Language to which translate messages. At present, only English and Japanese is implemented.
translate_msg <- function(type, to) {
  res <- msg[[to]][[type]]

  return(res)
}


#' Define plot title and labels
#'
#' Define plot title and labels based on arguments
#'
#' @param original_msg List of original title and labels before passed to [translate_msg()].
#' @param type Type of massages.
#' @param to Language to which translate messages. At present, only English and Japanese is implemented.
define_msg <- function(original_msg, type, to) {
  msg_names <- names(original_msg)
  original_msg_chr <- vapply(original_msg, is.character, T)
  original_msg_false <- vapply(original_msg, function(x) is.logical(x) && !x, T)

  res <- translate_msg(type, to)
  res[vapply(original_msg, is.null, T)] <- NULL
  res[msg_names[original_msg_chr]] <- original_msg[original_msg_chr]
  res[msg_names[original_msg_false]] <- NULL
  return(res)
}

