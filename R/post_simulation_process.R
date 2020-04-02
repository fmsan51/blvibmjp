#' Read cow_table at the start of a simulation from a csv and modify data ready to plot
#'
#' Read cow_table from a csv file, extract owned cows, set `i_simulation` column to 1, and redefine infection routes.
#'
#' @param path_to_csv Path to a csv file which contains [cow_table].
#' @param route_levels,route_labels See [redefine_route_levels].
#'
#' @return A [cow_table] with an additional column `i_simulation`.
#'
#' @seealso [read_final_cows]
read_initial_cows <- function(path_to_csv, route_levels = NULL,
                              route_labels = NULL) {
  cows <- fread(path_to_csv)
  cows <- cows[is_owned == T, ]
  cows <- redefine_route_levels(cows, lenguage = NULL,
                                route_levels, route_labels)
  cows$i_simulation <- 0
  return(cows)
}


#' Read cow information at the end of a simulation
#'
#' Read information of cows which owned by a farm at the end of simulations from csv files and redefine infection routes.
#'
#' @param output_filename,output_dir,n_simulation,simulation_length See [param].
#' @param route_levels,route_labels See [redefine_route_levels].
#'
#' @return A [cow_table] with an additional column `i_simulation`.
#'
#' @seealso [read_initial_cows]
read_final_cows <- function(output_filename, output_dir, n_simulation,
                            simulation_length,
                            route_levels = NULL, route_labels = NULL) {
  all_simulations <- vector("list", n_simulation)
  for (i in seq_len(n_simulation)) {
    cows <- fread(construct_filepath(output_filename, i, output_dir))
    cows$i_simulation <- i
    all_simulations[[i]] <- cows
  }
  all_simulations <- rbindlist(all_simulations)
  final_cows <-
    all_simulations[is_owned == T & i_month == simulation_length, ]
  final_cows <- redefine_route_levels(final_cows, language = NULL,
                                      route_levels, route_labels)
  return(final_cows)
}


#' Calculate prevalence from `cow_table` or a csv file
#'
#' Calculate monthly prevalences from `cow_table` or a csv file. Set either one of `cows` or `path_to_csv`.
#'
#' @param cows See `cow_table`
#' @param path_to_csv Path to a csv file.
#'
#' @return A [data.table][data.table::data.table] contains monthly prevalences.
#'
#' @export
calculate_prevalences <- function(cows = NULL, path_to_csv = NULL) {
  stopifnot(sum(is.null(cows), is.null(path_to_csv)) == 1)
  if (is.null(cows)) {
    cows <- fread(path_to_csv)
  }
  cows <- cows[is_owned == T, ]

  prevalences <- cows[,
                      list(prevalence = .SD[infection_status != "s", .N] / .N),
                      by = i_month][!is.na(i_month)]

  return(prevalences)
}


#' Plot the change in prevalence
#'
#' @param simulation_length See [param].
#' @param path_to_csv Path to a simulation output csv file.
#' @param language When set, plot title and labels are translated in this language. At present, only Japanese is implemented.
#' @param title,xlab,ylab logical or character. Plot title, label for x-axis and label for y-axis. When `TRUE`, the default value is used. When `FALSE`, a title is not shown (`TRUE` is valid only for `title`). When specified by character, the string is used as a title or label.
#' @param font Font in a plot. The default is "Meiryo" for Windows and "Hiragino Kaku Gothic Pro" for the other OS.
#'
#' @return An scatterplot by [ggplot2::ggplot] object.
#'
#' @export
plot_prevalences <- function(simulation_length, path_to_csv, language = NULL,
                             title = T, xlab = T, ylab = T, font = NULL) {
  prevalences <- calculate_prevalences(path_to_csv = path_to_csv)
  orig_msg <- list(title = title, xlab = xlab, ylab = ylab)
  translate_msg("plot_prevalences", language)
  default_msg <- list(title = "Change of prevalence",
                      xlab = "Months in simulation",
                      ylab = "Prevalence")
  define_msg(orig_msg, default_msg, language)

  if (grepl("Windows", osVersion, fixed = T)) {
    font <- ifelse(is.null(font), "Meiryo", font)
    eval(parse(text = paste0(
      "windowsFonts(`", font, "` = ", "windowsFont('", font, "'))")))
  } else {
    font <- ifelse(is.null(font), "Hiragino Kaku Gothic Pro", font)
  }
  gp <- ggplot(prevalences, aes(x = i_month, y = prevalence)) +
    geom_point() +
    ylim(0, 1) +
    scale_x_continuous(breaks = seq.int(0, simulation_length, by = 12)) +
    theme_bw(base_family = font) +
    theme(panel.border = element_blank(), axis.line = element_line())
  if (!is.null(title)) {
    gp <- gp + labs(title = title)
  }
  if (!is.null(xlab)) {
    gp <- gp + xlab(xlab)
  }
  if (!is.null(ylab)) {
    gp <- gp + ylab(ylab)
  }
  return(gp)
}


#' Redefine infection routes
#'
#' Recategorize `cause_infection` column in a `cow_table`.
#'
#' @param cows See [cow_table].
#' @param language When set, `route_labels` are translated in this language. At present, only Japanese is implemented.
#' @param route_levels If specified, infection routes not specified in `route_levels` are coarced into "other" category. See `cause_infection` in [cow_table] to know about default categories.
#' @param route_labels Specify if you want to rename categories.
#'
#' @return A [cow_table] with recategorized `cause_infection`.
#'
#' @export
redefine_route_levels <- function(cows, language = NULL, route_levels = NULL,
                                  route_labels = NULL) {
  cows <- copy(cows)

  cows[infection_status == "s", cause_infection := "uninfected"]

  if (is.null(route_levels)) {
    route_levels <- c("uninfected", "initial", "insects", "contact",
                      # "needles",
                      "rp", "vertical", "colostrum", "introduced", "pasture")
  }
  uninf_and_route <- unique(c("uninfected", route_levels))

  if (all(unique(cows$cause_infection) %in% uninf_and_route)) {
    cows$cause_infection <- factor(cows$cause_infection, uninf_and_route)
  } else {
    cows$cause_infection <- fct_other(cows$cause_infection, uninf_and_route,
                                      other_level = "other")
    uninf_and_route <- c(uninf_and_route, "other")
    cows$cause_infection <- factor(cows$cause_infection,
                                   levels = uninf_and_route)
  }

  if (!is.null(language)) {
    route_labels <- T
    translate_msg("redefine_route_levels", language)
    levels(cows$cause_infection) <- route_labels[uninf_and_route]
  } else if (!is.null(route_labels)) {
    if (length(route_labels) != length(levels(cows$cause_infection))) {
      stop(glue("Length of route_labels is not equals to the number of\\
                 categories in cause_infection.
                 Did't you forget a label for 'others'?"))
    }
    levels(cows$cause_infection) <- route_labels
  }

  return(cows)
}


#' Plot monthly infection routes nicely
#'
#' @param path_to_csv Path to an output csv file.
#' @param language When set, plot title and labels are translated in this language. At present, only Japanese is implemented.
#' @param route_levels,route_labels See [redefine_route_levels]
#' @param max_ylim Upper limit of the y-axis of the plot.
#' @param title,legend_title,xlab,ylab logical or character. Plot title, legend title, label for x-axis and label for y-axis. When `TRUE`, the default value is used. When `FALSE`, a title is not shown (`TRUE` is valid only for `title`). When specified by character, the string is used as a title or label.
#' @param gray When `TRUE`, a plot will be a grayscale image.
#' @param area_color Specify a color palette of a plot.
#' @param border When `TRUE`, each area in a plot will be surrounded by border.
#' @param border_color Specify a color palette for the border.
#' @param font Font in a plot. The default is "Meiryo" for Windows and "Hiragino Kaku Gothic Pro" for the other OS.
#'
#' @return A [ggplot2::ggplot] plot.
plot_infection_route <- function(path_to_csv, language = NULL,
                                 route_levels = NULL, route_labels = NULL,
                                 max_ylim = 100, title = T,
                                 legend_title = T, xlab = T, ylab = T,
                                 gray = F, area_color = NULL,
                                 border = F, border_color = NULL, font = NULL) {
  cows <- fread(path_to_csv)
  cows <- cows[is_owned == T, ]
  cows <- redefine_route_levels(cows, language, route_levels, route_labels)
  orig_msg <- list(title = title, legend_title = legend_title,
                   xlab = xlab, ylab = ylab)
  translate_msg("plot_infection_route", language)
  default_msg <- list(title = "Change of prevalence",
                      legend_title = "Infection route",
                      xlab = "Months in simulation",
                      ylab = "Number of cattle")
  define_msg(orig_msg, default_msg, language)
  infection_route <- cows[, .SD[, .N, by = cause_infection], by = i_month]
  infection_route <-
    complete(infection_route, i_month, cause_infection, fill = list(N = 0))
  n_cause <- n_distinct(infection_route$cause_infection)
  if (gray) {
    color_specification <- c("area_color", "border_color")
    is_color_specified <-
      !sapply(color_specification, function(x) is.null(get(x)))
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
    area_color <- c(gray(0.85), colorblind_pal()(n_cause)[-1])
    # TODO: Max #colors can be hundled by colorblind_pal is 8, but there is 10 categories.
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
    font <- ifelse(is.null(font), "Hiragino Kaku Gothic Pro", font)
  }

  gp <- ggplot(infection_route, aes(x = i_month, y = N)) +
    geom_area(aes(fill = cause_infection, color = !!color)) +
    scale_x_continuous(breaks =
                         seq(0, max(infection_route$i_month, na.rm = T), 6),
                       minor_breaks =
                         seq(0, max(infection_route$i_month, na.rm = T), 3)) +
    ylim(0, max_ylim) +
    scale_fill_manual(values = area_color, drop = F)

  if (!is.null(title)) {
    gp <- gp + labs(title = title)
  }
  if (!is.null(legend_title)) {
    gp <- gp + labs(fill = legend_title, color = legend_title)
  }
  if (!is.null(xlab)) {
    gp <- gp + xlab(xlab)
  }
  if (!is.null(ylab)) {
    gp <- gp + ylab(ylab)
  }
  if (!is.null(border_color)) {
    if (border) {
      gp <- gp + scale_color_manual(values = border_color, drop = F)
    } else {
      warning(
        "Argument border_color is ignored because argument border is FALSE.")
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
#' @param cows A `cow_table` read by [read_final_cows].
#' @inheritParams read_final_cows
#'
#' @seealso [table_infection_status]
#' @name table_route
table_route <- function(output_filename, output_dir, n_simulation,
                        simulation_length, route_levels = NULL) {
  cows <- read_final_cows(output_filename, output_dir, n_simulation,
                          simulation_length, route_levels)
  summary <- summary_route(cows)
  return(summary)
}
# TODO: add examples


#' @name table_route
summary_route <- function(cows) {
  table_route <- cows[, .N, by = .(i_simulation, cause_infection)]
  table_route <- dcast.data.table(table_route, i_simulation ~ cause_infection,
                                  value.var = "N", fill = 0, drop = F)
  table_route[, total := rowSums(.SD),
               .SDcols = setdiff(colnames(table_route), "i_simulation")]
  table_route[, total_inf := total - uninfected]
  table_route[, p_inf := round(total_inf / total * 100, 2)]
  return(table_route)
}


#' Summarize infection status
#'
#' Calculate monthly infection status at the end of simulations.
#'
#' @param cows A `cow_table` read by [read_final_cows].
#' @inheritParams read_final_cows
#'
#' @seealso [table_route]
#' @name table_infection_status
table_infection_status <- function(output_filename, output_dir,
                                   n_simulation, simulation_length) {
    cows <- read_final_cows(output_filename, output_dir, n_simulation,
                            simulation_length)
    summary <- summary_infection_status(cows)
    return(summary)
  }
# TODO: add examples


#' @name table_infection_status
summary_infection_status <- function(cows) {
  cows$infection_status <- factor(cows$infection_status,
                                  levels = c("s", "ial", "ipl", "ebl"))
  table_status <- cows[, .N, by = .(i_simulation, infection_status)]
  table_status <- dcast.data.table(table_status,
                                   i_simulation ~ infection_status,
                                   value.var = "N", fill = 0, drop = F)
  table_status[, total := rowSums(.SD),
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
#' @param to Language to which translate messages. At present, only Japanese is implemented.
translate_msg <- function(type, to) {
  if (is.null(to)) {
    return(list())
  }
  msg <- get(paste0(to, "_", type))
  msg_defined_in_parent <- mget(names(msg), parent.frame(),
                                ifnotfound = list(".notfound"))
  msg[msg_defined_in_parent == ".notfound"] <- NULL
  mapply(function(x, value) assign(x, value, envir = parent.frame(n = 3)),
         names(msg), msg)
  return(msg)
}


#' Define plot title and labels
#'
#' Define plot title and labels based on arguments
#'
#' @param original_msg List of original title and labels before passed to [translate_msg()].
#' @param default_msg List of default plot title and labels.
#' @param language Language to which translate messages.
define_msg <- function(original_msg, default_msg, language) {
  msg <- names(original_msg)
  original_msg_true <- vapply(original_msg, function(x) is.logical(x) && x, T)
  original_msg_false <- vapply(original_msg, function(x) is.logical(x) && !x, T)
  original_msg_chr <- vapply(original_msg, is.character, T)
  env <- parent.frame()
  mapply(function(x, value) assign(x, value, envir = env),
         msg[original_msg_chr], original_msg[original_msg_chr])
  lapply(msg[original_msg_false & msg == "title"],
         function(x) assign(x, NULL, envir = env))
  if (is.null(language)) {
    mapply(function(x, value) assign(x, value, envir = env),
           msg[original_msg_false & msg != "title"],
           default_msg[original_msg_false & msg != "title"])
    mapply(function(x, value) assign(x, value, envir = env),
           msg[original_msg_true], default_msg[original_msg_true])
  }
}

# TODO: Make functions to summary multiple simulation.csv

