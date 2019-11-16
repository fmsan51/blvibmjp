#' Read cow_table at the start of a simulation from a csv and modify data ready to plot
#'
#' Read cow_table from a csv file, extract owned cows, set `i_simulation` column to 1, and redefine infection routes.
#'
#' @param path_to_csv Path to a csv file which contains [cow_table].
#' @param levels_route,labels_route See [redefine_levels_route].
#'
#' @return A [cow_table] with an additional column `i_simulation`.
#'
#' @seealso [read_final_cows]
#' @export
read_initial_cows <- function(path_to_csv, levels_route = NULL,
                              labels_route = NULL) {
  cows <- fread(path_to_csv)
  cows <- cows[is_owned == T, ]
  cows <- redefine_levels_route(cows, levels_route, labels_route)
  cows$i_simulation <- 0
  return(cows)
}


#' Read cow information at the end of a simulation
#'
#' Read information of cows which owned by a farm at the end of simulations from csv files and redefine infection routes.
#'
#' @param output_filename,output_dir,n_simulation,simulation_length See [param_simulation].
#' @param levels_route,labels_route See [redefine_levels_route].
#'
#' @return A [cow_table] with an additional column `i_simulation`.
#'
#' @seealso [read_initial_cows]
#' @export
read_final_cows <- function(output_filename, output_dir, n_simulation,
                            simulation_length,
                            levels_route = NULL, labels_route = NULL) {
  all_simulations <- vector("list", n_simulation)
  for (i in seq_len(n_simulation)) {
    cows <- fread(construct_filepath(output_filename, i, output_dir))
    cows$i_simulation <- i
    all_simulations[[i]] <- cows
  }
  all_simulations <- rbindlist(all_simulations)
  final_cows <-
    all_simulations[is_owned == T & i_month == simulation_length, ]
  final_cows <- redefine_levels_route(final_cows, levels_route, labels_route)
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
#' @param simulation_length See [param_simulation].
#' @param path_to_csv Path to a simulation output csv file.
#'
#' @return An scatterplot by [ggplot2::ggplot] object.
#'
#' @export
plot_prevalences <- function(simulation_length, path_to_csv) {
  prevalences <- calculate_prevalences(path_to_csv = path_to_csv)
  ggplot(prevalences, aes(x = i_month, y = prevalence)) +
    geom_point() +
    ylim(0, 1) +
    scale_x_continuous(breaks = seq.int(0, simulation_length, by = 12))
}


#' Redefine infection routes
#'
#' Recategorize `cause_infection` column in a `cow_table`.
#'
#' @param cows See [cow_table].
#' @param levels_route If specified, infection routes not specified in `levels_route` are coarced into "other" category. See `cause_infection` in [cow_table] to know about default categories.
#' @param labels_route Specify if you want to rename categories.
#'
#' @return A [cow_table] with recategorized `cause_infection`.
#'
#' @export
redefine_levels_route <- function(cows, levels_route = NULL,
                                  labels_route = NULL) {
  cows <- copy(cows)

  cows[infection_status == "s", cause_infection := "uninfected"]

  if (is.null(levels_route)) {
    levels_route <- c("uninfected", "initial", "insects", "contact", "needles",
                      "rp", "vertical", "introduced", "comranch")
  }
  uninf_and_route <- unique(c("uninfected", levels_route))

  if (all(unique(cows$cause_infection) %in% uninf_and_route)) {
    cows$cause_infection <- factor(cows$cause_infection, uninf_and_route)
  } else {
    cows$cause_infection <- fct_other(cows$cause_infection, uninf_and_route,
                                      other_level = "other")
    cows$cause_infection <- factor(cows$cause_infection,
                                   levels = c(uninf_and_route, "other"))
  }

  if (!is.null(labels_route)) {
    stopifnot(length(labels_route) == length(levels(cows$cause_infection)))
    levels(cows$cause_infection) <- labels_route
  }

  return(cows)
}


#' Plot monthly infection routes nicely
#'
#' @param path_to_csv Path to an output csv file.
#' @param levels_route,labels_route See [redefine_levels_route]
#' @param max_ylim Upper limit of the y-axis of the plot.
#' @param title,legend_title,xlab,ylab Plot title, legend title, label for x-axis, label for y-axis.
#' @param scale_fill Specify a color palette of a plot.
#' @param border When `TRUE`, each area in a plot will be surrounded by border.
#' @param font Set a font. The default is "Meiryo" for Windows and "Hiragino Kaku Gothic Pro" for the other OS.
#'
#' @return A [ggplot2::ggplot] plot.
#'
#' @export
plot_infection_route <- function(path_to_csv,
                                 levels_route = NULL, labels_route = NULL,
                                 max_ylim = 100, title = NULL,
                                 legend_title = NULL,
                                 xlab = "Months in simulation",
                                 ylab = "Number of cattle", scale_fill = NULL,
                                 border = F, font = NULL) {
  cows <- fread(path_to_csv)
  cows <- cows[is_owned == T, ]
  cows <- redefine_levels_route(cows, levels_route, labels_route)
  infection_route <- cows[, .SD[, .N, by = cause_infection], by = i_month]
  infection_route <-
    complete(infection_route, i_month, cause_infection, fill = list(N = 0))

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
    ylim(0, max_ylim)

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
  if (!is.null(scale_fill)) {
    gp <- gp + scale_fill_manual(values = scale_fill, drop = F)
  }
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
#' @export
table_route <- function(output_filename, output_dir, n_simulation,
                        simulation_length, levels_route = NULL) {
  cows <- read_final_cows(output_filename, output_dir, n_simulation,
                          simulation_length, levels_route)
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
#' @export
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

