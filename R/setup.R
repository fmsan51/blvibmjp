#' Setup of `cow_table`
#'
#' Load initial cow status from a csv file, transform it to a [cow_table], and output the `cow_table` to a csv file `month0000.csv`.
#'
#' @param cow_table See [cow_table].
#' @param param See [param].
#' @param save_cows logical. Whether to save initial `cows` to a file.
#'
#' @return A list consisted of `init_cows` ([cow_table]) and `init_n_cows` (the number of rows of `cows`) as return of the function and `month0000.csv` in the directionry specified as `param$output_dir`.
#'
#' @seealso [cow_table] [setup_areas] [setup_rp_table] [setup_area_table]
setup_cows <- function(cow_table, param, save_cows) {
  # Prepare cow_table with many rows to reserve enough memory while simulation
  init_n_cows <- nrow(cow_table)
  max_herd_size <- init_n_cows * param$simulation_length * 2
  init_cows <- a_new_calf[rep(1, max_herd_size), ]
  init_cows[1:init_n_cows, ] <- cow_table
  # Used 1:n instead of seq_len(n) because it is faster

  init_max_cow_id <- max(init_cows$cow_id, na.rm = T)
  attr(init_cows, "herd_size") <- init_n_cows

  if (save_cows) {
    save_to_csv(init_cows, "month", 0, param$output_dir)
  }

  return(list(init_cows = init_cows, init_n_cows = init_n_cows,
              init_max_cow_id = init_max_cow_id))
}


#' Setup of `rp_table`
#'
#' Make initial `rp_table`.
#'
#' @param init_n_cows The element `init_n_cows` from the return of [setup_cows()].
#' @param param See [param].
#'
#' @seealso [setup_cows] [setup_areas] [rp_table] [setup_area_table]
setup_rp_table <- function(init_n_cows, param) {
  # Prepare rp_table with many rows to reserve enough memory while simulation
  one_day_rp[1:(init_n_cows * 2), ]
  # Used 1:n instead of seq_len(n) because it is faster
}


#' Setup of `newborn_table`
#'
#' Make initial `newborn_table`. `newborn_table` is similar to [cow_table] and with additional columns `"id_mother"`, `"id_calf"`, `"n_newborns_per_cow"`, `"status_mother"` and `"is_freemartin`".
#'
#' @param init_n_cows The element `init_n_cows` from the return of [setup_cows()].
#'
#' @seealso [setup_cows] [setup_areas] [cow_table] [rp_table] [setup_area_table]
setup_newborn_table <- function(init_n_cows) {
  # Prepare newborn_table with many rows to reserve enough memory
  # while simulation
  newborn_table <- a_new_calf[1:(init_n_cows * 2), ]
  # Used 1:n instead of seq_len(n) because it is faster
  newborn_table[, c("id_mother", "id_calf", "n_litter", "status_mother",
                    "is_freemartin") :=
                list(NA_integer_, NA_integer_, NA_real_, NA_character_, NA)]
  return(newborn_table)
}


#' Setup of `tie_stall_table`
#'
#' Make chamber matrix, which indicates in which chamber each cow is.
#' Cows kept in free-stall or paddock are not shown in this matrix.
#'
#' @param area_table See [area_table].
#'
#' @return A list composed of [tie_stall_table] and NA.
#' @seealso [setup_rp_table] [tie_stall_table] [setup_cows] [setup_area_table]
#' @name areas
setup_tie_stall_table <- function(area_table) {
  areas <- vector("list", nrow(area_table))
  names(areas) <- area_table$area_id
  for (i_area in attr(area_table, "tie_stall")) {
    # [[1]] is faster than using which()
    area_capacity <- area_table$capacity[i_area == area_table$area_id][[1]]
    n_chambers <- sum(area_capacity)
    a_tie_stall <- a_chamber[rep(1, n_chambers), ]
    a_tie_stall[, `:=`(chamber_id = 1:n_chambers,
                       adjoint_previous_chamber = T,
                       adjoint_next_chamber = T)]
    # 1:n is used because it is much faster than seq_len(n).
    lane_edges <- cumsum(area_capacity)
    a_tie_stall[lane_edges, `:=`(adjoint_next_chamber = F)]
    a_tie_stall[c(1, lane_edges[-length(lane_edges)] + 1),
                `:=`(adjoint_previous_chamber = F)]

    areas[[as.character(i_area)]] <- a_tie_stall
  }
  return(areas)
}


#' Initial assignment of `chamber_id`
#'
#' Initial assignment of `chamber_id`
#'
#' @param init_cows `init_cows` component of a result of [setup_cows()].
#' @param area_table A result of [setup_area_table()].
#' @param areas A result of [setup_tie_stall_table()].
#'
#' @return A list consisted of `cows` and `areas`.
set_init_chamber_id <- function(init_cows, area_table, areas) {
  area_assignment <- calculate_area_assignment(init_cows, area_table,
                       init_cows[!is.na(cow_id) & is.na(chamber_id), cow_id])
  res <- assign_chambers(init_cows, areas, area_assignment)
  return(res)
}


#' Setup of `area_table`
#'
#' Setup [area_table].
#'
#' @param area_table See [area_table].
#' @param param See [param].
#'
#' @seealso [area_table] [setup_cows] [setup_areas] [setup_movement_table] [setup_areas]
setup_area_table <- function(area_table, param) {
  area_table$capacity[is.na(area_table$capacity)] <- Inf

  attr(area_table, "capacity") <-
    setNames(vapply(area_table$capacity, sum, 1), area_table$area_id)
  attr(area_table, "tie_stall") <-
    area_table$area_id[area_table$area_type == "tie"]
  attr(area_table, "tie_stall_chr") <-
    as.character(attr(area_table, "tie_stall"))
  attr(area_table, "is_calf_isolated") <-
    area_table[area_id == 1, area_type == "hatch"]

  return(area_table)
}


#' Setup of `movement_table`
#'
#' Setup `movement_table` from [area_table] and [movement_table].
#'
#' @param area_table See [area_table].
#' @param movement_table See [movement_table].
#'
#' @seealso [area_table] [movement_table] [setup_cows] [setup_rp_table] [setup_areas]
setup_movement_table <- function(area_table, movement_table) {
  # Sort next_area along with priority
  movement_table$next_area <-
    mapply(function(next_area, priority) {next_area[order(priority)]},
           movement_table$next_area, movement_table$priority, SIMPLIFY = FALSE)

  # translate condition from a form that users can easily understand
  # to a form that functions can easily understand
  cond <- movement_table$condition
  convert_day_to_month <- function(day) {
    day <- as.numeric(day)
    month <- round(day / (365 / 12), 3)  # rounded for readability
    return(month)
  }
  cond <- str_replace_all(cond, "(?<=dim[^|&]{1,20}?)\\d+",
                          convert_day_to_month)
  # max range is 20 because it seems enough
  cond <- str_replace_all(cond, "(?<!parity[^|&]{1,20}?)(\\d*\\.\\d+)",
                          "integerize(\\1)")

  # fixed = T because it's about 2-3x faster
  cond <- gsub("months_from_delivery", "i_month - date_last_delivery",
               cond, fixed = T)
  cond <- gsub("months_from_pregnancy", "i_month - date_got_pregnant",
               cond, fixed = T)
  cond <- gsub("months_from_dry", "i_month - date_dried", cond, fixed = T)

  # (?^|[^_]) is about 3x faster than (?<!_)
  cond <- gsub("(?:^|[^_])delivery", "\\1i_month == date_last_delivery", cond)
  cond <- gsub("(?:^|[^_])pregnancy", "\\1i_month == date_got_pregnant", cond)
  cond <- gsub("(?:^|[^_])dry", "\\1i_month == date_dried", cond)
  cond <- gsub("dim", "i_month - date_last_delivery", cond, fixed = T)
  cond <- gsub("stay", "months_in_area", cond, fixed = T)

  cond <- paste0(cond, "& area_id == ", movement_table$current_area)
  movement_table$condition <- cond

  movement_table$priority <- lapply(movement_table$priority,
                                    function(x) ifelse(is.na(x), 1, x))

  # Attributes is added instead of converting area_id column to factor
  # because I don't want to change class of the columns from the original one
  attr(movement_table, "factored_current_area") <-
    factor(movement_table$current_area, levels = area_table$area_id)
  attr(movement_table, "is_priority_specified_by_integer") <-
    vapply(movement_table$priority, is.wholenumber, T)
  attr(movement_table, "cond_as_expr") <- parse(text = cond)

  return(movement_table)
}


# TODO: make an option to decide do validation of input or not
# TODO: connect to validation functions

