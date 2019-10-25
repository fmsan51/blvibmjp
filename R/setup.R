#' Setup of `cow_table`
#'
#' Load initial cow status from a csv file, transform it to a [cow_table], and output the `cow_table` to a csv file `month0000.csv`.
#'
#' @param param_simulation See [param_simulation].
#' @param param_area See [param_area].
#' @param save_cows Whether to save initial `cows` to a file.
#'
#' @return A list consisted of `init_cows` ([cow_table]) and `init_last_cow_id` (the number of rows of `cows`) as return of the function and `month0000.csv` in the directionry specified as `param_simulation$output_dir`.
#'
#' @seealso [cow_table] [setup_areas] [setup_rp_table] [setup_area_table]
#' @export
setup_cows <- function(param_simulation, param_area, save_cows) {
  cows <- fread(file = param_simulation$input_csv,
                colClasses = sapply(a_new_calf, class))

  # TODO: ここはbarnとareaについて再考するときに再検討
  cows[stage == "calf", area_id := 1]
  cows[stage == "heifer", area_id := 2]
  cows[stage == "milking", area_id := 3]
  cows[stage == "dry", area_id := 4]
  if (param_area$is_calf_separated) {
    cows[stage == "calf", is_isolated := T]
  }

  # Prepare cow_table with many rows to reserve enough memory while simulation
  init_last_cow_id <- nrow(cows)
  max_herd_size <- init_last_cow_id * param_simulation$simulation_length * 2
  init_cows <- a_new_calf[rep(1, max_herd_size), ]
  init_cows[1:init_last_cow_id, ] <- cows
  # Used 1:n instead of seq_len(n) because it is faster

  if (save_cows) {
    save_to_csv(init_cows, "month", 0, param_simulation$output_dir)
  }

  return(list(init_cows = cows, init_last_cow_id = init_last_cow_id))
}


#' Setup of `rp_table`
#'
#' Make initial `rp_table`.
#'
#' @param init_last_cow_id The element `init_last_cow_id` from the return of [setup_cows()].
#' @param param_simulation See [param_simulation].
#'
#' @seealso [setup_cows] [setup_areas] [rp_table] [setup_area_table]
#' @export
setup_rp_table <- function(init_last_cow_id, param_simulation) {
  # TODO: do_aiをimproveするときに再検討
  # Prepare rp_table with many rows to reserve enough memory while simulation
  one_day_rp[1:init_last_cow_id, ]
  # Used 1:n instead of seq_len(n) because it is faster
}


#' Setup of `tiestall_table`
#'
#' Make chamber matrix, which indicates in which chamber each cow is.
#' Cows kept in free-stall or paddock are not shown in this matrix.
#'
#' @param init_cows The element `init_cows` of a result of [setup_cows()].
#' @param param_area See [param_area].
#'
#' @return A [tiestall_table].
#' @seealso [setup_rp_table] [tiestall_table] [setup_cows] [setup_area_table]
#' @export
setup_areas <- function(init_cows, param_area) {
  # TODO: areasとbarnsの見直しに合わせてここも変更
  init_areas <- vector("list", param_area$n_area)
  for (i in 1:param_area$n_area) {
    xy <- param_area$xy_chamber[[i]]
    if (!anyNA(xy)) {
      init_areas[[i]] <- make_ts_area(init_cows[area_id == i, ],
                                        xy[1], xy[2])
      init_areas[[i]][, area_id := i]
    }
  }
  return(init_areas)
}


#' Setup of `area_table`
#'
#' Setup [area_table].
#'
#' @param area_table See [area_table].
#' @param cows See [cow_table].
#' @param use_communal_pasture logical. Whether a farm uses a communal pasture or not.
#'
#' @seealso [area_table] [setup_cows] [setup_areas] [setup_movement_table] [setup_areas]
#' @export
setup_area_table <- function(area_table, cows, use_communal_pasture) {
  area_table$capacity[is.na(area_table$capacity)] <- Inf
  if (use_communal_pasture) {
    area_table <- rbindlist(list(area_table,
                                 list(area_id = nrow(area_table) + 1L,
                                      area_type = "communal pasture",
                                      capacity = "Inf")
                                 )
                            )
  }

  attr(area_table, "n_cows") <- 
    table(factor(cows$area_id, levels = area_table$area_id))
  attr(area_table, "capacity") <- vapply(area_table$capacity, sum, 1)
  
  return(area_table)
}

#' Setup of `movement_table`
#'
#' Setup `movement_table` from [area_table], [movement_table] and [communal_pasture_table].
#'
#' @param area_table See [area_table].
#' @param movement_table See [movement_table].
#' @param communal_pasture_table See [communal_pasture_table]. Set `NULL` if a farm does not use communal pastures.
#'
#' @seealso [area_table] [movement_table] [communal_pasture_table] [setup_cows] [setup_rp_table] [setup_areas]
#' @export
setup_movement_table <- function(area_table, movement_table,
                                 communal_pasture_table) {
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
  cond <- gsub("(?^|[^_])delivery", "\\1i_month == date_delivered", cond)
  cond <- gsub("(?^|[^_])pregnancy", "\\1i_month == date_got_pregnant", cond)
  cond <- gsub("(?^|[^_])dry", "\\1i_month == date_dried", cond)
  cond <- gsub("dim", "i_month - date_delivered", cond, fixed = T)
  cond <- gsub("stay", "months_in_area", cond, fixed = T)
  movement_table$condition <- cond
 
  # Add movement from/to a communal pasture to movement_table
  if (!is.null(communal_pasture_table)) {
    compas_area_id <- nrow(area_id) + 1L

    # Set movement to a communal pasture with the highest priority
    # (= in the top rows)
    move_to_compas <- data.table(current_area = communal_pasture_table$area_out,
                                 condition = communal_pasture_table$cond_out,
                                 next_area = compas_area_id,
                                 priority = 1)
    move_from_compas <- data.table(current_area = compas_area_id,
                                   condition = communal_pasture_table$cond_back,
                                   next_area = communal_pasture_table$area_back,
                                   priority = 1)
    movement_table <- rbindlist(list(move_to_compas, move_from_compas,
                                     movement_table))
  }

  # TODO: warn when capacity > cows at the start of a simulation.

  # Attributes is added instead of converting area_id column to factor
  # because I don't want to change class of the columns from the original one
  attr(movement_table, "factored_current_area") <-
    factor(movement_table$current_area, levels = area_table$area_id)
  attr(movement_table, "is_priority_specified_by_integer") <-
    vapply(movement_table$priority, is.wholenumber, T)

  return(movement_table)
}


# TODO: make an option to decide do validation of input or not
# TODO: connect to validation functions
# TODO: add a function to randomly set parameter which is not specified by user input

