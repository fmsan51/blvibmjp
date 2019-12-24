#' Setup of `cow_table`
#'
#' Load initial cow status from a csv file, transform it to a [cow_table], and output the `cow_table` to a csv file `month0000.csv`.
#'
#' @param param_simulation See [param_simulation].
#' @param param_area See [param_area].
#' @param save_cows logical. Whether to save initial `cows` to a file.
#' @param area_table See [area_table].
#'
#' @return A list consisted of `init_cows` ([cow_table]) and `init_n_cows` (the number of rows of `cows`) as return of the function and `month0000.csv` in the directionry specified as `param_simulation$output_dir`.
#'
#' @seealso [cow_table] [setup_areas] [setup_rp_table] [setup_area_table]
#' @export
setup_cows <- function(param_simulation, save_cows) {
  cows <- fread(file = param_simulation$input_csv,
                colClasses = sapply(a_new_calf, class))

  # Prepare cow_table with many rows to reserve enough memory while simulation
  init_n_cows <- nrow(cows)
  max_herd_size <- init_n_cows * param_simulation$simulation_length * 2
  init_cows <- a_new_calf[rep(1, max_herd_size), ]
  init_cows[1:init_n_cows, ] <- cows
  # Used 1:n instead of seq_len(n) because it is faster
  
  if (save_cows) {
    save_to_csv(init_cows, "month", 0, param_simulation$output_dir)
  }

  return(list(init_cows = init_cows, init_n_cows = init_n_cows))
}


#' Setup of `rp_table`
#'
#' Make initial `rp_table`.
#'
#' @param init_n_cows The element `init_n_cows` from the return of [setup_cows()].
#' @param param_simulation See [param_simulation].
#'
#' @seealso [setup_cows] [setup_areas] [rp_table] [setup_area_table]
#' @export
setup_rp_table <- function(init_n_cows, param_simulation) {
  # TODO: do_aiをimproveするときに再検討
  # Prepare rp_table with many rows to reserve enough memory while simulation
  one_day_rp[1:init_n_cows, ]
  # Used 1:n instead of seq_len(n) because it is faster
}


#' Setup of `tie_stall_table`
#'
#' Make chamber matrix, which indicates in which chamber each cow is.
#' Cows kept in free-stall or paddock are not shown in this matrix.
#'
#' @param init_cows The element `init_cows` of a result of [setup_cows()].
#' @param area_table See [area_table].
#'
#' @return A list composed of [tie_stall_table] and NA.
#' @seealso [setup_rp_table] [tie_stall_table] [setup_cows] [setup_area_table]
#' @name area_list
#' @export
setup_tie_stall_table <- function(init_cows, area_table) {
  area_list <- vector("list", nrow(area_table))
  names(area_list) <- area_table$area_id
  for (i_area in attr(area_table, "tie_stall")) {
    area_capacity <- area_table$capacity[i_area == area_table$area_id]
    a_tie_stall <- a_chamber[rep(1, sum(area_capacity)), ]
    a_tie_stall[, `:=`(chamber_id = seq_along(area_capacity),
                       adjoint_previous_chamber = T,
                       adjoint_next_chamber = T)]
    a_tie_stall[area_capacity, adjoint_previous_chamber = F]

    area_list[[area_table]] <- a_tie_stall
  }

  area_assignment <- calculate_area_assignment(init_cows, area_table, NULL)
  area_list <- assign_cows(init_cows, area_list, area_assignment)
  
  # NOTE: currently, initial area_id must be set by users.
  # Want to make a function to calculate initial area_id.
  return(area_list)
}
# NOTE: Is this function really necessary?


#' Setup of `area_table`
#'
#' Setup [area_table].
#'
#' @param area_table See [area_table].
#' @param cows See [cow_table].
#' @param param_farm See [param_farm].
#'
#' @seealso [area_table] [setup_cows] [setup_areas] [setup_movement_table] [setup_areas]
#' @export
setup_area_table <- function(area_table, cows, param_farm, param_area) {
  area_table$capacity[is.na(area_table$capacity)] <- Inf
  if (param_farm$use_communal_pasture & all(area_type != "communal pasture")) {
    area_table <- rbindlist(list(area_table,
                                 list(area_id = max(area_table$area_id) + 1L,
                                      area_type = "communal pasture",
                                      capacity = list(Inf))
                                 )
                            )
  }

  attr(area_table, "capacity") <- 
    setNames(vapply(area_table$capacity, sum, 1), area_table$area_id)
  attr(area_table, "tie_stall") <- 
    area_table$area_id[area_table$area_type == "tie"]
  attr(area_table, "is_calf_isolated") <- 
    area_table[area_id == param_area$calf_area_id, area_type == "hatch"]

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
  cond <- gsub("(?:^|[^_])delivery", "\\1i_month == date_last_delivery", cond)
  cond <- gsub("(?:^|[^_])pregnancy", "\\1i_month == date_got_pregnant", cond)
  cond <- gsub("(?:^|[^_])dry", "\\1i_month == date_dried", cond)
  cond <- gsub("dim", "i_month - date_last_delivery", cond, fixed = T)
  cond <- gsub("stay", "months_in_area", cond, fixed = T)
  movement_table$condition <- cond
 
  # Add movement from/to a communal pasture to movement_table
  if (!is.null(communal_pasture_table)) {
    compas_area_id <- 
      area_table$area_id[area_table$area_type == "communal pasture"]

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

  movement_table$priority <- lapply(movement_table$priority,
                                    function(x) ifelse(is.na(x), 1, x))

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

