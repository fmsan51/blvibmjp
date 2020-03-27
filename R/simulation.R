#' Simulate spread of BLV
#'
#' Simulate spread of BLV in a typical Japanese dairy herd.
#'
#' @param param_simulation See [param_simulation].
#' @param param_farm See [param_farm].
#' @param param_area See [param_area].
#' @param processed_data The result of [process_raw_data()]. Set this parameters or `area_table` and `movement_table`.
#' @param area_table See [area_table].
#' @param movement_table See [movement_table].
#' @param communal_pasture_table See [communal_pasture_table]. Set `NULL` if a farm does not use communal pastures.
#' @param list_param_modification List of lists. Parameter specified in each inner list overwrite default parameters. Each inner list is passed to `param_modification` of  [calc_param()]. Specify like `list(modification_for_iter1 = list(parameter_name = new_value, ...), modification_for_iter2 = list(...), ...)`.
#' @param save_cows,save_param Whether to save `result_combined` and `param_calculated` (a result of [calc_param()]) to a file.
#' @param i_simulation_start An option to rerun a simulation from the middle of simulations. For example, you run 100 simulation, simulation 26 encounter error and stopped, and you want to run simulation 26-100 again while keeping the result from simulation 1-25. Then set i_simulation = 26.
#'
#' @return The function invisibully returns the result of the final run of simulations. csv files storing cow data and txt files storing parameters information are written to a directory specified by `param_simulation$output_dir`.
#' @export
simulate_blv_spread <- function(param_simulation, param_farm, param_area,
                                processed_data,
                                area_table, movement_table,
                                communal_pasture_table = NULL,
                                list_param_modification = NULL,
                                save_cows = T, save_param = T,
                                i_simulation_start = 1) {
  if ((save_param | save_cows) & !(file.exists(param_simulation$output_dir))) {
    dir.create(param_simulation$output_dir, recursive = T)
  }

  cow_table <- NULL
  if (!missing(processed_data)) {
    cow_table <- processed_data$cows
    area_table <- processed_data$areas
    movement_table <- processed_data$movement
    communal_pasture_table <- processed_data$communal_pasture
  }

  # TODO: Varidate params (communal_pasture_table must not be NULL when param_farm$use_communal_pasture is T)
  setup_cows_res <- setup_cows(param_simulation, save_cows, cow_table)
  area_table <- setup_area_table(area_table, param_farm, param_area)

  area_list <- setup_tie_stall_table(area_table)
  # setup_tie_stall_table() must come after setup_area_table()
  # because it uses an attributes which setup_area_table() calculates

  movement_table <- setup_movement_table(area_table, movement_table,
                                         communal_pasture_table)
  cows_areas <- set_init_chamber_and_area_id(setup_cows_res$init_cows,
                                    area_table, area_list)
  day_rp <- setup_rp_table(setup_cows_res$init_n_cows, param_simulation)
  param_processed <- process_param(cows_areas, param_simulation, param_farm)

  result <- result_area <-
    vector("list", param_simulation$simulation_length + 1)
  result[[1]] <- copy(cows_areas$cows)
  # result_aras is made to make debugging easy.
  result_area[[1]] <- cows_areas$area_list

  if (save_param) {
    save_param_txt(
      c(param_simulation, param_farm, param_area, param_processed),
      param_processed$param_output_filename, 0,
      subdir = param_simulation$output_dir)
  }
  # TODO: Make param list to a data.frame

  # TODO: set seed for reproductivity
  max_simulation <- param_simulation$n_simulation + i_simulation_start - 1
  for (i_simulation in (i_simulation_start:max_simulation)) {
    cat("Simulation ", i_simulation, " / ", max_simulation, "\n")
    res <- simulate_once(cows_areas, setup_cows_res$init_n_cows,
             area_table, movement_table,
             day_rp, i_simulation, result, result_area,
             param_simulation, param_farm, param_area, param_processed,
             param_modification = list_param_modification[[i_simulation]],
             save_cows, save_param)
  }

  invisible(res)
}
# TODO: ドキュメント改善


#' Body of the simulation
#'
#' Run simulation once.
#'
#' @note This function does not output the result to a csv file.
#'
#' @param cows_areas A result of [set_init_chamber_and_area_id()].
#' @param last_cow_id `init_last_cow_id` component of a result of [setup_cows()].
#' @param area_table A result of [setup_area_table()].
#' @param movement_table A result of [setup_movement_table()].
#' @param day_rp A result of [setup_rp_table()].
#' @param i_simulation The iteration number of simulations.
#' @param result,result_area Lists to store a `cow_table` and a `tie_stall_table` respectively.
#' @param param_simulation See [param_simulation].
#' @param param_farm See [param_farm].
#' @param param_area See [param_area].
#' @param param_processed A result of [process_param()].
#' @param param_modification See [calc_param()].
#' @param save_cows,save_param Whether to save `result_combined` and `param_calculated` (a result of [calc_param()]) to a file.
#'
#' @return A list composed of two components: `result_combined` and `result_area_combined`
#' @export
simulate_once <- function(cows_areas, last_cow_id, area_table,
                          movement_table, day_rp, i_simulation,
                          result, result_area,
                          param_simulation, param_farm, param_area,
                          param_processed, param_modification,
                          save_cows, save_param) {
  cows <- cows_areas$cows
  areas <- cows_areas$area_list
  param_calculated <- calc_param(param_farm, param_simulation,
                                 param_modification)
  if (save_param) {
    save_param_txt(param_calculated, param_processed$param_output_filename,
                   i_simulation, subdir = param_simulation$output_dir)
  }

  for (i in 1:param_simulation$simulation_length) {
    # Here, 1:n, not seq_len(n), is used due to the speed
    month <- (i + param_simulation$simulation_start - 2) %% 12 + 1
    cows <- set_i_month(cows, i)

    cows <- add_1_to_age(cows)
    cows <- do_ai(cows, i, day_rp, param_calculated)
    cows <- change_stage(cows, i, param_calculated)
    cows <- do_test(cows, month, param_calculated)

    cows <- change_infection_status(cows, i, month, area_table, areas,
                                    param_calculated)
    res <- add_newborns(cows, area_table, i, last_cow_id, param_area,
                        param_calculated, param_processed)
    cows <- res$cows
    last_cow_id <- res$last_cow_id
    res <- tether_roaming_cows(cows, areas)
    cows <- res$cows
    areas <- res$area_list

    # check_removal() must come after add_newborns(), because check_removal()
    # replaces infected old cows with non-replacement newborns
    res <- check_removal(cows, areas, i, area_table,
                         param_farm, param_calculated, param_processed)
    cows <- res$cows
    areas <- res$areas

    # change_area() must be come after check_removal(), because change_area()
    # assigns newborns to areas and removes dead cows from areas.
    res <- change_area(cows, i, movement_table, area_table, areas,
                       param_area, param_calculated)
    cows <- res$cows
    areas <- res$area_list

    result[[i + 1]] <- copy(cows)
    # result_area[[i + 1]] <- copy(areas)
    cows <- extract_owned_cows(cows)
  }

  result_combined <- rbindlist(result)
  result_combined <- result_combined[!is.na(cow_id), ]
  if (save_cows) {
    save_to_csv(result_combined, param_simulation$output_filename,
                i_simulation, subdir = param_simulation$output_dir)
  }
  # result_area, result_area_combined
  # result_area_combined <- lapply(result_area, rbindlist)

  return(result_combined)
}
# TODO: simulationが強制終了したとき用の備え
# TODO: Make functions to load area_table, area_list, ... from csv files
# TODO: ここcowsとareasと何度も分配するくらいなら最初からlist(cows, areas)ベースでやったほうが楽では?

