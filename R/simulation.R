#' Simulate spread of BLV
#'
#' Simulate spread of BLV in a typical Japanese dairy herd.
#'
#' @param param_simulation See [param_simulation].
#' @param param_farm See [param_farm].
#' @param param_group See [param_group].
#' @param list_param_modification List of lists. Parameter specified in each inner list overwrite default parameters. Each inner list is passed to `param_modification` of  [calc_param()]. Specify like `list(modification_for_iter1 = list(parameter_name = new_value, ...), modification_for_iter2 = list(...), ...)`.
#' @param save_cows,save_param Whether to save `result_combined` and `param_calculated` (a result of [calc_param()]) to a file.
#' @param i_simulation_start An option to rerun a simulation from the middle of simulations. For example, you run 100 simulation, simulation 26 encounter error and stopped, and you want to run simulation 26-100 again while keeping the result from simulation 1-25. Then set i_simulation = 26.
#'
#' @return The function invisibully returns the result of the final run of simulations. csv files storing cow data and txt files storing parameters information are written to a directory specified by `param_simulation$output_dir`.
simulate_blv_spread <- function(param_simulation, param_farm, param_group,
                                list_param_modification = NULL,
                                save_cows = T, save_param = T,
                                i_simulation_start = 1) {
  if (save_param & !(file.exists(param_simulation$output_dir))) {
    dir.create(param_simulation$output_dir)
  }

  setup_cows_res <- setup_cows(param_simulation, param_group, save_cows)
  init_groups <- setup_groups(setup_cows_res$init_cows, param_group)
  day_rp <- setup_rp_table(setup_cows_res$init_last_cow_id, param_simulation)
  param_processed <- process_param(setup_cows_res, param_simulation, param_farm,
                                   param_group)

  result <- vector("list", param_simulation$simulation_length + 1)
  result[[1]] <- copy(setup_cows_res$init_cows)
  # result_groups is used nowhere, but it is leaved in the code for debugging
  result_groups <- vector("list", param_simulation$simulation_length + 1)
  # result_groups[[1]] <- copy(init_groups)

  if (save_param) {
    save_param_txt(
      c(param_simulation, param_farm, param_group, param_processed),
      param_processed$param_output_filename, 0,
      subdir = param_simulation$output_dir)
  }

  # TODO: set seed for reproductivity
  max_simulation <- param_simulation$n_simulation + i_simulation_start - 1
  for (i_simulation in (i_simulation_start:max_simulation)) {
    cat("Simulation ", i_simulation, " / ", max_simulation, "\n")
    res <- simulate_once(setup_cows_res, init_groups, day_rp, i_simulation,
             result, result_groups,
             param_simulation, param_group, param_processed,
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
#' @param setup_cows_res A result of [setup_cows()].
#' @param init_groups A result of [setup_groups()].
#' @param day_rp A result of [setup_rp_table()].
#' @param i_simulation The iteration number of simulations.
#' @param result,result_groups Lists to store a `cow_table` and a `tiestall_table` respectively.
#' @param param_simulation See [param_simulation].
#' @param param_group See [param_group].
#' @param param_processed A result of [process_param()].
#' @param param_modification See [calc_param()].
#' @param save_cows,save_param Whether to save `result_combined` and `param_calculated` (a result of [calc_param()]) to a file.
#'
#' @return A list composed of two components: `result_combined` and `result_groups_combined`
#' @export
simulate_once <- function(setup_cows_res, init_groups, day_rp,
                          i_simulation, result, result_groups,
                          param_simulation, param_group, param_processed,
                          param_modification, save_cows, save_param) {
  cows <- copy(setup_cows_res$init_cows)
  groups <- copy(init_groups)
  last_cow_id <- setup_cows_res$init_last_cow_id
  param_calculated <- calc_param(param_farm, param_modification)
  if (save_param) {
    save_param_txt(param_calculated, param_processed$param_output_filename,
                   i_simulation, subdir = param_simulation$output_dir)
  }

  for (i in 1:param_simulation$simulation_length) {
    # Here, 1:n, not seq_len(n), is used due to the speed
    month <- (i + param_simulation$simulation_start - 2) %% 12 + 1
    cows <- add_1_to_age(cows)
    # TODO: change_stage と move_group を分けたい
    cows <- do_ai(cows, i, day_rp, param_calculated)

    res <- change_stage(cows, groups, i, param_group, param_calculated,
                        param_processed)
    cows <- res$cows
    groups <- res$groups

    cows <- change_infection_status(cows, i, month, param_calculated)
    res <- add_newborns(cows, i, last_cow_id, param_calculated, param_processed)
    cows <- res$cows
    last_cow_id <- res$last_cow_id

    res <- check_removal(cows, groups, i, param_calculated, param_processed)
    cows <- res$cows
    groups <- res$groups

    cows <- set_i_month(cows, i)
    result[[i + 1]] <- copy(cows)
    # result_groups[[i + 1]] <- copy(groups)
    cows <- extract_owned_cows(cows)
  }

  result_combined <- rbindlist(result)
  result_combined <- result_combined[!is.na(cow_id), ]
  if (save_cows) {
    save_to_csv(result_combined, param_simulation$output_filename,
                i_simulation, subdir = param_simulation$output_dir)
  }
  # result_groups, result_groups_combined
  # result_groups_combined <- lapply(result_groups, rbindlist)

  return(result_combined)
}
# TODO: simulationが強制終了したとき用の備え

