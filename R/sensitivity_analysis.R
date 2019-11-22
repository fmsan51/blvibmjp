#' Do sensitivity analysis
#'
#' Load parameter list from a csv files, run simulation repeatedly with parameter list, and save results.
#'
#' @inheritParams simulate_blv_spread
#'
#' @export
sensitivity_analysis <- function(param_simulation, param_farm, param_group,
                                 i_simulation_start = 1) {
  if (!(file.exists(param_simulation$output_dir))) {
    dir.create(param_simulation$output_dir, recursive = T)
  }

  setup_cows_res <- setup_cows(param_simulation, param_group, save_cows = F)
  init_groups <- setup_groups(setup_cows_res$init_cows, param_group)
  day_rp <- setup_rp_table(setup_cows_res$init_last_cow_id, param_simulation)
  param_processed <- process_param(setup_cows_res, param_simulation, param_farm,
                                   param_group)

  result <- vector("list", param_simulation$simulation_length + 1)
  result[[1]] <- copy(setup_cows_res$init_cows)
  result_groups <- vector("list", param_simulation$simulation_length + 1)

  save_param_txt(
    c(param_simulation, param_farm, param_group, param_processed),
    param_processed$param_output_filename, 0,
    subdir = param_simulation$output_dir)

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
}
# TODO: WIP
# TODO: improve document

