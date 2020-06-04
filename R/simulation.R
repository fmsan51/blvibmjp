#' Simulate spread of BLV
#'
#' Simulate spread of BLV in a typical Japanese dairy herd.
#'
#' @param prepared_data The result of [prepare_data()].
#' @param param See [param].
#' @param list_param_modif List of lists. Parameter specified in each inner list overwrite default parameters. Each inner list is passed to `param_modif` of  [calc_param()]. Specify like `list(modification_for_iter1 = list(parameter_name = new_value, ...), modification_for_iter2 = list(...), ...)`.
#' @param save_cows,save_param Wheher to save results of simulations and used parameters to files.
#' @param i_simulation_start An option to rerun a simulation from the middle of simulations. For example, you run 100 simulation, simulation 26 encounter error and stopped, and you want to run simulation 26-100 again while keeping the result from simulation 1-25. Then set i_simulation = 26.
#' @param seed Seed for a simulation.
#' @param validate A logical value indicates whether validate inputs (data and paramters).
#' @param silent A logical value to control print `Simulation x / x` while simulation.
#' @param gc If `TRUE`, a garbage collection is done after every simulation.
#'
#' @return The function invisibully returns the result of the final run of simulations. csv files storing cow data and txt files storing parameters information are written to a directory specified by `param$output_dir`.
#' @export
simulate_blv_spread <- function(prepared_data, param,
                                list_param_modif = NULL,
                                save_cows = T, save_param = T,
                                i_simulation_start = 1, seed = NULL,
                                validate = T, silent = F, gc = T) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if ((save_param | save_cows) & !(file.exists(param$output_dir))) {
    dir.create(param$output_dir, recursive = T)
  }

  if (validate) {
    validate_param(param, list_param_modif)
  }

  cow_table <- prepared_data$cows
  area_table <- prepared_data$areas
  movement_table <- prepared_data$movement

  param_processed <- c(param, process_param(cow_table, param))

  cow_table <- setup_cows(cow_table, param_processed, save_cows)
  area_table <- setup_area_table(area_table, param_processed)
  areas <- setup_tie_stall_table(area_table)
  # setup_tie_stall_table() must come after setup_area_table()
  # because it uses an attributes which setup_area_table() calculates

  movement_table <- setup_movement_table(area_table, movement_table)
  cows_areas <- set_init_chamber_id(cow_table, area_table, areas)
  day_rp <- setup_rp_table(param_processed)
  newborn_table <- setup_newborn_table(param_processed)

  result <- vector("list", param_processed$simulation_length + 1)
  result[[1]] <- copy(cows_areas$cows)

  seeds <- sample.int(.Machine$integer.max, param_processed$n_simulation)
  if (save_param) {
    save_param_txt(c(seed = seed, param_processed),
                   param_processed$param_output_filename, 0,
                   subdir = param_processed$output_dir)
  }

  for (i_simulation in (i_simulation_start:param_processed$n_simulation)) {
    if (!silent) {
      cat("Simulation ", i_simulation, " / ", param_processed$n_simulation,
          "\n")
    }
    set.seed(seeds[i_simulation])
    res <- simulate_once(cows_areas, param_processed$init_max_cow_id,
             area_table, movement_table, day_rp, newborn_table,
             i_simulation, result,
             param_processed, param_modif = list_param_modif[[i_simulation]],
             save_cows, save_param)
    if (gc) {
      gc(verbose = F)
      gc(verbose = F)
    }
  }

  invisible(res)
}


#' Body of the simulation
#'
#' Run simulation once.
#'
#' @note This function does not output the result to a csv file.
#'
#' @param cows_areas A result of [set_init_chamber_id()].
#' @param max_cow_id `init_max_cow_id` component of a result of [process_param()].
#' @param area_table A result of [setup_area_table()].
#' @param movement_table A result of [setup_movement_table()].
#' @param day_rp A result of [setup_rp_table()].
#' @param newborn_table A result of [setup_newborn_table()].
#' @param i_simulation The iteration number of simulations.
#' @param result Lists to store a `cow_table`.
#' @param param_processed A result of [process_param()].
#' @param param_modif See [calc_param()].
#' @param save_cows,save_param Wheher to save a result of a simulation and used parameters to files.
#'
#' @return A `cow_table`
simulate_once <- function(cows_areas, max_cow_id,
                          area_table, movement_table, day_rp, newborn_table,
                          i_simulation, result,
                          param_processed, param_modif, save_cows, save_param) {
  cows <- copy(cows_areas$cows)
  areas <- copy(cows_areas$areas)
  param_calculated <- calc_param(param_processed, param_modif)
  if (save_param) {
    save_param_txt(param_calculated, param_processed$param_output_filename,
                   i_simulation, subdir = param_processed$output_dir)
  }
  param_sim <- c(param_calculated, param_processed)
  param_sim <- param_sim[!duplicated(names(param_sim))]

  for (i in 1:param_sim$simulation_length) {
    # Here, 1:n, not seq_len(n), is used due to the speed
    month <- (i + param_sim$simulation_start - 2) %% 12 + 1
    cows <- set_i_month(cows, i)

    cows <- add_1_to_age(cows)
    res <- do_ai(cows, areas, area_table, i, day_rp, param_sim)
    cows <- res$cows
    areas <- res$areas
    cows <- change_stage(cows, i, param_sim)
    cows <- do_test(cows, month, param_sim)
    res <- add_newborns(cows, area_table, i, max_cow_id, newborn_table,
                        param_sim)
    cows <- res$cows
    max_cow_id <- res$max_cow_id

    # change_infection_status() must come after all functions which calculate
    # new infections, because this sets date_ipl/ebl_expected to new infected
    # cows.
    res <- change_infection_status(cows, i, month, area_table, areas,
                                   param_sim)
    cows <- res$cows
    areas <- res$areas
    res <- tether_roaming_cows(cows, areas)
    cows <- res$cows
    areas <- res$areas

    # check_removal() must come after add_newborns() and
    # change_infection_status(), because check_removal() replaces or removes
    # cows based on infection status.
    res <- check_removal(cows, areas, i, area_table, param_sim)
    cows <- res$cows
    areas <- res$areas

    # change_area() must be come after check_removal(), because change_area()
    # assigns newborns to areas and removes dead cows from areas.
    res <- change_area(cows, i, movement_table, area_table, areas, param_sim)
    cows <- res$cows
    areas <- res$areas

    result[[i + 1]] <- copy(cows)
    cows <- extract_owned_cows(cows)
  }

  result_combined <- rbindlist(result)
  result_combined <- result_combined[!is.na(cow_id), ]
  if (save_cows) {
    save_to_csv(result_combined, param_processed$output_filename,
                i_simulation, subdir = param_processed$output_dir)
  }

  return(result_combined)
}

