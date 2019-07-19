#' Setup of `cow_table`
#'
#' Load initial cow status from a csv file, transform it to a [cow_table], and output the `cow_table` to a csv file `month0000.csv`.
#'
#' @param param_simulation See [param_simulation].
#' @param param_group See [param_group].
#' @param save_cows Whether to save initial `cows` to a file.
#'
#' @return A list consisted of `init_cows` ([cow_table]) and `init_last_cow_id` (the number of rows of `cows`) as return of the function and `month0000.csv` in the directionry specified as `param_simulation$output_dir`.
#'
#' @seealso [cow_table] [setup_groups] [setup_rp_table]
#' @export
setup_cows <- function(param_simulation, param_group, save_cows) {
  cows <- fread(file = param_simulation$input_csv,
                colClasses = sapply(a_new_calf, class))

  # TODO: ここはbarnとgroupについて再考するときに再検討
  cows[stage == "calf", group_id := 1]
  cows[stage == "heifer", group_id := 2]
  cows[stage == "milking", group_id := 3]
  cows[stage == "dry", group_id := 4]
  if (param_group$is_calf_separated) {
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
#' @seealso [setup_cows] [setup_groups] [rp_table]
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
#' @param param_group See [param_group].
#'
#' @return A [tiestall_table].
#' @seealso [setup_rp_table] [tiestall_table] [setup_cows]
#' @export
setup_groups <- function(init_cows, param_group) {
  # TODO: groupsとbarnsの見直しに合わせてここも変更
  init_groups <- vector("list", param_group$n_group)
  for (i in 1:param_group$n_group) {
    xy <- param_group$xy_chamber[[i]]
    if (!anyNA(xy)) {
      init_groups[[i]] <- make_ts_group(init_cows[group_id == i, ],
                                        xy[1], xy[2])
      init_groups[[i]][, group_id := i]
    }
  }
  return(init_groups)
}


# TODO: make an option to decide do validation of input or not
# TODO: connect to validation functions
# TODO: add a function to randomly set parameter which is not specified by user input

