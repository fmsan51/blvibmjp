# tie_stall_settings ----

## ---- make_ts_group
#' Make a barn_table from a cow_table
#'
#' Make a [barn_table] based on x, y coordinates in a [cow_table].
#'
#' @param cows See [cow_table].
#' @param n_x The number of chambers in a lane.
#' @param n_y The number of lanes in a tie-stall barn.
#'
#' @return A [barn_table]
#' @export
make_ts_group <- function(cows, n_x, n_y) {
  # TODO: これcsvから読み込むような形にしたい
  group <- a_chamber[rep(1, n_x * n_y), ]
  group[, chamber_id := seq_len(.N)]
  group[, is_edge1 := F]
  group[, is_edge2 := F]
  group[seq(1, n_x * n_y, by = n_x), is_edge1 := T]
  group[seq(n_x, n_x * n_y, by = n_x), is_edge2 := T]

  group[cows$chamber_id, ':='(cow_id = cows$cow_id,
                              cow_status = cows$infection_status,
                              is_isolated = cows$is_isolated)]
  group[, ':='(neighbor1_status = shift(cow_status, type = "lag"),
               neighbor1_isolated = shift(is_isolated, type = "lag"),
               neighbor2_status = shift(cow_status, type = "lead"),
               neighbor2_isolated = shift(is_isolated, type = "lead"))]

  group[, ':='(neighbor1_infectivity = if_else(is_edge1 == F &
                                                neighbor1_isolated == F &
                                                neighbor1_status != "s",
                                               T, F, F),
               neighbor2_infectivity = if_else(is_edge2 == F &
                                                neighbor2_isolated == F &
                                                neighbor2_status != "s",
                                               T, F, F))]

  group[!is.na(cow_id),
        is_exposed := (is_isolated == F &
                         (neighbor1_infectivity == T |
                          neighbor2_infectivity == T)
                       )]

  return(group)
}


## ---- is_ts
#' Check wheter a barn is tie-stall
#'
#' @param params A list of parameter. See [calc_params()].
#'
#' @return A logical vector of length 4 which indicates whether barns are tie-stall or not.
is_ts <- function(params) {
  # TODO: このfunction必要ないのでは？
  is_ts <- logical(4)
  for (stage in 1:4) {
    group_id <- params$groups[stage]
    group_xy <- params$xy_chambers[[group_id]]
    is_ts[stage] <- !is.na(group_xy[1])
  }
  return(is_ts)
}



## ---- is_md_separated_in_ts
#' Check whether milking cows and dry cows are separated
#'
#' @param params A list of parameters. See [calc_params()].
#'
#' @return A logical value
is_md_separated_in_ts <- function(params) {
  group_m <- params$groups[3]
  group_d <- params$groups[4]
  is_ts_milking <- !is.na(params$xy_chambers[group_m])
  is_md_separated_in_ts <- ((group_m == group_d) &
                              is_ts_milking &
                              params$is_milking_dry_separated)
  return(is_md_separated_in_ts)
}



## group_settings ----

## ---- remove_from_group
#' Remove dead or sold cows from a barn
#'
#' @param group See [barn_table].
#' @param cow_id_removed The ID of cows removed from the barn.
#'
#' @return A [barn_table].
remove_from_group <- function(group, cow_id_removed) {
  # Remove chambers for isolation
  removed_chamber <- group[cow_id %in% cow_id_removed, chamber_id]

  contact1 <- removed_chamber + 1
  contact2 <- removed_chamber - 1
  group[contact1[contact1 != (.N + 1)], ':='(neighbor1_status = NA,
                                             neighbor1_isolated = NA,
                                             neighbor1_infectivity = F)]
  group[contact2[contact2 != 0], ':='(neighbor2_status = NA,
                                      neighbor2_isolated = NA,
                                      neighbor2_infectivity = F)]

  group[removed_chamber, ':='(cow_id = NA,
                              cow_status = NA,
                              is_isolated = NA,
                              hazard_ratio = NA,
                              is_exposed = NA)]

  return(group)
}



## ---- find_empty_chamber
#' Find empty chambers in a barn_table
#'
#' Find empty chambers in a barn_table and assign cows to chambers.
#'
#' @param group See [barn_table].
#' @param added_cows A [cow_table] consisted of cows to add to the barn.
#'
#' @return A [barn_table]
find_empty_chamber <- function(group, added_cows) {
  group <- copy(group)

  empty_chamber <- group[is.na(cow_id), chamber_id]
  added_chamber <- sample(empty_chamber, nrow(added_cows))

  # Assign added_chamber manually when is_test = T
  if (is_test && !is.null(test_params$find_empty_chamber)) {
    added_chamber <- test_params$find_empty_chamber
  }

  group[added_chamber, ':='(cow_id = added_cows$cow_id,
                            cow_status = added_cows$infection_status,
                            is_isolated = added_cows$is_isolated)]

  neighbor2 <- added_chamber[which(added_chamber != nrow(group))] + 1
  neighbor1 <- added_chamber[which(added_chamber != 1)] - 1
  group[neighbor2, ':='(neighbor1_status = group[neighbor2 - 1, cow_status],
                        neighbor1_isolated = group[neighbor2 - 1, is_isolated])]
  group[neighbor1, ':='(neighbor2_status = group[neighbor1 + 1, cow_status],
                        neighbor2_isolated = group[neighbor1 + 1, is_isolated])]

  group[c(added_chamber, neighbor2, neighbor1),
        ':='(neighbor1_infectivity = if_else(is_edge1 == F &
                                             neighbor1_isolated == F &
                                             neighbor1_status != "s", T, F, F),
             neighbor2_infectivity = if_else(is_edge2 == F &
                                             neighbor2_isolated == F &
                                             neighbor2_status != "s", T, F, F))]
  group[, is_exposed := (is_isolated == F &
                         (neighbor1_infectivity == T |
                          neighbor2_infectivity == T))]
  group[is.na(cow_id), is_exposed := NA]

  return(group)
}
# TODO: 移動させた後にgroup_idを変えるコードがどこかにあることを確認する

## ---- set_capacity
#' Calculate capacity of a barn based on inputed parameters
#'
#' @param herd_size The herd size in a simulated herd.
#'
#' @return numeric
set_capacity <- function(herd_size) {
  if (!is.na(PARAMS_FARM$capacity_in_head[1])) {
    capacity <- PARAMS_FARM$capacity_in_head
  } else if (!is.na(PARAMS_FARM$capacity_as_ratio[1])) {
    capacity <- round(c(herd_size * PARAMS_FARM$capacity_as_ratio[1],
                        herd_size * PARAMS_FARM$capacity_as_ratio[2]))
  } else {
    capacity <- round(c(herd_size * 0.9, herd_size * 1.1))
  }
  return(capacity)
}



