#' Make a tiestall_table from a cow_table
#'
#' Make a [tiestall_table] based on x, y coordinates in a [cow_table].
#'
#' @param cows See [cow_table].
#' @param n_x The number of chambers in a lane.
#' @param n_y The number of lanes in a tie-stall barn.
#'
#' @return A [tiestall_table].
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

  # TODO: これifelseの結果NAになる場合の対処はできているか？
  # そもそもNAにならないのか？
  group[, ':='(neighbor1_infectivity = ifelse(is_edge1 == F &
                                                neighbor1_isolated == F &
                                                neighbor1_status != "s",
                                              T, F),
               neighbor2_infectivity = ifelse(is_edge2 == F &
                                                neighbor2_isolated == F &
                                                neighbor2_status != "s",
                                              T, F))]

  group[!is.na(cow_id),
        is_exposed := (is_isolated == F &
                         (neighbor1_infectivity == T |
                          neighbor2_infectivity == T)
                       )]

  return(group)
}


#' Check wheter a barn is tie-stall
#'
#' @param param_group See [param_group].
#'
#' @return A logical vector of length 4 which indicates whether barns are tie-stall or not.
is_ts <- function(param_group) {
  # TODO: このfunction必要ないのでは？
  is_ts <- logical(4)
  for (group_id in 1:4) {
    group_xy <- param_group$xy_chamber[[group_id]]
    is_ts[group_id] <- !is.na(group_xy[1])
  }
  return(is_ts)
}


#' Check whether milking cows and dry cows are separated
#'
#' @param param_calculated A list of parameters. See [calc_param()].
#'
#' @return A logical value.
is_md_separated_in_ts <- function(param_calculated) {
  # TODO: groupとbarnについて設定考えるときに変更
  group_m <- param_calculated$groups[3]
  group_d <- param_calculated$groups[4]
  is_ts_milking <- !is.na(param_calculated$xy_chamber[group_m])
  is_md_separated_in_ts <- ((group_m == group_d) &
                              is_ts_milking &
                              param_calculated$is_milking_dry_separated)
  return(is_md_separated_in_ts)
}
# FIXME: currentry used nowhere


#' Remove dead or sold cows from a barn
#'
#' @param group See [tiestall_table].
#' @param cow_id_removed The ID of cows removed from the barn.
#'
#' @return A [tiestall_table].
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


#' Find empty chambers in a tiestall_table
#'
#' Find empty chambers in a tiestall_table and assign cows to chambers.
#'
#' @param group See [tiestall_table].
#' @param added_cows A [cow_table] consisted of cows to add to the barn.
#'
#' @return A [tiestall_table].
find_empty_chamber <- function(group, added_cows) {
  group <- copy(group)

  empty_chamber <- group[is.na(cow_id), chamber_id]
  added_chamber <- sample(empty_chamber, nrow(added_cows))

  group[added_chamber, ':='(cow_id = added_cows$cow_id,
                            cow_status = added_cows$infection_status,
                            is_isolated = added_cows$is_isolated)]

  neighbor2 <- added_chamber[which(added_chamber != nrow(group))] + 1
  neighbor1 <- added_chamber[which(added_chamber != 1)] - 1
  group[neighbor2, ':='(neighbor1_status = group[neighbor2 - 1, cow_status],
                        neighbor1_isolated = group[neighbor2 - 1, is_isolated])]
  group[neighbor1, ':='(neighbor2_status = group[neighbor1 + 1, cow_status],
                        neighbor2_isolated = group[neighbor1 + 1, is_isolated])]

  # TODO: ここ、infectivityがNAになる場合の対処はできているか？　そもそもNAにならないのか？
  group[c(added_chamber, neighbor2, neighbor1),
        ':='(neighbor1_infectivity = ifelse(is_edge1 == F &
                                             neighbor1_isolated == F &
                                             neighbor1_status != "s", T, F),
             neighbor2_infectivity = ifelse(is_edge2 == F &
                                             neighbor2_isolated == F &
                                             neighbor2_status != "s", T, F))]
  group[, is_exposed := (is_isolated == F &
                         (neighbor1_infectivity == T |
                          neighbor2_infectivity == T))]
  group[is.na(cow_id), is_exposed := NA]

  return(group)
}
# TODO: 移動させた後にgroup_idを変えるコードがどこかにあることを確認する


#' Calculate capacity of a barn based on inputed parameters
#'
#' @param herd_size The herd size in a simulated herd.
#' @param param_farm See [param_farm].
#'
#' @return Numeric vector of length 2: `c(lower_limit_of_herd_size, upper_limit_of_herd_size)`.
set_capacity <- function(herd_size, param_farm) {
  # TODO: rename function to explicity
  if (!is.na(param_farm$capacity_in_head[1])) {
    capacity <- param_farm$capacity_in_head
  } else if (!is.na(param_farm$capacity_as_ratio[1])) {
    capacity <- round(c(herd_size * param_farm$capacity_as_ratio[1],
                        herd_size * param_farm$capacity_as_ratio[2]))
  } else {
    capacity <- round(c(herd_size * 0.9, herd_size * 1.1))
  }
  return(capacity)
}

