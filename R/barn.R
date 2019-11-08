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
make_ts_area <- function(cows, n_x, n_y) {
  # TODO: これcsvから読み込むような形にしたい
  area <- a_chamber[rep(1, n_x * n_y), ]
  area[, chamber_id := seq_len(.N)]
  area[, is_edge1 := F]
  area[, is_edge2 := F]
  area[seq(1, n_x * n_y, by = n_x), is_edge1 := T]
  area[seq(n_x, n_x * n_y, by = n_x), is_edge2 := T]

  area[cows$chamber_id, ':='(cow_id = cows$cow_id,
                              cow_status = cows$infection_status,
                              is_isolated = cows$is_isolated)]
  area[, ':='(neighbor1_status = shift(cow_status, type = "lag"),
               neighbor1_isolated = shift(is_isolated, type = "lag"),
               neighbor2_status = shift(cow_status, type = "lead"),
               neighbor2_isolated = shift(is_isolated, type = "lead"))]

  # TODO: これifelseの結果NAになる場合の対処はできているか？
  # そもそもNAにならないのか？
  area[, ':='(neighbor1_infectivity = ifelse(is_edge1 == F &
                                                neighbor1_isolated == F &
                                                neighbor1_status != "s",
                                              T, F),
               neighbor2_infectivity = ifelse(is_edge2 == F &
                                                neighbor2_isolated == F &
                                                neighbor2_status != "s",
                                              T, F))]

  area[!is.na(cow_id),
        is_exposed := (is_isolated == F &
                         (neighbor1_infectivity == T |
                          neighbor2_infectivity == T)
                       )]

  return(area)
}


#' Check whether a barn is tie-stall
#'
#' @param param_area See [param_area].
#'
#' @return A logical vector of length 4 which indicates whether barns are tie-stall or not.
is_ts <- function(param_area) {
  # TODO: このfunction必要ないのでは？
  is_ts <- logical(4)
  for (area_id in 1:4) {
    area_xy <- param_area$xy_chamber[[area_id]]
    is_ts[area_id] <- !is.na(area_xy[1])
  }
  return(is_ts)
}


#' Check whether milking cows and dry cows are separated
#'
#' @param param_calculated A list of parameters. See [calc_param()].
#'
#' @return A logical value.
is_md_separated_in_ts <- function(param_calculated) {
  # TODO: areaについて設定考えるときに変更
  area_m <- param_calculated$areas[3]
  area_d <- param_calculated$areas[4]
  is_ts_milking <- !is.na(param_calculated$xy_chamber[area_m])
  is_md_separated_in_ts <- ((area_m == area_d) &
                              is_ts_milking &
                              param_calculated$is_milking_dry_separated)
  return(is_md_separated_in_ts)
}
# FIXME: currentry used nowhere


#' Remove dead or sold cows from a area
#'
#' @param area See [tiestall_table].
#' @param cow_id_removed The ID of cows removed from the area.
#'
#' @return A [tiestall_table].
remove_from_area <- function(area, cow_id_removed) {
  # Remove chambers for isolation
  removed_chamber <- area[cow_id %in% cow_id_removed, chamber_id]

  contact1 <- removed_chamber + 1
  contact2 <- removed_chamber - 1
  area[contact1[contact1 != (.N + 1)], ':='(neighbor1_status = NA,
                                             neighbor1_isolated = NA,
                                             neighbor1_infectivity = F)]
  area[contact2[contact2 != 0], ':='(neighbor2_status = NA,
                                      neighbor2_isolated = NA,
                                      neighbor2_infectivity = F)]

  area[removed_chamber, ':='(cow_id = NA,
                              cow_status = NA,
                              is_isolated = NA,
                              hazard_ratio = NA,
                              is_exposed = NA)]

  return(area)
}


#' Find empty chambers in a tiestall_table
#'
#' Find empty chambers in a tiestall_table and assign cows to chambers.
#'
#' @param area See [tiestall_table].
#' @param added_cows A [cow_table] consisted of cows to add to the area.
#'
#' @return A [tiestall_table].
find_empty_chamber <- function(area, added_cows) {
  area <- copy(area)

  empty_chamber <- area[is.na(cow_id), chamber_id]
  added_chamber <- sample(empty_chamber, nrow(added_cows))

  area[added_chamber, ':='(cow_id = added_cows$cow_id,
                            cow_status = added_cows$infection_status,
                            is_isolated = added_cows$is_isolated)]

  neighbor2 <- added_chamber[which(added_chamber != nrow(area))] + 1
  neighbor1 <- added_chamber[which(added_chamber != 1)] - 1
  area[neighbor2, ':='(neighbor1_status = area[neighbor2 - 1, cow_status],
                        neighbor1_isolated = area[neighbor2 - 1, is_isolated])]
  area[neighbor1, ':='(neighbor2_status = area[neighbor1 + 1, cow_status],
                        neighbor2_isolated = area[neighbor1 + 1, is_isolated])]

  # TODO: ここ、infectivityがNAになる場合の対処はできているか？　そもそもNAにならないのか？
  area[c(added_chamber, neighbor2, neighbor1),
        ':='(neighbor1_infectivity = ifelse(is_edge1 == F &
                                             neighbor1_isolated == F &
                                             neighbor1_status != "s", T, F),
             neighbor2_infectivity = ifelse(is_edge2 == F &
                                             neighbor2_isolated == F &
                                             neighbor2_status != "s", T, F))]
  area[, is_exposed := (is_isolated == F &
                         (neighbor1_infectivity == T |
                          neighbor2_infectivity == T))]
  area[is.na(cow_id), is_exposed := NA]

  return(area)
}
# TODO: 移動させた後にarea_idを変えるコードがどこかにあることを確認する


#' Remove cows from areas
#'
#' Assign `NA`s to `area_id` and `chamber_id` of specified cows.
#'
#' @param cows See [cow_table].
#' @param removed_cow_id `cow_id` of cows to be removed from current areas.
#'
#' @return A [cow_table] in which `area_id` and `chamber_id` of specified cows are set as `NA`.
remove_from_areas <- function(cows, removed_cow_id) {
  cows[cow_id %in% removed_cow_id, `:=`(area_id = NA_integer_,
                                        chamber_id = NA_integer_)]
  return(cows)
}
#' Calculate capacity of an area based on inputed parameters
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

