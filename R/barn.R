#' Make a tie_stall_table from a cow_table
#'
#' Make a [tie_stall_table] based on x, y coordinates in a [cow_table].
#'
#' @param cows See [cow_table].
#' @param n_x The number of chambers in a lane.
#' @param n_y The number of lanes in a tie-stall barn.
#'
#' @return A [tie_stall_table].
#' @export
depreciate_make_ts_area <- function(cows, n_x, n_y) {
  # TODO: これcsvから読み込むような形にしたい
  area <- a_chamber[rep(1, n_x * n_y), ]
  area[, chamber_id := seq_len(.N)]
  area[, adjoint_previous_chamber := F]
  area[, adjoint_next_chamber := F]
  area[seq(1, n_x * n_y, by = n_x), adjoint_previous_chamber := T]
  area[seq(n_x, n_x * n_y, by = n_x), adjoint_next_chamber := T]

  area[cows$chamber_id, ':='(cow_id = cows$cow_id,
                              cow_status = cows$infection_status,
                              is_isolated = cows$is_isolated)]
  area[, ':='(previous_neighbor_status = shift(cow_status, type = "lag"),
               previous_neighbor_isolated = shift(is_isolated, type = "lag"),
               next_neighbor_status = shift(cow_status, type = "lead"),
               next_neighbor_isolated = shift(is_isolated, type = "lead"))]

  # TODO: これifelseの結果NAになる場合の対処はできているか？
  # そもそもNAにならないのか？
  area[, ':='(previous_neighbor_infectivity = ifelse(adjoint_previous_chamber == F &
                                                previous_neighbor_isolated == F &
                                                previous_neighbor_status != "s",
                                              T, F),
               next_neighbor_infectivity = ifelse(adjoint_next_chamber == F &
                                                next_neighbor_isolated == F &
                                                next_neighbor_status != "s",
                                              T, F))]

  area[!is.na(cow_id),
        is_exposed := (is_isolated == F &
                         (previous_neighbor_infectivity == T |
                          next_neighbor_infectivity == T)
                       )]

  return(area)
}


#' Check whether a barn is tie-stall
#'
#' @param param_area See [param_area].
#'
#' @return A logical vector of length 4 which indicates whether barns are tie-stall or not.
depreciate_is_ts <- function(param_area) {
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
depreciate_is_md_separated_in_ts <- function(param_calculated) {
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
#' @param area See [tie_stall_table].
#' @param cow_id_removed The ID of cows removed from the area.
#'
#' @return A [tie_stall_table].
remove_from_area <- function(area, cow_id_removed) {
  # Remove chambers for isolation
  removed_chamber <- area[cow_id %in% cow_id_removed, chamber_id]

  contact1 <- removed_chamber + 1
  contact2 <- removed_chamber - 1
  area[contact1[contact1 != (.N + 1)], ':='(previous_neighbor_status = NA,
                                             previous_neighbor_isolated = NA,
                                             previous_neighbor_infectivity = F)]
  area[contact2[contact2 != 0], ':='(next_neighbor_status = NA,
                                      next_neighbor_isolated = NA,
                                      next_neighbor_infectivity = F)]

  area[removed_chamber, ':='(cow_id = NA,
                              cow_status = NA,
                              is_isolated = NA,
                              hazard_ratio = NA,
                              is_exposed = NA)]

  return(area)
}


#' Find empty chambers in a tie_stall_table
#'
#' Find empty chambers in a tie_stall_table and assign cows to chambers.
#'
#' @param area See [tie_stall_table].
#' @param added_cows A [cow_table] consisted of cows to add to the area.
#'
#' @return A [tie_stall_table].
depreciated_find_empty_chamber <- function(area, added_cows) {
  area <- copy(area)

  empty_chamber <- area[is.na(cow_id), chamber_id]
  added_chamber <- sample(empty_chamber, nrow(added_cows))

  area[added_chamber, ':='(cow_id = added_cows$cow_id,
                            cow_status = added_cows$infection_status,
                            is_isolated = added_cows$is_isolated)]

  next_neighbor <- added_chamber[which(added_chamber != nrow(area))] + 1
  previous_neighbor <- added_chamber[which(added_chamber != 1)] - 1
  area[next_neighbor, ':='(previous_neighbor_status = area[next_neighbor - 1, cow_status],
                        previous_neighbor_isolated = area[next_neighbor - 1, is_isolated])]
  area[previous_neighbor, ':='(next_neighbor_status = area[previous_neighbor + 1, cow_status],
                        next_neighbor_isolated = area[previous_neighbor + 1, is_isolated])]

  # TODO: ここ、infectivityがNAになる場合の対処はできているか？　そもそもNAにならないのか？
  area[c(added_chamber, next_neighbor, previous_neighbor),
        ':='(previous_neighbor_infectivity = ifelse(adjoint_previous_chamber == F &
                                             previous_neighbor_isolated == F &
                                             previous_neighbor_status != "s", T, F),
             next_neighbor_infectivity = ifelse(adjoint_next_chamber == F &
                                             next_neighbor_isolated == F &
                                             next_neighbor_status != "s", T, F))]
  area[, is_exposed := (is_isolated == F &
                         (previous_neighbor_infectivity == T |
                          next_neighbor_infectivity == T))]
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


#' Assign chamber_id to cows allocated to tie-stall barns
#'
#' Assign `chamber_id` to cows allocated to tie-stall barns.
#'
#' @param cows See [cow_table].
#' @param area_list See [setup_areas].
#' @param area_assignment See [calculate_area_assignment()].
#' 
#' @note This function assign `chamber_id` just for `cows`. Assignment of `cow_id` in `area_list` must be done by [assign_cows] after using this function.
#'
#' @return A [cow_table].
assign_chambers <- function(cows, area_list, area_assignment) {
  for (i_area in names(area_assignment)) {
    assigned_area <- area_list[[i_area]]
    assigned_cows <- area_assignment[[i_area]]
    empty_chambers <- assigned_area$chamber_id[is.na(assigned_area$cow_id)]
    assigned_chambers <- sample(empty_chambers, length(assigned_cows))
    cows$chamber_id[cows$cow_id %in% assigned_cows] <- assigned_chambers
  }
  return(cows)
}
# TODO: Think a way to combine assign_chambers and assign_cows


#' Calculate infection in barns
#' 
#' Calculate infection in barns depending on barn type (tied or freed)
#' 
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param area_list See [setup_areas] and [tie_stall_table].
#' 
#' @return A [cow_table].
calc_infection_in_barns <- function(cows, area_table, area_list) {
  for (i_area in names(area_list)) {
    area <- area_list[[i_area]]
    if (i_area %in% attr(area_table, "tie_stall")) {
      is_infectious <- area[, cow_status != "s" & !is_isolated]
      is_infectious[is.na(is_infectious)] <- F
      is_exposed_to_infected_cow_in_next_chamber <-
        area$adjoint_next_chamber &
        shift(is_infectious, type = "lead", fill = F)
      is_exposed_to_infected_cow_in_previous_chamber <-
        area$adjoint_next_chamber &
        shift(is_infectious, type = "lag", fill = F)
      is_s_in_chamber <- area[, !is.na(area$cow_status) & cow_status == "s"]
      is_exposed_to_infected_cow <-
        is_exposed_to_infected_cow_in_next_chamber |
        is_exposed_to_infected_cow_in_previous_chamber
      expose_result <-
        is_infected_in_exposed_chamber(sum(is_exposed_to_infected_cow))
      cows[is_s_in_chamber & is_exposed_to_infected_cow,
           `:=`(infection_status =
                  c("ial", NA_character_)[is.na(expose_result) + 1],
                cause_infection = expose_result)]
      cows[is_s_in_chamber & !is_exposed_to_infected_cow &
             is_infected_in_non_exposed_chamber(.N),
           `:=`(infection_status = "ial",
                cause_infection = "insect")]
    } else {
    }
  }
  return(cows)
}


#' Assign cows roaming in a tie-stall to empty chambers
#'
#' Assign `chamber_id` to cows roaming in a tie-stall barn.
#'
#' @param cows See [cow_table].
#' @param area_list See [setup_areas].
#'
#' @return A list consisted of [area_list] and [cow_table].
tether_roaming_cows <- function(cows, area_list) {
  roaming_cows <- cows[chamber_id == 0 & is_owned, ]
  roaming_cow_assign_list <- split(roaming_cows$cow_id, roaming_cows$area_id)
  cows <- assign_chambers(cows, area_list, roaming_cow_assign_list)
  area_list <- assign_cows(cows, area_list, roaming_cow_assign_list)
  return(list(cows = cows, area_list = area_list))
}


#' Assign cows to area_list according to cow_table.
#'
#' Assign `chamber_id` to cows allocated to tie-stall barns.
#'
#' @param cows See [cow_table].
#' @param area_list See [setup_areas].
#' @param area_assignment See [calculate_area_assignment()].
#'
#' @note This function assign `cow_id` just for `area_list`. Assignment of `chamber_id` in `cows` must be done by [assign_chambers] before using this function.
#'
#' @return A [area_list].
assign_cows <- function(cows, area_list, area_assignment) {
  for (i_area in names(area_assignment)) {
    assigned_area <- area_list[[i_area]]
    assigned_cow_id <- area_assignment[[i_area]]
    assigned_chambers <- cows$chamber_id[match(assigned_cow_id, cows$cow_id)]
    assigned_cows <- cows[cow_id %in% assigned_cow_id,
                          list(cow_id, infection_status, is_isolated)]
    assigned_area[chamber_id %in% assigned_chambers,
                  c("cow_id", "cow_status", "is_isolated") := assigned_cows]
    area_list[[i_area]] <- assigned_area
  }
  return(area_list)
}


#' Make area_assignment list
#'
#' Make an `area_assignment` list which is used for [assign_chambers()] and [assign_cows()].
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param assigned_cow_id integer vector. An `area_assignment` list will be made only about cows specified by this parameter. When `NULL` is set, all the cows are used.
#' 
#' @return A list in a form of `list(area_id_of_a_tie_stall_barn = c(cow_ids_to_be_assigned_to_chambers_in_the_area), ...).
calculate_area_assignment <- function(cows, area_table, assigned_cow_id) {
  if (is.null(assigned_cow_id)) {
    cows_assigned <- cows
  } else {
    cows_assigned <- cows[cow_id %in% assigned_cow_id, ]
  }
  tied_cows <- cows_assigned[area_id %in% attr(area_table, "tie_stall"), ]
  area_assignment <- split(tied_cows$cow_id, tied_cows$area_id)
  return(area_assignment)
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

