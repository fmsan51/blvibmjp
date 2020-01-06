#' Remove dead or sold cows from a area
#'
#' @param area See [tie_stall_table].
#' @param cow_id_removed The ID of cows removed from the area.
#'
#' @return A [tie_stall_table].
remove_from_area <- function(area, cow_id_removed) {
  # Remove chambers for isolation
  removed_chamber <- area[cow_id %in% cow_id_removed, chamber_id]
  area[removed_chamber, ':='(cow_id = NA,
                             cow_status = NA,
                             is_isolated = NA)]
  return(area)
}


#' Remove cows from areas
#'
#' Assign `NA`s to `area_id` and `chamber_id` of specified cows.
#'
#' @param cows See [cow_table].
#' @param area_list See [setup_areas].
#' @param area_table See [area_table].
#' @param removed_cow_id `cow_id` of cows to be removed from current areas.
#'
#' @return A [cow_table] in which `area_id` and `chamber_id` of specified cows are set as `NA`.
remove_from_areas <- function(cows, area_list, area_table, removed_cow_id) {
  cows[cow_id %in% removed_cow_id, `:=`(area_id = NA_integer_,
                                        chamber_id = NA_integer_)]
  for (i_area in as.character(attr(area_table, "tie_stall"))) {
    area_list[[i_area]][cow_id %in% removed_cow_id,
                        `:=`(cow_id = NA_integer_,
                             cow_status = NA_character_,
                             is_isolated = NA)]
  }
  return(list(cows = cows, area_list = area_list))
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
    assigned_chambers <- resample(empty_chambers, length(assigned_cows))
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
#' @param month The current month (1, 2, ..., 12).
#' @param area_table See [area_table].
#' @param area_list See [setup_areas] and [tie_stall_table].
#' 
#' @return A [cow_table].
calc_infection_in_barns <- function(cows, month, area_table, area_list,
                                    param_calculated) {
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
      is_s_in_chamber <- !is.na(area$cow_status) & area$cow_status == "s"
      is_exposed_to_infected_cow <- is_s_in_chamber &
        (is_exposed_to_infected_cow_in_next_chamber |
         is_exposed_to_infected_cow_in_previous_chamber)
      expose_result <-
        is_infected_in_exposed_chamber(sum(is_exposed_to_infected_cow),
                                       month, param_calculated)
      exposed_cow <- area$cow_id[is_exposed_to_infected_cow]
      cows[cow_id %in% exposed_cow,
           `:=`(infection_status =
                  c("ial", NA_character_)[is.na(expose_result) + 1],
                cause_infection = expose_result)]
      non_exposed_cow <-
        area$cow_id[is_s_in_chamber & !is_exposed_to_infected_cow]
      infected_non_exposed <- non_exposed_cow[
        is_infected_in_non_exposed_chamber(length(non_exposed_cow),
                                           month, param_calculated)]
      cows[cow_id %in% infected_non_exposed,
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
    is_na <- is.na(assigned_chambers)
    assigned_cows <- cows[cow_id %in% assigned_cow_id[!is_na],
                          list(cow_id, infection_status, is_isolated)]
    assigned_area[chamber_id %in% assigned_chambers[!is_na],
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
  area_assignment <- cows_assigned[area_id %in% attr(area_table, "tie_stall"),
                                   split(cow_id, area_id)]
  return(area_assignment)
}


#' Calculate capacity of an area based on inputed parameters
#'
#' @param herd_size The herd size in a simulated herd.
#' @param param_farm See [param_farm].
#'
#' @return Numeric vector of length 2: `c(lower_limit_of_herd_size, upper_limit_of_herd_size)`.
set_capacity <- function(herd_size, param_farm) {
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

