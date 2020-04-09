#' Remove cows from a herd
#'
#' Assign `NA`s to `area_id` and `chamber_id` of specified cows.
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas].
#' @param i The number of months from the start of the simulation.
#' @param area_table See [area_table].
#' @param removed_row Row indice in `cow_table` of cows to be removed from current areas.
#' @param cause_removal A cause of removal of a cow.
#'
#' @return A [cow_table] in which `area_id` and `chamber_id` of specified cows are set as `NA`.
remove_cows <- function(cows, areas, i, area_table, removed_row,
                        cause_removal) {
  cows[removed_row, `:=`(is_owned = F,
                         date_removal = i,
                         cause_removal = cause_removal)]
  attr(cows, "herd_size") <- sum(cows$is_owned, na.rm = T)
  res <- remove_from_areas(cows, areas, area_table, removed_row)
  return(res)
}



#' Remove cows from areas
#'
#' Assign `NA`s to `area_id` and `chamber_id` of specified cows.
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas].
#' @param area_table See [area_table].
#' @param removed_row Row indice in `cow_table` of cows to be removed from current areas.
#'
#' @return A [cow_table] in which `area_id` and `chamber_id` of specified cows are set as `NA`.
remove_from_areas <- function(cows, areas, area_table, removed_row) {
  cows[removed_row, `:=`(area_id = NA_integer_,
                         chamber_id = NA_integer_)]
  removed_cow_id <- cows$cow_id[removed_row]
  for (i_area in attr(area_table, "tie_stall_chr")) {
    areas[[i_area]][match(removed_cow_id, cow_id),
                    `:=`(cow_id = NA_integer_,
                         cow_status = NA_character_,
                         is_isolated = NA)]
  }
  return(list(cows = cows, areas = areas))
}


#' Assign chamber_id to cows allocated to tie-stall barns
#'
#' Assign `chamber_id` to cows allocated to tie-stall barns.
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas].
#' @param area_assignment See [calculate_area_assignment()].
#'
#' @return A [cow_table].
assign_chambers <- function(cows, areas, area_assignment) {
  for (i_area in names(area_assignment)) {
    assigned_area <- areas[[i_area]]
    candidate_cow_id <- area_assignment[[i_area]]
    empty_chambers <- assigned_area$chamber_id[is.na(assigned_area$cow_id)]

    n_candidates <- length(candidate_cow_id)
    n_chambers <- length(empty_chambers)
    n_assigned_cows <- min(n_candidates, n_chambers)
    assigned_chambers <- resample(empty_chambers, n_assigned_cows)
    assigned_cow_id <- resample(candidate_cow_id, n_assigned_cows)

    rows_assigned <- match(assigned_cow_id, cows$cow_id)
    if (n_candidates > n_chambers) {
      rows$chamber_id[match(candidate_cow_id, cows$cow_id)] <- 0
    }
    cows$chamber_id[rows_assigned] <- assigned_chambers
    assigned_area[match(assigned_chambers, chamber_id),
                  `:=`(cow_id = cows$cow_id[rows_assigned],
                       cow_statuts = cows$infection_status[rows_assigned],
                       is_isolated = cows$is_isolated[rows_assigned])]
    areas[[i_area]] <- assigned_area
  }
  return(list(cows = cows, areas = areas))
}


#' Calculate infection in barns
#'
#' Calculate infection in barns depending on barn type (tied or freed)
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param month The current month (1, 2, ..., 12).
#' @param area_table See [area_table].
#' @param areas See [setup_areas] and [tie_stall_table].
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A [cow_table].
calc_infection_in_barns <- function(cows, i, month, area_table, areas,
                                    param_sim) {
  expose_status <- causes <- character(sum(!is.na(cows)))
  names(expose_status) <- as.character(cows$cow_id[!is.na(cows$cow_id)])
  for (i_area in names(areas)) {
    area <- areas[[i_area]]
    cows_in_area <- as.character(area$cow_id)
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
      exposed_cow <- cows_in_area[is_exposed_to_infected_cow]
      non_exposed_cow <-
        cows_in_area[is_s_in_chamber & !is_exposed_to_infected_cow]
      expose_status[exposed_cow] <- "exposed"
      expose_status[non_exposed_cow] <- "non_exposed"
    } else {
      s_cow_id <- cows_in_area[area$cow_status == "s"]
      new_infected_cow_id <- s_cow_id[
        is_infected_in_free_stall(length(s_cow_id),
                                  sum(area$cow_status != "s", na.rm = T),
                                  month, param_sim)
        ]
      expose_status[s_cow_id] <- "not_tied"
    }
  }
  expose_result <- is_infected_in_exposed_chamber(
    sum(expose_status == "exposed"), month, param_sim
    )
  causes[expose_status == "exposed"] <- c("", "insects")[expose_result + 1]
  non_expose_result <- is_infected_in_non_exposed_chamber(
    sum(expose_status == "non_exposed"), month, param_sim
    )
  causes[expose_status == "non_exposed"] <-
    c("", "insects")[non_expose_result + 1]
  causes[expose_status == "not_tied"] <- "insects"
  is_inf <- causes != ""
  res <- infect(cows, areas, area_table, cows$cow_id[is_inf], causes[is_inf], i)

  return(res)
}


#' Assign cows roaming in a tie-stall to empty chambers
#'
#' Assign `chamber_id` to cows roaming in a tie-stall barn.
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas].
#'
#' @return A list consisted of [areas] and [cow_table].
tether_roaming_cows <- function(cows, areas) {
  roaming_cow_assign_list <-
    cows[chamber_id == 0 & is_owned, split(cow_id, area_id)]
  res <- assign_chambers(cows, areas, roaming_cow_assign_list)
  return(res)
}


#' Make area_assignment list
#'
#' Make an `area_assignment` list which is used in [assign_chambers()].
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param assigned_cow_id integer vector. An `area_assignment` list will be made only about cows specified by this parameter. When `NULL` is set, all the cows are used.
#'
#' @return A list in a form of `list(area_id_of_a_tie_stall_barn = c(cow_ids_to_be_assigned_to_chambers_in_the_area), ...).
calculate_area_assignment <- function(cows, area_table, assigned_cow_id) {
  area_assignment <- cows[cow_id %in% assigned_cow_id &
                          area_id %in% attr(area_table, "tie_stall"),
                          split(cow_id, area_id)]
  return(area_assignment)
}

