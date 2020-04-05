#' Remove cows from areas
#'
#' Assign `NA`s to `area_id` and `chamber_id` of specified cows.
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas].
#' @param area_table See [area_table].
#' @param removed_cow_id `cow_id` of cows to be removed from current areas.
#'
#' @return A [cow_table] in which `area_id` and `chamber_id` of specified cows are set as `NA`.
remove_from_areas <- function(cows, areas, area_table, removed_cow_id) {
  cows[cow_id %in% removed_cow_id, `:=`(area_id = NA_integer_,
                                        chamber_id = NA_integer_)]
  for (i_area in as.character(attr(area_table, "tie_stall"))) {
    areas[[i_area]][cow_id %in% removed_cow_id,
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
    n_assigned_cows <- min(length(candidate_cow_id), length(empty_chambers))
    assigned_chambers <- resample(empty_chambers, n_assigned_cows)
    assigned_cow_id <- candidate_cow_id[seq_len(n_assigned_cows)]
    cows$chamber_id[match(assigned_cow_id, cows$cow_id)] <-
      assigned_chambers
    assigned_cows <- cows[match(assigned_cow_id, cow_id),
                          list(cow_id, infection_status, is_isolated)]
    assigned_area[match(assigned_chambers, chamber_id),
                  c("cow_id", "cow_status", "is_isolated") := assigned_cows]
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
  expose_status <- causes <- character(nrow(cows))
  names(expose_status) <- as.character(cows$cow_id)
  for (i_area in names(areas)) {
    area <- areas[[i_area]]
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
      exposed_cow <- area$cow_id[is_exposed_to_infected_cow]
      non_exposed_cow <-
        area$cow_id[is_s_in_chamber & !is_exposed_to_infected_cow]
      expose_status[as.character(exposed_cow)] <- "exposed"
      expose_status[as.character(non_exposed_cow)] <- "non_exposed"
    } else {
      s_cow_id <- area$cow_id[area$cow_status == "s"]
      new_infected_cow_id <- s_cow_id[
        is_infected_in_free_stall(length(s_cow_id),
                                  sum(area$cow_status != "s", na.rm = T),
                                  month, param_sim)
        ]
      expose_status[as.character(s_cow_id)] <- "not_tied"
    }
  }
  expose_result <- is_infected_in_exposed_chamber(
    sum(expose_status == "exposed"), month, param_sim
    )
  causes[expose_status == "exposed"] <- expose_result
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
  roaming_cows <- cows[chamber_id == 0 & is_owned, ]
  roaming_cow_assign_list <- split(roaming_cows$cow_id, roaming_cows$area_id)
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
  if (is.null(assigned_cow_id)) {
    cows_assigned <- cows
  } else {
    cows_assigned <- cows[cow_id %in% assigned_cow_id, ]
  }
  cows_assigned_to_tie <-
    cows_assigned[area_id %in% attr(area_table, "tie_stall"), ]
  area_assignment <- split(cows_assigned_to_tie$cow_id,
                           cows_assigned_to_tie$area_id)
  return(area_assignment)
}


#' Calculate capacity of an area based on inputed parameters
#'
#' @param herd_size The herd size in a simulated herd.
#' @param param See [param].
#'
#' @return Numeric vector of length 2: `c(lower_limit_of_herd_size, upper_limit_of_herd_size)`.
set_capacity <- function(herd_size, param) {
  if (!anyNA(param$capacity_in_head)) {
    capacity <- param$capacity_in_head
  } else if (!anyNA(param$capacity_as_ratio)) {
    capacity <- round(c(herd_size * param$capacity_as_ratio[1],
                        herd_size * param$capacity_as_ratio[2]))
  } else {
    capacity <- round(c(herd_size * 0.9, herd_size * 1.1))
  }
  return(capacity)
}

