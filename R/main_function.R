#' Increment age in a cow table
#'
#' Add 1 to `age` variable in a cow table.
#'
#' @param cows See [cow_table].
#'
#' @return A [cow_table].
add_1_to_age <- function(cows) {
  cows$age <- cows$age + 1
  return(cows)
}


#' Conduct AI and check chance of infection
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas] and [tie_stall_table].
#' @param area_table See [area_table].
#' @param i The number of months from the start of the simulation.
#' @param day_rp See [rp_table].
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A [cow_table].
do_ai <- function(cows, areas, area_table, i, day_rp, param_sim) {
  day_rp[, ] <- NA
  day_rp_last_row <- 0

  # Calculate day of next heat
  # Heat can occur 1-2 times/month because mean heat cycle is 21.
  # Here we use 4 to calculate the number of heat in month with some margin.
  heat_matrix <- matrix(heat_cycle(attr(cows, "herd_size") * 4, param_sim),
                        nrow = 4)
  heat_matrix <-
    rbind(cows$day_heat[cows$is_owned & !is.na(cows$is_owned)], heat_matrix)
  heat_matrix <- apply(heat_matrix, 2, cumsum)
  possible_heat_list <- lapply(data.frame(heat_matrix), function(x) x[x <= 30])
  day_heat_of_next_month <- apply(heat_matrix, 2,
                                  function(x) x[sum(x <= 30) + 1] - 30)

  rows_heifer <-
    cows[stage == "heifer" & n_ai == 0 & is.na(date_got_pregnant), which = TRUE]
  is_ai_started_heifer <-
    is_ai_started_heifer(cows$age[rows_heifer], param_sim)
  rows_adult <- cows[(stage == "milking" | stage == "dry") & n_ai == 0 &
                     is.na(cows$date_got_pregnant),
                     which = T]
  is_ai_started_adult <-
    is_ai_started_milking(i - cows$date_last_delivery[rows_adult], param_sim)
  rows_started_ai <-
    c(rows_heifer[is_ai_started_heifer], rows_adult[is_ai_started_adult])

  # The first AI
  # This is separated from later AI because the heat in this month must be found
  # in order to follow the data about date of doing first AI.
  n_cows_started_ai <- length(rows_started_ai)
  if (n_cows_started_ai != 0) {
    possible_heat_detection_list <- possible_detected_heat_list <-
      possible_succeeded_ai_list <- vector("list", n_cows_started_ai)
    n_ai_vec <- numeric(n_cows_started_ai)
    pregnancy <- logical(n_cows_started_ai)

    # Continue until AI is conducted to all candidate cows
    possible_heat_started_ai <- possible_heat_list[rows_started_ai]
    while (sum(n_ai_vec == 0) != 0) {
      index_ai_not_done <- which(n_ai_vec == 0)
      possible_heat <- possible_heat_started_ai[index_ai_not_done]
      calculated_ai <- calc_ai_list(possible_heat, param_sim)
      n_ai_vec[index_ai_not_done] <- calculated_ai$n_ai
      possible_succeeded_ai_list[index_ai_not_done] <-
        calculated_ai$succeeded_ai
      possible_heat_detection_list[index_ai_not_done] <-
        calculated_ai$heat_detection
      possible_detected_heat_list[index_ai_not_done] <-
        calculated_ai$detected_heat
      pregnancy[index_ai_not_done] <- calculated_ai$pregnancy
    }

    calculated_heat <-
      calc_heat(possible_heat_started_ai,
                list(succeeded_ai = possible_succeeded_ai_list,
                     heat_detection = possible_heat_detection_list,
                     detected_heat = possible_detected_heat_list))

    cows[rows_started_ai,
         `:=`(n_ai = n_ai_vec,
              day_heat = day_heat_of_next_month[rows_started_ai],
              day_last_detected_heat = calculated_heat$day_last_detected_heat,
              is_to_test_pregnancy = T)]
    cows[rows_started_ai[pregnancy], `:=`(date_got_pregnant = i,
                                          n_ai = 0)]

    n_ai_done <- sum(n_ai_vec)
    if (n_ai_done != 0) {
      day_rp[1:n_ai_done,
             `:=`(cow_id = rep(cows$cow_id[rows_started_ai], n_ai_vec),
                  infection_status = rep(cows$infection_status[rows_started_ai],
                                         n_ai_vec),
                  day_rp = calculated_heat$detected_heat,
                  type = c("ai_am", "ai_pm")[(runif(n_ai_done) < 0.5) + 1])]
      day_rp_last_row <- n_ai_done
    }
  }

  # AI after the first one
  rows_open <- cows[n_ai != 0 & is.na(date_got_pregnant), which = T]

  n_open_cows <- length(rows_open)
  if (n_open_cows != 0) {
    possible_heat_open <- possible_heat_list[rows_open]
    calculated_ai <- calc_ai_list(possible_heat_open, param_sim)
    calculated_heat <- calc_heat(possible_heat_open, calculated_ai)

    cows[rows_open,
         `:=`(n_ai = n_ai + calculated_ai$n_ai,
              day_heat = day_heat_of_next_month[rows_open],
              day_last_detected_heat =
                fcoalesce(calculated_heat$day_last_detected_heat,
                          day_last_detected_heat),
              is_to_test_pregnancy = T)]
    cows[rows_open[pregnancy], `:=`(date_got_pregnant = i,
                                    n_ai = 0)]

    n_ai_done <- sum(calculated_ai$n_ai)
    if (n_ai_done != 0) {
      day_rp[day_rp_last_row + (1:n_ai_done),
             `:=`(cow_id = rep(cows$cow_id[rows_open], calculated_ai$n_ai),
                  infection_status =
                    rep(cows$infection_status[rows_open], calculated_ai$n_ai),
                  day_rp = calculated_heat$detected_heat,
                  type = c("ai_am", "ai_pm")[(runif(n_ai_done) < 0.5) + 1])]
      day_rp_last_row <- day_rp_last_row + n_ai_done
    }

    # No conception, however pregnancy cheking is done
    # because the heat right after the AI is overlooked.
    rows_ai_failed <- cows[is_to_test_pregnancy &
                           (is.na(date_got_pregnant) | date_got_pregnant != i),
                           which = T]
    n_cow_ai_failed <- length(rows_ai_failed)
    if (n_cow_ai_failed != 0) {
      day_rp[day_rp_last_row + (1:n_cow_ai_failed),
        `:=`(cow_id = cows$cow_id[rows_ai_failed],
             infection_status = cows$infection_status[rows_ai_failed],
             day_rp =
               15 * (cows$day_last_detected_heat[rows_ai_failed] <= 15) +
               30 * (cows$day_last_detected_heat[rows_ai_failed] > 15),
             type = "pregnancy_test_candidate")]
      day_rp_last_row <- day_rp_last_row + n_cow_ai_failed
      setorder(day_rp, cow_id, day_rp, na.last = T)
      # Remove cows to which AI was conducted in this month
      rp_rows_wrong_pregnancy_test <-
        day_rp[, list(type, type_prev = shift(type, type = "lag")),
               by = cow_id
              ][type == "pregnancy_test_candidate" & is.na(type_prev),
                which = T]
      day_rp$type[rp_rows_wrong_pregnancy_test] <- "pregnency_test"
      day_rp[type == "pregnancy_test_candidate", ] <- NA
      cows$is_to_test_pregnancy[
        match(day_rp[rp_rows_wrong_pregnancy_test], cows$cow_id)
        ] <- F
    }
  }

  # Pregnancy test
  rows_pregnant <- cows[date_got_pregnant == i - 1 | date_got_pregnant == i - 2,
                        which = T]
  n_pregnant_cows <- length(rows_pregnant)
  if (n_pregnant_cows != 0) {
    day_rp[
      day_rp_last_row + (1:n_pregnant_cows),
      `:=`(cow_id = cows$cow_id[rows_pregnant],
           infection_status = cows$infection_status[rows_pregnant],
           day_rp = 15 * (cows$day_last_detected_heat[rows_pregnant] <= 15) +
                      30 * (cows$day_last_detected_heat[rows_pregnant] > 15),
           type = "pregnancy_test")
      ]
    day_rp_last_row <- day_rp_last_row + n_pregnant_cows
    cows$is_to_test_pregnancy <- (cows$date_got_pregnant == i - 2)
  }

  # Health check after delivery
  rows_delivered <-
    cows[date_last_delivery == i - 1 | date_last_delivery == i - 2, which = T]
  n_delivered_cows <- length(rows_delivered)
  if (n_delivered_cows != 0) {
    day_rp[
      day_rp_last_row + (1:n_delivered_cows),
      `:=`(cow_id = cows$cow_id[rows_delivered],
           infection_status = cows$infection_status[rows_delivered],
           day_rp = 15 * (cows$day_last_detected_heat[rows_delivered] <= 15) +
                      30 * (cows$day_last_detected_heat[rows_delivered] > 15),
           type = "health_check")
      ]
    day_rp_last_row <- day_rp_last_row + n_delivered_cows
  }

  # Judging about horizontal infection
  # Divide change of rectal palpation into four areas:
  # - AI (in morning) and  AI (in afternoon) (everyday)
  # - health check after the delivery (15th and 30th of every month)
  # - pregnancy checking (ditto)
  # The cow on which rectal palpation was conducted RIGHT AFTER an infected cow has a chance of infection.
  # (At first, cows after (not RIGHT after) an infected cow has a risk of infection. But it was modified because it showed too high infection rate.)

  if (day_rp_last_row != 0) {
    day_rp[1:day_rp_last_row, `:=`(i_rp = sample.int(.N)),
           by = list(day_rp, type)]
    setorder(day_rp, day_rp, type, i_rp, na.last = T)
    day_rp[,
           `:=`(is_after_inf = (shift(infection_status, type = "lag") != "s")),
           by = list(day_rp, type)]
    day_rp[infection_status == "s",
           `:=`(is_infected = (is_after_inf & is_infected_rp(.N, param_sim)))]
    res <- infect(cows, areas, area_table,
                  day_rp[is_infected == T, cow_id], "rp", i)
                  # is_infected = NA rows are excluded
  } else {
    res <- list(cows = cows, areas = areas)
  }

  return(res)
}


#' Calculate the number of conducted AI
#'
#' @param possible_heat A list consisted of day of possible heats in a month.
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @name calc_ai
calc_ai_list <- function(possible_heat, param_sim) {
  heat_detection <-
    lapply(possible_heat, function(x) is_heat_detected(length(x), param_sim))
  detected_heat <- mapply(function(x, y) x[y],
                          possible_heat, heat_detection,
                          SIMPLIFY = F)
  succeeded_ai <-
    lapply(detected_heat, function(x) is_ai_succeeded(length(x), param_sim))
  n_ai <- vapply(succeeded_ai,
    function(x) ifelse(length(x) == 0 | !any(x), length(x), min(which(x))), 0)
  # Here ifelse is used instead of fifelse,
  # because min(which(x)) may cause warning when the condition is not met.
  pregnancy <- vapply(succeeded_ai, function(x) length(x) > 0, T)
  return(list(succeeded_ai = succeeded_ai, heat_detection = heat_detection,
              detected_heat = detected_heat, n_ai = n_ai,
              pregnancy = pregnancy))
}


#' @param calculated_ai A list consisted of `"succeeded_ai"`, `"heat_detection"` and `"detected_heat"`.
#'
#' @name calc_ai
calc_heat <- function(possible_heat, calculated_ai) {
  n_heat <- vapply(calculated_ai$succeeded_ai,
    function(x) ifelse(!any(x), length(x), min(which(x))), 0)
  # Here ifelse is used instead of fifelse,
  # because min(which(x)) may cause warning when the condition is not met.
  heat_list <- mapply(function(x, y) x[1:y],
                      possible_heat, n_heat, SIMPLIFY = F)
  heat_detection_list <-
    mapply(function(x, y) x[1:y], calculated_ai$heat_detection, n_heat,
           SIMPLIFY = F)
  detected_heat_list <-
    mapply(function(x, y) x[1:y], calculated_ai$detected_heat, n_heat,
           SIMPLIFY = F)
  detected_heat <- unlist(detected_heat_list)
  day_last_detected_heat <- vapply(detected_heat_list,
    function(x) ifelse(length(x) == 0, NA_real_, x[length(x)]), 0)
  # Here ifelse is used, too.
  return(list(detected_heat = detected_heat,
              day_last_detected_heat = day_last_detected_heat))
}


#' Change stage of cows accordinglly
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A list consists of `cow_table` and `tie_stall_table`.
change_stage <- function(cows, i, param_sim) {
  # TODO: 12-23mo is heifer (temporary)
  # Calf to heifer
  cows[age == 12, `:=`(stage = "heifer",
                       parity = 0)]

  # Heifer/Dry to milking
  cows[(i - date_got_pregnant) == 10,
       `:=`(stage = "milking",
            date_last_delivery = i,
            parity = parity + 1,
            date_got_pregnant = NA,
            day_heat = sample.int(30, .N, replace = T) * 1)]

  # Milking to dry
  cows[stage == "milking" & is_dried(i - date_last_delivery, param_sim),
       `:=`(stage = "dry")]

  return(cows)
}


#' Check change of infection status of cows
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param month The month (1, 2, ..., 12) of month `i`.
#' @param area_table See [area_table].
#' @param areas See [setup_areas] and [tie_stall_table].
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A [cow_table].
change_infection_status <- function(cows, i, month, area_table, areas,
                                    param_sim) {
  res <- calc_infection_in_barns(cows, i, month, area_table, areas, param_sim)

  s_cows <- cows$cow_id[cows$infection_status == "s"]
  cows_inf_by_needles <- s_cows[is_infected_needles(cows, param_sim)]
  res <- infect(cows, areas, area_table, cows_inf_by_needles, "needles", i)

  res$cows[date_ial == i,
           c("date_ipl_expected", "date_ebl_expected") :=
             n_month_to_progress(susceptibility_ial_to_ipl,
                                 susceptibility_ipl_to_ebl,
                                 i, param_sim)]

  res$cows[date_ipl_expected == i,
           `:=`(infection_status = "ipl",
                date_ipl = i)]
  res$cows[date_ebl_expected == i,
           `:=`(infection_status = "ebl",
                date_ebl = i)]

  return(res)
}


#' Conduct BLV test
#'
#' @param cows See [cow_table].
#' @param month The month (1, 2, ..., 12).
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A [cow_table].
do_test <- function(cows, month, param_sim) {
  if (sum(month == param_sim$test_months) > 0) {
    is_detected <- cows$infection_status != "s" &
       runif(attr(cows, "herd_size")) < param_sim$test_sensitivity
    is_false_positive <- cows$infection_status == "s" &
      runif(attr(cows, "herd_size")) > param_sim$test_specificity
    cows$is_detected <- is_detected | is_false_positive
  }
  return(cows)
}

#' Add newborns to a cow_table
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param i The number of months from the start of the simulation.
#' @param max_cow_id The ID of a cow at the last non-empty row of `cows`.
#' @param newborn_table A result of [setup_newborn_table()].
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A list consisted of two elements: `cows` and `max_cow_id`.
add_newborns <- function(cows, area_table, i, max_cow_id, newborn_table,
                         param_sim) {
  newborn_table[, ] <- NA
  rows_mothers <- which(cows$date_last_delivery == i)
  # TODO: Make newborn_list like day_rp
  # Here, date_last_delivery == i (not i - 12), because date_last_delivery is changed by change_stage().
  # TODO: Temporary delivery interval is set to 12 months.
  # TODO: Consider the functions to change stage and consider delivery could be the same or not.

  if (length(rows_mothers) != 0) {  # If there is any cow delivers in 'month i'
    n_newborns_per_cow <- n_newborn_per_dam(length(rows_mothers), param_sim)
    n_newborns <- sum(n_newborns_per_cow)
    rows_newborns <- 1:n_newborns
    # 1:n is used because it is much faster than seq_len(n).

    newborn_table[rows_newborns,
      `:=`(id_mother = rep(cows$cow_id[rows_mothers], n_newborns_per_cow),
           id_calf = rows_newborns,
           n_litter =
             rep(n_newborns_per_cow, n_newborns_per_cow),
           status_mother = rep(cows[rows_mothers, infection_status],
                               n_newborns_per_cow),
           age = 0,
           stage = "calf",
           sex = sex_newborns(n_newborns, param_sim),
           is_freemartin = F,
           date_birth = i,
           is_owned = T,
           parity = 0,
           n_ai = 0,
           day_heat = sample.int(30, n_newborns, replace = T) * 1,
           infection_status = "s",
           area_id = 1,
           months_in_area = 0,
           is_isolated = attr(area_table, "is_calf_isolated"),
           i_month = i)]

    # Setting about twins
    if (sum(newborn_table$n_litter == 2, na.rm = T) != 0) {
      newborn_table[n_litter == 2,
                    `:=`(sex = sex_twins(.N, param_sim),
                         is_freemartin = (sex == "freemartin"))]
      newborn_table[is_freemartin == T, `:=`(sex = "female")]
    }

    newborn_table[sex == "male" | is_freemartin == T, `:=`(is_replacement = F)]
    # Male calves and freemartin female calves will be sold
    newborn_table[is.na(is_replacement) & !is.na(id_calf),
      `:=`(is_replacement =
             is_replacement(.N, attr(cows, "herd_size"), param_sim))]

    # Setting of longevity
    longevity <- longevity(n_newborns, param_sim)
    newborn_table[rows_newborns,
                  `:=`(date_removal_expected = i + longevity$age,
                       cause_removal = longevity$cause)]

    # Susceptibility
    susceptibility <- susceptibility(
      n_newborns,
      rep(cows$susceptibility_ial_to_ipl[rows_mothers], n_newborns_per_cow),
      rep(cows$susceptibility_ipl_to_ebl[rows_mothers], n_newborns_per_cow),
      param_sim
      )
    newborn_table[rows_newborns,
                  `:=`(susceptibility_ial_to_ipl = susceptibility$ial_to_ipl,
                       susceptibility_ipl_to_ebl = susceptibility$ipl_to_ebl)]

    # Calculation of vertical infection
    # infect() cannot used here because newborns are not yet added to areas.
    is_inf_vert <-
      is_infected_vertical(newborn_table$status_mother[rows_newborns],
                           param_sim)
    newborn_table[rows_newborns[is_inf_vert],
                  `:=`(infection_status = "ial",
                       date_ial = i,
                       cause_infection = "vertical"
                       )]
    is_inf_colostrum <-
      is_infected_by_colostrum(newborn_table$status_mother[rows_newborns],
                               param_sim) &
      newborn_table$infection_status[rows_newborns] != "s"
    newborn_table[rows_newborns[is_inf_colostrum],
                  `:=`(infection_status = "ial",
                       date_ial = i,
                       cause_infection = "colostrum")]

    # TODO: Simulate failure of delivery (stillbirth/abortion) 妊娠途中で流産する場合についてもどこかで計算しなければ。
    parity_mothers <-
      cows[match(newborn_table$id_mother[rows_newborns], cow_id), parity]
    rows_born_alive <- rows_newborns[!is_stillbirth(parity_mothers, param_sim)]

    n_newborns_born <- length(rows_born_alive)
    if (n_newborns_born != 0) {
      newborn_table$cow_id[rows_born_alive] <- max_cow_id + 1:n_newborns_born
      # 1:n is used because it is much faster than seq_len(n).
      max_cow_id <- max_cow_id + n_newborns_born
      cows[attr(cows, "herd_size") + 1:n_newborns_born, ] <-
        newborn_table[rows_born_alive, ..cow_table_cols]
    }

  }

  # areas will be updated later in tether_roaming_cows().
  return(list(cows = cows, max_cow_id = max_cow_id))
}


#' Check death and sale of current cows
#'
#' @param cows See [cow_table].
#' @param areas See [tie_stall_table].
#' @param i The number of months from the start of the simulation.
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A list consisted of [cow_table] and [tie_stall_table].
check_removal <- function(cows, areas, i, area_table, param_sim) {
  # Removal by death
  rows_expected_removal <- which(cows$date_removal_expected == i)
  res <- remove_cows(cows, areas, i, area_table, rows_expected_removal,
    fifelse(cause_removal == "will_die", "died", "slaughtered"))
  cows <- res$cows
  areas <- res$areas

  # Removal by culling
  res <- cull_infected_cows(cows, areas, i, param_sim)
  cows <- res$cows
  areas <- res$areas

  # Removal by selling
  rows_removed_sold <- which(cows$is_replacement == F &
                               cows$date_removal_expected != i)
  res <- remove_cows(cows, areas, i, area_table, rows_removed_sold, "sold")
  cows <- res$cows
  areas <- res$areas
  # TODO: とりあえず後継牛以外は0ヶ月齢で売却

  # Removal by detection of EBL
  rows_new_ebl <- which(cows$date_ebl == i)
  rows_removed_ebl <- rows_new_ebl[is_ebl_detected(rows_new_ebl, param_sim)]
  if (length(rows_removed_ebl) != 0) {
    res <- remove_cows(cows, areas, i, area_table, rows_removed_ebl, "culled")
    cows <- res$cows
    areas <- res$areas
  }
  rows_overlooked <- rows_new_ebl[!rows_new_ebl %in% rows_removed_ebl]
  if (length(rows_overlooked) != 0) {
    month_ebl_die <- n_month_until_ebl_die(rows_overlooked, param_sim) + i
    cows[rows_overlooked,
         `:=`(date_removal_expected =
                fifelse(date_removal_expected >= month_ebl_die,
                        month_ebl_die, date_removal_expected),
              cause_removal =
                fifelse(date_removal_expected >= month_ebl_die,
                        "will_die", cause_removal))]
    # 1:n is used because it is much faster than seq_len(n).
  }

  return(res)
}


#' Assign `chamber_id` to newborns
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param areas See [setup_areas] and [tie_stall_table].
assign_newborns <- function(cows, area_table, areas) {
  newborn_cow_id <- cows$cow_id[cows$age == 0 & !is.na(cows$age)]
  n_newborn <- length(newborn_cow_id)
  if (all(attr(area_table, "tie_stall") != 1) | n_newborn == 0) {
    return(list(cows = cows, areas = areas))
  }

  calf_area <- areas[["1"]]
  is_empty_chambers <- is.na(calf_area$cow_id)
  n_empty_chambers <- sum(is_empty_chambers)
  if (n_empty_chambers < n_newborn) {
    cows$chamber_id[match(newborn_cow_id, cows$cow_id)] <- 0
    newborn_can_be_assigned <- resample(newborn_cow_id, n_empty_chambers)
  } else {
    newborn_can_be_assigned <- newborn_cow_id
  }

  assigned_chambers <- resample(calf_area$chamber_id[is_empty_chambers],
                                length(newborn_can_be_assigned))
  cows$chamber_id[match(newborn_can_be_assigned, cows$cow_id)] <-
    assigned_chambers
  areas[["1"]]$cow_id[assigned_chambers] <- newborn_can_be_assigned

  return(list(cows = cows, areas = areas))
}


#' Cull infected cows
#'
#' @param cows See [cow_table].
#' @param areas See [tie_stall_table].
#' @param i The number of months from the start of the simulation.
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A [cow_table].
cull_infected_cows <- function(cows, areas, i, param_sim) {
  if (param$cull_infected_cows == "no") {
    return(list(cows = cows, areas = areas))
  }

  id_detected_highrisk <-
    cows[(infection_status == "pl" | infection_status == "ebl") &
         is_detected & is_owned,
         cow_id]
  res <- replace_selected_cows(cows, areas, id_detected_highrisk, i)
  if (param$cull_infected_cows == "all") {
    id_detected <- cows[is_detected & is_owned, cow_id]
    res <- replace_selected_cows(cows, areas, id_detected, i)
  }
  return(res)
}


#' Replace selected cows
#'
#' @param cows See [cow_table].
#' @param areas See [tie_stall_table].
#' @param cow_id_to_cull `cow_id` to remove from `cows`.
#' @param i The number of months from the start of the simulation.
#'
#' @return A [cow_table].
replace_selected_cows <- function(cows, areas, cow_id_to_cull, i) {
  id_non_replacement_newborns <-
    cows[age == 0 & is_owned & !is_replacement & sex == "female",
         cow_id]
  n_non_replacement <- length(id_non_replacement_newborns)
  n_to_cull <- length(cow_id_to_cull)
  if (n_non_replacement != 0 & n_to_cull != 0) {
    if (n_non_replacement > n_to_cull) {
      id_culled <- cow_id_to_cull
      id_replaced <- resample(id_non_replacement_newborns, n_to_cull)
    } else {
      id_culled <- resample(cow_id_to_cull, n_non_replacement)
      id_replaced <- id_non_replacement_newborns
    }
    res <- remove_cows(cows, areas, i, area_table,
                       match(id_culled, cows$cow_id), "culled")
    cows$is_replacement[match(id_replaced, cows$cow_id)] <- T
  } else {
    res <- list(cows = cows, areas = areas)
  }
  return(res)
}


#' Set the variable i_month in a cow_table
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#'
#' @return A [cow_table].
set_i_month <- function(cows, i) {
  cows$i_month <- i
  return(cows)
}


#' Extract owned cows from a cow_table
#'
#' @param cows See [cow_table].
#'
#' @return A [cow_table].
extract_owned_cows <- function(cows) {
  cows <- cows[is_owned == T | is.na(is_owned), ]
  return(cows)
}


#' Check and move cows between areas
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param movement_table See [movement_table].
#' @param area_table See [area_table].
#' @param areas See [setup_areas] and [tie_stall_table].
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @return A list composed of [cow_table] and [areas].
change_area <- function(cows, i, movement_table, area_table, areas, param_sim) {
  # area_tableに沿って、移動する個体、合致したconditionを抽出
  # とりあえず全て移動させて、移動できなかった個体はchamber_idを決めない

  res <- assign_newborns(cows, area_table, areas)
  cows <- res$cows
  areas <- res$areas

  # Extract cows whose area must be changed
  cow_id_met_condition <- lapply(
    attr(movement_table, "cond_as_expr"),
    function(x) {cows[eval(x) & is_owned, cow_id]}
    )

  # Remove duplicated cow_id
  duplicated_cow_id <-
    relist(!duplicated(unlist(cow_id_met_condition)), cow_id_met_condition)
  cow_id_to_move <- mapply(function(x, y) {x[y]},
                           cow_id_met_condition, duplicated_cow_id,
                           SIMPLIFY = FALSE)
  cow_id_to_move <- unlist(cow_id_to_move_in_each_area)
  n_cows_to_move <- length(cow_id_to_move)
  if (n_cows_to_move == 0) {
    return(res)
  }

  cow_id_returned_from_pasture <-
      cows[cow_id %in% unlist(cow_id_to_move) & area_id == 0, cow_id]

  # Remove cows to move from n_cows
  n_cows_in_each_area <- table(
    factor(cows$area_id[cows$is_owned & !is.na(cows$is_owned)],
           levels = area_table$area_id)
    )
  n_cows_to_move_by_each_condition <- sapply(cow_id_to_move, length)
  n_cows_to_move_in_each_area <- tapply(
    n_cows_to_move_by_each_condition,
    attr(movement_table, "factored_current_area"),
    sum)
  empty_spaces <- attr(area_table, "capacity") - n_cows_in_each_area +
    n_cows_to_move_in_each_area
  empty_spaces[empty_spaces < 0] <- 0

  # Remove cows from areas
  vec_cows_to_move <- flatten_dbl(cow_id_met_condition)
  res <- remove_from_areas(cows, areas, area_table,
                           match(vec_cows_to_move, cows$cow_id))
  cows <- res$cows
  areas <- res$areas
  cow_id_allocated_to_full_areas <- numeric(attr(cows, "herd_size"))
  cow_id_allocated_to_full_areas_index <- 0

  # Decide to which next_area cows will move
  for (i_movement in 1:nrow(movement_table)) {
    # 1:n is used because it is much faster than seq_len(n).
    i_cow_id <- resample(cow_id_to_move[[i_movement]])
    # sample() here must not be replaced with sample.int() because the latter
    # causes error when the length of x is 0.
    # Order of cow_id is randomized to decide cow_id_allocated_to_full_areas
    i_next_area <- movement_table$next_area[[i_movement]]
    chr_i_next_area <- as.character(i_next_area)
    if (attr(movement_table, "is_priority_specified_by_integer")[i_movement]) {
      # A. For conditions with priorities specified by integers

      empty_spaces_in_next_areas <- empty_spaces[chr_i_next_area]
      allocated_area_index <-
        findInterval(seq_along(i_cow_id),
                     c(0, cumsum(empty_spaces_in_next_areas)), left.open = T)
      allocated_areas <- i_next_area[allocated_area_index]
      empty_spaces[chr_i_next_area] <- empty_spaces[chr_i_next_area] -
        table(factor(allocated_areas, levels = chr_i_next_area))

      # When length(i_cow_id) is larger than sum(empty_spaces_in_next_area),
      # allocated_area_index includes NA.
      # Then allocate such cows into full areas according to capacity.
      if (anyNA(allocated_areas)) {
        capacity_of_next_areas <- attr(area_table, "capacity")[chr_i_next_area]
        is_na_allocated_areas <- is.na(allocated_areas)
        n_na_allocated_areas <- sum(is_na_allocated_areas)
        if (any(capacity_of_next_areas == Inf)) {
          allocated_areas[which(is_na_allocated_areas)] <-
            resample(i_next_area[capacity_of_next_areas == Inf],
                     n_na_allocated_areas, replace = T)
        } else {
          allocated_areas[which(is_na_allocated_areas)] <-
            resample(i_next_area, n_na_allocated_areas, replace = T,
                     prob = capacity_of_next_areas)
        }
        cow_id_allocated_to_full_areas[
          cow_id_allocated_to_full_areas_index + 1:n_na_allocated_areas
          ] <- i_cow_id[is_na_allocated_areas]
        # 1:n is used because it is much faster than seq_len(n).
        cow_id_allocated_to_full_areas_index <-
          cow_id_allocated_to_full_areas_index + n_na_allocated_areas
      }
    } else {
      # B. For conditions with priorities specified by real numbers

      n_cows_to_move <- length(i_cow_id)
      i_priority <- movement_table$priority[[i_movement]]
      vacancy <- empty_spaces[i_next_area]

      if (sum(vacancy) > n_cows_to_move) {
        n_cows_to_reallocate <- n_cows_to_move
        n_cows_allocated_in_each_area <- setNames(numeric(length(i_next_area)),
                                                  chr_i_next_area)
        while (n_cows_to_reallocate > 0) {
          # When some cows are allocated to full areas, assign cows
          # to non-full areas.
          is_overcrowded <- vacancy < 0
          is_not_full <- vacancy > 0
          allocated_areas <- resample(i_next_area[is_not_full],
                                      n_cows_to_reallocate, replace = T,
                                      prob = i_priority[is_not_full])
          n_cows_reallocated_in_each_area <-
            table(factor(allocated_areas, levels = chr_i_next_area))
          vacancy <- vacancy - n_cows_reallocated_in_each_area
          n_cows_to_reallocate_in_each_area <- -vacancy * (vacancy < 0)
          n_cows_to_reallocate <- sum(n_cows_to_reallocate_in_each_area)
          n_cows_allocated_in_each_area <-
            n_cows_allocated_in_each_area + n_cows_reallocated_in_each_area -
            n_cows_to_reallocate_in_each_area
        }
        allocated_areas <- rep(i_next_area,
                               times = n_cows_allocated_in_each_area)
      } else {
        # When there is not enough vacancy
        n_cows_allocated_to_full_areas <- sum(vacancy) - n_cows_to_move
        cow_id_allocated_to_full_areas[
          seq_len(n_cows_allocated_to_full_areas) +
            cow_id_allocated_to_full_areas_index
          ] <- i_cow_id[seq_len(n_cows_allocated_to_full_areas)]
        cow_id_allocated_to_full_areas_index <-
          cow_id_allocated_to_full_areas_index + n_cows_allocated_to_full_areas

        capacity_of_areas <- attr(area_table, "capacity")[i_next_area]
        allocated_areas <- c(rep(i_next_area, vacancy),
          resample(i_next_area, n_cows_allocated_to_full_areas, replace = T,
                   prob = capacity_of_areas))
      }
    }
    cows$area_id[match(i_cow_id, cows$cow_id)] <- allocated_areas
  }
  # Assignment of a chamber_id for a cow allocated to a full but free area
  # will not occur because calculate_area_assignment() calculates only about
  # tie-stall areas.
  cows_to_allocate_chambers <-
    calculate_area_assignment(cows, area_table, vec_cows_to_move)
  res <- assign_chambers(cows, areas, cows_to_allocate_chambers)

  # Calculate seroconversion of cows have returned from a communal pasture
  if (any(cows$area_id == 0, na.rm = T)) {
    cow_id_infected_in_pasture <- cow_id_returned_from_pasture[
      is_infected_pasture(length(cow_id_returned_from_pasture), param_sim)
      ]
    res <-
      infect(cows, areas, area_table, cow_id_infected_in_pasture, "pasture", i)
  }

  return(res)
}


#' Change infection status of new infected cows
#'
#' Update `infection_status`, `date_ial` and `cause_infection` of [cow_table] and `cow_status` of [areas].
#'
#' @param cows See [cow_table].
#' @param areas See [setup_areas] and [tie_stall_table].
#' @param area_table See [area_table].
#' @param infected_cow_id `cow_id` of new infected cows.
#' @param cause A cause of infection.
#' @param i The number of months from the start of the simulation.
#'
#' @return A list composed of [cow_table] and [areas].
infect <- function(cows, areas, area_table, infected_cow_id, cause, i) {
  cows[match(infected_cow_id, cow_id),
       `:=`(infection_status = "ial",
            cause_infection = cause)]
  for (i_area in attr(area_table, "tie_stall_chr")) {
    areas[[i_area]][match(infected_cow_id, cow_id),
                    `:=`(cow_status = "ial")]
  }
  return(list(cows = cows, areas = areas))
}

