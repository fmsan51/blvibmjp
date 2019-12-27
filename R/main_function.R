#' Increment age in a cow table
#'
#' Add 1 to `age` variable in a cow table.
#'
#' @param cows See [cow_table].
#'
#' @return A [cow_table].
#' @export
add_1_to_age <- function(cows) {
  cows[, age := age + 1]
  return(cows)
}


#' Conduct AI and check chance of infection
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param param_calculated Return from [calc_param()].
#' @param day_rp See [rp_table].
#'
#' @return A [cow_table].
#' @export
do_ai <- function(cows, i, day_rp, param_calculated) {
  day_rp_last_row <- 0

  heifers_start_ai <- cows[
    stage == "heifer" & n_ai == 0 & is.na(date_got_pregnant),
    .SD[is_ai_started_heifer(age, param_calculated), ]]
  milking_cows_start_ai <- cows[
    stage == "milking" & n_ai == 0 & is.na(cows$date_got_pregnant),
    .SD[is_ai_started_milking(i - date_last_delivery, param_calculated), ]]
  cows_started_ai <- rbindlist(list(heifers_start_ai, milking_cows_start_ai))

  # The first AI
  # This is separated from later AI because the heat in this month must be found
  # in order to follow the data about date of doing first AI.
  n_cows_started_ai <- nrow(cows_started_ai)
  if (n_cows_started_ai != 0) {
    # Calculate day of next heat
    # Heat can occur 1-2 times/month because mean heat cycle is 21.
    # Here we use 4 to calculate the number of heat in month with some margin.
    heat_matrix <- matrix(
      heat_cycle(n_cows_started_ai * 4, param_calculated),
      nrow = 4)
    heat_matrix <- rbind(cows_started_ai$day_heat, heat_matrix)
    heat_matrix <- apply(heat_matrix, 2, cumsum)
    possible_heat_list <- apply(heat_matrix, 2, function(x) x[x <= 30])

    possible_heat_detection_list <- possible_detected_heat_list <-
      possible_ai_success_list <- vector("list", n_cows_started_ai)
    n_ai_vec <- numeric(n_cows_started_ai)
    # Continue until AI is conducted to all candidate cows
    while (sum(n_ai_vec == 0) != 0) {
      index_ai_not_done <- which(n_ai_vec == 0)
      possible_heat_detection_list[index_ai_not_done] <-
        lapply(possible_heat_list[index_ai_not_done],
               function(x) is_heat_detected(length(x), param_calculated))
      possible_detected_heat_list[index_ai_not_done] <- 
        mapply(function(x, y) x[y],
          possible_heat_list[index_ai_not_done], 
          possible_heat_detection_list[index_ai_not_done],
          SIMPLIFY = F)
      possible_ai_success_list[index_ai_not_done] <- lapply(
        possible_detected_heat_list[index_ai_not_done],
        function(x) is_first_ai_successed(length(x), param_calculated))
      n_ai_vec[index_ai_not_done] <- vapply(
        possible_ai_success_list[index_ai_not_done],
        function(x) ifelse(length(x) == 0 | !any(x), length(x), min(which(x))), 
        0)
      # Here ifelse is used instead of fifelse,
      # because min(which(x)) may cause warning when the condition is not met.
    }

    n_heat_vec <- vapply(possible_ai_success_list,
      function(x) ifelse(!any(x), length(x), min(which(x))), 0)
    # Here ifelse is used, too.
    heat_list <- mapply(function(x, y) x[1:y], possible_heat_list, n_heat_vec)
    heat_detection_list <- mapply(function(x, y) x[1:y],
                                  possible_heat_detection_list, n_heat_vec)
    detected_heat_list <- mapply(function(x, y) x[1:y],
                                   possible_detected_heat_list, n_heat_vec)

    day_heat_of_next_month <- 
      apply(heat_matrix, 2, function(x) x[sum(x <= 30) + 1] - 30)
    day_last_detected_heat_vec <- vapply(detected_heat_list,
      function(x) ifelse(length(x) == 0, NA_real_, x[length(x)]), 0)
    # Here ifelse is used, too.
    pregnancy <- vapply(possible_ai_success_list, function(x) length(x) > 0, T)

    cows_started_ai[, `:=`(n_ai = n_ai_vec,
                           day_heat = day_heat_of_next_month,
                           day_last_detected_heat = day_last_detected_heat_vec)]
    cows_started_ai[(pregnancy), `:=`(date_got_pregnant = i,
                                      n_ai = 0)]

    cows[match(cows_started_ai$cow_id, cow_id), 
         colnames(cows) := cows_started_ai]

    n_ai_done <- sum(n_ai_vec)
    if (n_ai_done != 0) {
      day_rp[1:n_ai_done,
             `:=`(cow_id = rep(cows_started_ai$cow_id, n_ai_vec),
                  infection_status = rep(cows_started_ai$infection_status,
                                         n_ai_vec),
                  day_rp = unlist(detected_heat_list),
                  type = c("ai_am", "ai_pm")[(runif(n_ai_done) < 0.5) + 1])]
      day_rp_last_row <- n_ai_done
    }
  }

  # AI after the first one
  open_cows <- cows[n_ai != 0 & is.na(date_got_pregnant), ]

  n_open_cows <- nrow(open_cows)
  if (n_open_cows != 0) {
    heat_matrix <- matrix(heat_cycle(n_open_cows * 4, param_calculated),
                          nrow = 4)
    heat_matrix <- rbind(open_cows$day_heat, heat_matrix)
    heat_matrix <- apply(heat_matrix, 2, cumsum)
    possible_heat_list <- apply(heat_matrix, 2, function(x) x[x <= 30])
    possible_heat_detection_list <- lapply(possible_heat_list,
      function(x) is_heat_detected(length(x), param_calculated))
    possible_detected_heat_list <- mapply(function(x, y) x[y],
                                          possible_heat_list,
                                          possible_heat_detection_list,
                                          SIMPLIFY = F)
    possible_ai_success_list <- lapply(possible_detected_heat_list,
      function(x) is_ai_successed(length(x), param_calculated))
    n_ai_vec <- vapply(possible_ai_success_list,
      function(x) ifelse(length(x) == 0 | !any(x), length(x), min(which(x))), 0)
    # Here ifelse is used, too.

    n_heat_vec <- vapply(possible_ai_success_list,
      function(x) ifelse(!any(x), length(x), min(which(x))), 0)
    # Here ifelse is used, too.
    heat_list <- mapply(function(x, y) x[seq_len(y)],
                        possible_heat_list, n_heat_vec)
    heat_detection_list <- mapply(function(x, y) x[seq_len(y)],
                                  possible_heat_detection_list, n_heat_vec)
    detected_heat_list <- mapply(function(x, y) x[seq_len(y)],
                                   possible_detected_heat_list, n_heat_vec)

    day_heat_of_next_month <- 
      apply(heat_matrix, 2, function(x) x[sum(x <= 30) + 1] - 30)
    day_last_detected_heat_vec <- vapply(detected_heat_list,
      function(x) ifelse(length(x) == 0, NA_real_, x[length(x)]), 0)
    # Here, ifelse is used, too.
    pregnancy <- vapply(possible_ai_success_list, function(x) length(x) > 0, T)

    open_cows[, `:=`(n_ai = n_ai_vec,
                     day_heat = day_heat_of_next_month,
                     day_last_detected_heat = day_last_detected_heat_vec)]
    open_cows[(pregnancy), `:=`(date_got_pregnant = i,
                                n_ai = 0)]

    cows[match(open_cows$cow_id, cow_id), colnames(cows) := open_cows]

    n_ai_done <- sum(n_ai_vec)
    if (n_ai_done != 0) {
      day_rp[day_rp_last_row + (1:n_ai_done),
             `:=`(cow_id = rep(open_cows$cow_id, n_ai_vec),
                  infection_status = rep(open_cows$infection_status, n_ai_vec),
                  day_rp = unlist(detected_heat_list),
                  type = c("ai_am", "ai_pm")[(runif(n_ai_done) < 0.5) + 1])]
      day_rp_last_row <- day_rp_last_row + n_ai_done
    }
    # No conception, however pregnancy cheking is done 
    # because the heat right after the AI is overlooked.
    cows_heat_overlooked <- open_cows[
      is_to_test_pregnancy & date_got_pregnant != i, ]
    n_cow_heat_overlooked <- nrow(cows_heat_overlooked)
    if (n_cow_heat_overlooked != 0) {
      # TODO: Consider the probability that next heat comes before pregnancy_test.
      possible_day_rp <- one_day_rp[rep(1, n_cow_heat_missed), ]
      possible_day_rp[,
        `:=`(cow_id = cows_heat_overlooked$cow_id,
             infection_status = cows_heat_overlooked$infection_status,
             day_rp = 
               15 * (cows_heat_overlooked$day_last_detected_heat <= 15) +
               30 * (cows_heat_overlooked$day_last_detected_heat > 15),
             type = "overlooked_heat")]
      heat_date <- day_rp[cow_id %in% cows_heat_overlooked$cow_id, ][
                          order(day_rp), head(.SD, 1), by = "cow_id"]
      nonpregnant_cows_ai_done <- heat_date[possible_day_rp, on = "cow_id"][
                                            i.day_rp < day_rp, 
                                            type := "pregnancy_test"]
      n_nonpregnant_cows_ai_done <- nrow(nonpregnant_cows_ai_done)
      if (n_nonpregnant_cows_ai_done > 0) {
        day_rp[day_rp_last_row + (1:n_nonpregnant_cows_ai_done),
               c("cow_id", "infection_status", "day_rp", "type") :=
                 nonpregnant_cows_ai_done[,
                   c("cow_id", "infection_status", "i.day_rp", "type")]]
        day_rp_last_row <- day_rp_last_row + n_nonpregnant_cows_ai_done
        open_cows[cow_id %in% nonpregnant_cows_ai_done$cow_id,
                  is_to_test_pregnenancy := F]
        # TODO: Set is_to_test_pregnancy of cows which heat comes to F.
      }
    }
  }

  # Pregnancy test
  pregnant_cows <- cows[date_got_pregnant == i - 1 | 
                        date_got_pregnant == i - 2, ]
  n_pregnant_cows <- nrow(pregnant_cows)
  if (n_pregnant_cows != 0) {
    day_rp[day_rp_last_row + (1:n_pregnant_cows),
           `:=`(cow_id = pregnant_cows$cow_id,
                infection_status = pregnant_cows$infection_status,
                day_rp = 15 * (pregnant_cows$day_last_detected_heat <= 15) +
                           30 * (pregnant_cows$day_last_detected_heat > 15),
                type = "pregnancy_test")]
    day_rp_last_row <- day_rp_last_row + n_pregnant_cows
  }

  # Health check after delivery
  delivered_cows <- cows[date_last_delivery == i - 1 |
                         date_last_delivery == i - 2, ]
  n_delivered_cows <- nrow(delivered_cows)
  if (n_delivered_cows != 0) {
    day_rp[day_rp_last_row + (1:n_delivered_cows),
           `:=`(cow_id = delivered_cows$cow_id,
                infection_status = delivered_cows$infection_status,
                day_rp = 15 * (delivered_cows$day_last_detected_heat <= 15) +
                           30 * (delivered_cows$day_last_detected_heat > 15),
                type = "health_check")]
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
    rp_inf_check <- day_rp[1:day_rp_last_row, ]
    rp_inf_check[, i_rp := sample.int(.N), by = .(day_rp, type)]
    rp_inf_check <- rp_inf_check[order(day_rp, type, i_rp), ]
    rp_inf_check[,
                 is_after_inf := (shift(infection_status, type = "lag") != "s"),
                 by = .(day_rp, type)]
    rp_inf_check[infection_status == "s",
                 is_infected := (is_after_inf &
                                   is_infected_rp(.N, param_calculated))]
    cows_inf_rp <- rp_inf_check[is_infected == T, cow_id]
    cows[cow_id %in% cows_inf_rp,
         `:=`(infection_status = "ial",
              date_ial = i,
              cause_infection = "rp")]
    cows[cow_id %in% cows_inf_rp,
         c("date_ipl_expected", "date_ebl_expected") :=
           n_month_to_progress(susceptibility_ial_to_ipl,
                               susceptibility_ipl_to_ebl,
                               i, param_calculated)]
  }

  return(cows)
}


#' Change stage of cows accordinglly
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A list consists of `cow_table` and `tie_stall_table`.
#' @export
change_stage <- function(cows, i, param_calculated) {
  # TODO: 12-23mo is heifer (temporary)
  # Calf to heifer
  cows[age == 12, ':='(stage = "heifer",
                       parity = 0)]

  # Heifer/Dry to milking
  cows[(i - date_got_pregnant) == 10,
       ':='(stage = "milking",
            date_last_delivery = i,
            parity = parity + 1,
            date_got_pregnant = NA,
            day_heat = sample.int(30, .N, replace = T) * 1)]

  # Milking to dry
  cows[stage == "milking" & is_dried(i - date_last_delivery, param_calculated),
       stage := "dry"]

  return(cows)
}


#' Check change of infection status of cows
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param month The month (1, 2, ..., 12) of month `i`.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A [cow_table].
#' @export
change_infection_status <- function(cows, i, month, param_calculated) {
  n_cows <- nrow(cows)

  cows[is_infected_insects(n_cows, month, param_calculated) &
         infection_status == "s",
       ':='(infection_status = "ial",
            date_ial = i,
            cause_infection = "insects"
            )]
  cows[is_infected_needles(n_cows, cows, param_calculated) &
         infection_status == "s",
       ':='(infection_status = "ial",
            date_ial = i,
            cause_infection = "needles")]

  cows[date_ial == i,
       c("date_ipl_expected", "date_ebl_expected") :=
         n_month_to_progress(susceptibility_ial_to_ipl,
                             susceptibility_ipl_to_ebl,
                             i, param_calculated)]
  
  cows[date_ipl_expected == i,
       ":="(infection_status = "ipl",
            date_ipl = i)]
  cows[date_ebl_expected == i,
       ":="(infection_status = "ebl",
            date_ebl = i)]

  return(cows)
}


#' Conduct BLV test
#'
#' @param cows See [cow_table].
#' @param month The month (1, 2, ..., 12).
#' @param param_calculated Return from [calc_param()].
#'
#' @return A [cow_table].
#' @export
do_test <- function(cows, month, param_calculated) {
  if (sum(month == param_calculated$test_months) > 0) {
    n_cow <- nrow(cows)
    is_detected <- cows$infection_status != "s" &
      (cows$is_detected | runif(n_cow) < param_calculated$test_sensitivity)
    is_false_positive <- cows$infection_status == "s" &
      runif(n_cow) > param_calculated$test_specificity
    cows$is_detected <- is_detected | is_false_positive
  }
  return(cows)
}

#' Add newborns to a cow_table
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param i The number of months from the start of the simulation.
#' @param last_cow_id The ID of a cow at the last non-empty row of `cows`.
#' @param param_area See [param_area].
#' @param param_calculated Return from [calc_param()].
#' @param param_processed Return from [process_param()].
#'
#' @return A list consisted of two elements: `cows` and `last_cow_id`.
#' @export
add_newborns <- function(cows, area_table, i, last_cow_id, param_area,
                         param_calculated, param_processed) {
  rows_mothers <- which(cows$date_last_delivery == i)
  # Here, date_last_delivery == i (not i - 12), because date_last_delivery is changed by change_stage().
  # TODO: Temporary delivery interval is set to 12 months.
  # TODO: Consider the functions to change stage and consider delivery could be the same or not.
  n_cows <- sum(!is.na(cows$cow_id))

  if (length(rows_mothers) != 0) {  # If there is any cow delivers in 'month i'
    n_newborns_per_cow <- n_newborn_per_dam(length(rows_mothers),
                                            param_calculated)
    n_newborns <- sum(n_newborns_per_cow)

    newborns <- a_new_calf[rep(1, n_newborns), ]
    newborns[, ':='(id_mother = rep(rows_mothers, n_newborns_per_cow),
                    id_calf = seq_len(n_newborns),
                    n_newborns_per_cow =
                      rep(n_newborns_per_cow, n_newborns_per_cow),
                    status_mother = rep(cows[rows_mothers, infection_status],
                                        n_newborns_per_cow),
                    age = 0,
                    stage = "calf",
                    sex = sex_newborns(n_newborns, param_calculated),
                    is_freemartin = F,
                    date_birth = i,
                    is_owned = T,
                    parity = 0,
                    n_ai = 0,
                    day_heat = sample.int(30, n_newborns, replace = T) * 1,
                    infection_status = "s",
                    area_id = param_area$calf_area_id,
                    months_in_area = 0,
                    is_isolated = attr(area_table, "is_calf_isolated"),
                    i_month = i)]

    # Setting about twins
    if (sum(newborns$n_newborns_per_cow == 2) != 0) {
      newborns[n_newborns_per_cow == 2,
               ':='(sex = sex_twins(.N, param_calculated),
                    is_freemartin = (sex == "freemartin"))]
      newborns[is_freemartin == T, sex := "female"]
    }

    newborns[sex == "male" | is_freemartin == T, is_replacement := F]  # Male calves and freemartin female calves will be sold
    newborns[is.na(is_replacement),
             is_replacement := is_replacement(.N, n_cows, param_processed)]

    # Setting of longevity
    longevity <- longevity(n_newborns, param_calculated)
    newborns[, ':='(date_death_expected = i + longevity$age,
                    cause_removal = longevity$cause)]

    # Susceptibility
    susceptibility <- susceptibility(
      n_newborns,
      rep(cows[rows_mothers, susceptibility_ial_to_ipl], n_newborns_per_cow),
      rep(cows[rows_mothers, susceptibility_ipl_to_ebl], n_newborns_per_cow),
      param_calculated
      )
    newborns[, ':='(susceptibility_ial_to_ipl = susceptibility$ial_to_ipl,
                    susceptibility_ipl_to_ebl = susceptibility$ipl_to_ebl)]

    # Calculation of vertical infection
    newborns[is_infected_vertical(newborns$status_mother, param_calculated),
             ':='(infection_status = "ial",
                  date_ial = i,
                  cause_infection = "vertical"
                  )]
    newborns[is_infected_by_colostrum(status_mother, param_calculated) & 
               infection_status != "s",
             `:=`(infection_status = "ial",
                  date_ial = i,
                  cause_infection = "colostrum")]
    newborns[infection_status != "s",
             c("date_ipl_expected", "date_ebl_expected") :=
               n_month_to_progress(susceptibility_ial_to_ipl,
                                   susceptibility_ipl_to_ebl,
                                   i, param_calculated)]

    # TODO: Simulate failure of delivery (stillbirth/abortion) 妊娠途中で流産する場合についてもどこかで計算しなければ。
    parity_mothers <- cows[newborns$id_mother, parity]
    newborns <- newborns[!is_stillbirth(parity_mothers, param_calculated), ]

    n_newborns_born <- nrow(newborns)
    if (n_newborns_born != 0) {
      rows_newborns <- n_cows + seq_len(n_newborns_born)
      newborns$cow_id <- last_cow_id + seq_len(n_newborns_born)
      last_cow_id <- last_cow_id + n_newborns_born
      cows[rows_newborns, ] <-
        newborns[, c("id_mother", "id_calf", "n_newborns_per_cow",
                     "status_mother", "is_freemartin") := NULL]
    }

  }
  return(list(cows = cows, last_cow_id = last_cow_id))
}


#' Check death and sale of current cows
#'
#' @param cows See [cow_table].
#' @param areas See [tie_stall_table].
#' @param i The number of months from the start of the simulation.
#' @param param_farm See [param_farm].
#' @param param_calculated Return from [calc_param()].
#' @param param_processed Return from [process_param()].
#'
#' @return A list consisted of [cow_table] and [tie_stall_table].
#' @export
check_removal <- function(cows, areas, i, param_farm, param_calculated,
                          param_processed) {
  # Removal by death
  rows_removed_death <- which(cows$date_death_expected == i)
  cows[rows_removed_death, ':='(is_owned = F,
                                date_death = i,
                                cause_removal =
                                  fifelse(cause_removal == "will_die",
                                          "died", "slaughtered"))]

  # Removal by culling
  cows <- cull_infected_cows(cows, i, param_farm, param_calculated)

  # Removal by selling
  rows_removed_sold <- which(cows$is_replacement == F &
                               cows$date_death_expected != i)
  cows[rows_removed_sold, ':='(is_owned = F,
                               cause_removal = "sold")]
  # TODO: とりあえず後継牛以外は0ヶ月齢で売却

  # Removal by detection of EBL
  rows_new_ebl <- which(cows$date_ebl == i)
  rows_removed_ebl <-
    rows_new_ebl[is_ebl_detected(rows_new_ebl, param_calculated)]
  if (length(rows_removed_ebl) != 0) {
    cows[rows_removed_ebl,  ':='(is_owned = F,
                                 cause_removal = "culled")]
  }
  rows_overlooked <- setdiff(rows_new_ebl, rows_removed_ebl)
  if (length(rows_overlooked) != 0) {
    month_ebl_die <-
      n_month_until_ebl_die(rows_overlooked, param_calculated) + i
    cows[seq_len(nrow(cows)) %in% rows_overlooked,
         ':='(date_death_expected =
                fifelse(date_death_expected >= month_ebl_die,
                        month_ebl_die, date_death_expected),
              cause_removal =
                fifelse(date_death_expected >= month_ebl_die,
                        "will_die", cause_removal))]
  }  # TODO: ここテスト

  rows_removed <- c(rows_removed_death, rows_removed_sold, rows_removed_ebl)
  areas_removed <- cows[rows_removed, area_id]
  for (area in param_processed$ts_area) {
    rows_removed_this_area <- rows_removed[areas_removed == area]
    areas[[area]] <-
      remove_from_area(areas[[area]], cows[rows_removed_this_area, cow_id])
  }

  return(list(cows = cows, areas = areas))
}


#' Remove dead/culled/sold cows from `area_list`
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param area_list See [setup_areas] and [tie_stall_table].
#' @param i The number of months from the start of the simulation.
unassign_removed_cows <- function(cows, area_table, area_list, i) {
  removed_cow_id <- cows$cow_id[cows$date_death == i]
  for (i_area in attr(area_table, "tie_stall")) {
    area_list[[i_area]] <-
      area_list[[i_area]][cow_id %in% removed_cow_id, area_id := NA_integer_]
  }
  return(cows)
}


#' Assign `chamber_id` to newborns
#'
#' @param cows See [cow_table].
#' @param area_table See [area_table].
#' @param area_list See [setup_areas] and [tie_stall_table].
#' @param param_area See [param_area].
assign_newborns <- function(cows, area_table, area_list, param_area) {
  newborn_cow_id <- cows$cow_id[cows$age == 0]
  n_newborn <- length(newborn_cow_id)
  if (all(attr(area_table, "tie_stall") != param_area$calf_area_id) |
      n_newborn == 0) {
    return(list(cows = cows, area_list = area_list))
  }

  calf_area <- area_list[[as.character(param_area$calf_area_id)]]
  is_empty_chambers <- is.na(calf_area$cow_id)
  n_empty_chambers <- sum(is_empty_chambers)
  if (n_empty_chambers < n_newborn) {
    newborn_can_be_assigned <- sample.int(newborn_cow_id, n_empty_chambers)
  } else {
    newborn_can_be_assigned <- newborn_cow_id
  }

  assigned_chambers <- resample(calf_area$chamber_id[is_empty_chambers],
                                length(newborn_can_be_assigned))
  cows[match(newborn_can_be_assigned, cow_id), chamber_id := assigned_chambers]
  area_list[[as.character(param_area$calf_area_id)]][
    assigned_chambers, cow_id := newborn_can_be_assigned]

  return(list(cows = cows, area_list = area_list))
}


#' Cull infected cows
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param param_farm See [param_farm].
#' @param param_calculated Return from [calc_param()].
#'
#' @return A [cow_table].
#' @export
cull_infected_cows <- function(cows, i, param_farm, param_calculated) {
  if (param_farm$cull_infected_cows != "no") {
    id_detected_highrisk <-
      cows[(infection_status == "pl" | infection_status == "ebl") &
           is_detected & is_owned,
           cow_id]
    cows <- replace_selected_cows(cows, id_detected_highrisk, i)
  }
  if (param_farm$cull_infected_cows == "all") {
    id_detected <- cows[is_detected & is_owned, cow_id]
    cows <- replace_selected_cows(cows, id_detected, i)
  }
  return(cows)
}


#' Replace selected cows
#'
#' @param cows See [cow_table].
#' @param cow_id_to_cull `cow_id` to remove from `cows`.
#' @param i The number of months from the start of the simulation.
#' 
#' @return A [cow_table].
#' @export
replace_selected_cows <- function(cows, cow_id_to_cull, i) {
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
    cows[cow_id %in% id_culled,
         `:=`(is_owned = F,
              date_death = i,
              cause_removal = "culled")]
    cows$is_replacement[cows$cow_id %in% id_replaced] <- T
  }
  return(cows)
}


#' Set the variable i_month in a cow_table
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#'
#' @return A [cow_table].
#' @export
set_i_month <- function(cows, i) {
  cows[, i_month := i]
  return(cows)
}


#' Extract owned cows from a cow_table
#'
#' @param cows See [cow_table].
#'
#' @return A [cow_table].
#' @export
extract_owned_cows <- function(cows) {
  cows <- cows[is_owned == T | is.na(is_owned), ]
  return(cows)
}


#' Check and move cows between areas
#'
#' @param cows See [cow_table].
#' @param movement_table See [movement_table].
#' @param area_table See [area_table].
#' @param area_list See [setup_areas] and [tie_stall_table].
#' @param param_area See [param_area].
#' @param i The number of months from the start of the simulation.
#'
#' @return A list composed of [cow_table] and [area_list].
#' @export
change_area <- function(cows, movement_table, area_table, area_list,
                        param_area, i) {
  # area_tableに沿って、移動する個体、合致したconditionを抽出
  # とりあえず全て移動させて、移動できなかった個体はchamber_idを決めない

  cows <- unassign_removed_cows(cows, area_table, area_list, i)
  res <- assign_newborns(cows, area_table, area_list, param_area)
  cows <- res$cows
  area_list <- res$area_list

  # Extract cows whose area must be changed
  cow_id_met_condition <- lapply(
    movement_table$condition,
    function(x) {cows[eval(parse(text = x)) & is_owned, cow_id]}
    )

  # Remove duplicated cow_id
  duplicated_cow_id <-
    relist(!duplicated(flatten_dbl(cow_id_met_condition)), cow_id_met_condition)
  cow_id_to_move <- mapply(function(x, y) {x[y]},
                           cow_id_met_condition, duplicated_cow_id,
                           SIMPLIFY = FALSE)

  # Remove cows to move from n_cows
  n_cows_in_each_area <-
    table(factor(cows[(is_owned), area_id], levels = area_table$area_id))
  n_cows_to_move_by_each_condition <- sapply(cow_id_to_move, length)
  n_cows_to_move_in_each_area <- tapply(
    n_cows_to_move_by_each_condition,
    attr(movement_table, "factored_current_area"),
    sum)
  empty_spaces <- attr(area_table, "capacity") - n_cows_in_each_area +
    n_cows_to_move_in_each_area

  # Remove cows from areas
  vec_cows_to_move <- flatten_dbl(cow_id_met_condition)
  res <- remove_from_areas(cows, area_list, area_table, vec_cows_to_move)
  cows <- res$cows
  area_list <- res$area_list
  cow_id_allocated_to_full_areas <- numeric(sum(cows$is_owned, na.rm = T))
  cow_id_allocated_to_full_areas_index <- 0

  # Decide to which next_area cows will move
  for (i_movement in seq_len(nrow(movement_table))) {
    i_cow_id <- resample(cow_id_to_move[[i_movement]])
    # sample() here must not be replaced with sample.int() because the latter
    # causes error when the length of x is 0.
    # Order of cow_id is randomized to decide cow_id_allocated_to_full_areas
    if (attr(movement_table, "is_priority_specified_by_integer")[i_movement]) {
      # For conditions with priorities specified by integers

      i_next_area <- movement_table$next_area[[i_movement]]
      chr_i_next_area <- as.character(i_next_area)
      empty_spaces_in_next_areas <- empty_spaces[chr_i_next_area]
      allocated_area_index <-
        findInterval(seq_along(i_cow_id),
                     c(0, cumsum(empty_spaces_in_next_areas)), left.open = T)
      allocated_areas <- i_next_area[allocated_area_index]
      empty_spaces[chr_i_next_area] <-
        table(factor(allocated_areas, levels = chr_i_next_area))

      # When length(i_cow_id) is larger than sum(empty_spaces_in_next_area),
      # allocated_area_index includes NA.
      # Then allocate such cows into full areas according to capacity.
      if (anyNA(allocated_areas)) {
        capacity_of_next_areas <- attr(area_table, "capacity")[chr_i_next_area]
        is_na_allocated_areas <- is.na(allocated_areas)
        n_na_allocated_areas <- sum(is_na_allocated_areas)
        allocated_areas[which(is_na_allocated_areas)] <-
          resample(i_next_area, n_na_allocated_areas, replace = T,
                   prob = capacity_of_next_areas)
        cow_id_allocated_to_full_areas[
          cow_id_allocated_to_full_areas_index + seq_len(n_na_allocated_areas)
          ] <- i_cow_id[is_na_allocated_areas]
        cow_id_allocated_to_full_areas_index <-
          cow_id_allocated_to_full_areas_index + n_na_allocated_areas
      }
    } else {
      # B. For conditions with priorities specified by real numbers

      i_next_area <- movement_table$next_area[[i_movement]]
      chr_i_next_area <- as.character(i_next_area)
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
          ] <- cow_id[seq_len(n_cows_allocated_to_full_areas)]
        cow_id_allocated_to_full_areas_index <-
          cow_id_allocated_to_full_areas_index + n_cows_allocated_to_full_areas

        capacity_of_areas <- attr(area_table, "capacity")[i_next_area]
        allocated_areas <- c(rep(i_next_area, vacancy),
          resample(i_next_area, n_cows_allocated_to_full_areas, replace = T,
                   prob = capacity_of_areas))
      }
    }
    cows[match(i_cow_id, cow_id), area_id := allocated_areas]
  }
  cow_id_allocated_to_full_areas <-
    cow_id_allocated_to_full_areas[cow_id_allocated_to_full_areas != 0]
  # a[!a %in% b] is 5x faster than setdiff()
  cow_id_to_allocate_chambers <-
    vec_cows_to_move[!vec_cows_to_move %in% cow_id_allocated_to_full_areas]
  cows_to_allocate_chambers <-
    calculate_area_assignment(cows, area_table, cow_id_to_allocate_chambers)
  cows <- assign_chambers(cows, area_list, cows_to_allocate_chambers)
  area_list <- assign_cows(cows, area_list, cows_to_allocate_chambers)
  return(list(cows = cows, area_list = area_list))
}
# TODO: Make a function to setup tie_stall_table.

# TODO: tie-stallのAreaに割り当てられているがchamber_idの決まってない牛にchamber_idを割り振るためのfunction

