## ---- add_1_to_age

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


## ---- do_ai

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
  # TODO: あまりに長いので適度に分割したい
  day_rp_last_row <- 0

  rows_h <- which(cows$stage == "heifer" & cows$n_ai == 0 &
                    is.na(cows$date_got_pregnant))
  rows_h_ai_started <-
    rows_h[is_ai_started_heifer(cows[rows_h, age], param_calculated)]
  rows_m <- which(cows$stage == "milking" & cows$n_ai == 0 &
                    is.na(cows$date_got_pregnant))
  rows_m_ai_started <- rows_m[
    is_ai_started_milking(i - cows[rows_m, date_last_delivery],
                          param_calculated)
    ]
  rows_ai_started <- c(rows_h_ai_started, rows_m_ai_started)

  # 初回授精 (初回授精開始日と一致させるためには、この月の授精は必ず発見されなくてはならないので、初回以降と別にしている)
  if (length(rows_ai_started) != 0) {
    cows_first_ai <- cows[rows_ai_started, .(cow_id, infection_status, day_heat)]
    n_cows_first_ai <- nrow(cows_first_ai)

    cows_first_ai[,
                  ':='(day_heat1 = day_heat,
                       day_next_heat = day_heat +
                         heat_cycle(n_cows_first_ai, param_calculated))]
    cows_first_ai[day_next_heat <= 30, day_heat2 := day_next_heat]
    cows_first_ai[day_next_heat <= 30,
                  day_next_heat := day_next_heat +
                    heat_cycle(.N, param_calculated)]
    cows_first_ai[, ':='(day_next_heat = ifelse(day_next_heat - 30 > 0,
                                                day_next_heat - 30, 1),
                         is_heat1_detected =
                           is_heat_detected(n_cows_first_ai, param_calculated),
                         is_heat2_detected =
                           is_heat_detected(n_cows_first_ai, param_calculated))]
    cows_first_ai[, is_ai1_successed := ifelse(
                      is_heat1_detected,
                      is_first_ai_successed(n_cows_first_ai, param_calculated),
                      F)]
    cows_first_ai[is_ai1_successed == T, ':='(day_heat2 = NA_real_,
                                              day_next_heat = 0)]
    cows_first_ai[, n_heat := 1 + !is.na(day_heat2)]
    cows_first_ai[n_heat == 1, is_heat2_detected := NA]

    cows_first_ai[is_heat2_detected == T,
                  is_ai2_successed :=
                    ifelse(is_heat1_detected,
                           is_ai_successed(.N, param_calculated),
                           is_first_ai_successed(.N, param_calculated))]

    cows_first_ai[, n_ai := ifelse(n_heat == 1,
                                   is_heat1_detected * 1,
                                   is_heat1_detected + is_heat2_detected)]
    while (sum(cows_first_ai$n_ai == 0) != 0) {
      cows_first_ai[n_ai == 0,
                    is_heat1_detected :=
                      is_heat_detected(.N, param_calculated)]
      cows_first_ai[n_ai == 0,
                    is_ai1_successed :=
                      ifelse(is_heat1_detected,
                             is_first_ai_successed(.N, param_calculated), F)]
      cows_first_ai[n_ai == 0 & is_ai1_successed == T,
                    ':='(day_heat2 = NA_real_,
                         day_next_heat = NA_real_)]
                         # day_next_heat = 0)]
      cows_first_ai[n_ai == 0, n_heat := 1 + !is.na(day_heat2)]

      cows_first_ai[n_ai == 0 & n_heat == 2,
                    is_heat2_detected :=
                      is_heat_detected(.N, param_calculated)]
      cows_first_ai[n_ai == 0 & is_heat2_detected == T,
                    is_ai2_successed :=
                      ifelse(is_heat1_detected,
                             is_ai_successed(.N, param_calculated),
                             is_first_ai_successed(.N, param_calculated))]
      cows_first_ai[, n_ai := ifelse(n_heat == 1,
                                     is_heat1_detected * 1,
                                     is_heat1_detected + is_heat2_detected)]
    }

    cows_first_ai[, ':='(day_last_heat = ifelse(is.na(day_heat2),
                                                day_heat1, day_heat2),
                         day_last_heat_detected =
                           ifelse(n_heat == 1,
                                  day_heat1,
                                  ifelse(is_heat2_detected,
                                         day_heat2, day_heat1)),
                         n_heat_from_ai =
                           ifelse(n_heat == 2 & !is_heat2_detected, 1, 0),
                         is_pregnant = (is_ai1_successed | is_ai2_successed))]
    cows_first_ai[is.na(is_pregnant), is_pregnant := F]

    cows[rows_ai_started, ':='(n_ai = cows_first_ai$n_ai * 1,
                               day_heat = cows_first_ai$day_next_heat,
                               day_last_heat = cows_first_ai$day_last_heat,
                               day_last_heat_detected =
                                 cows_first_ai$day_last_heat_detected,
                               n_heat_from_ai = cows_first_ai$n_heat_from_ai)]
    # TODO: リピートブリーダーは処分するかもしれないのでとりあえず授精回数を記録しておく。

    cows[rows_ai_started[cows_first_ai$is_pregnant],
         ':='(date_got_pregnant = i,
              n_ai = 0)]

    cows_first_ai_melt <- melt.data.table(cows_first_ai,
                            measure.vars = c("day_heat1", "day_heat2"),
                            variable.name = "i_heat", value.name = "day")
    cows_first_ai_melt <-
      cows_first_ai_melt[!is.na(day) &
                          !(is_heat1_detected == F & i_heat == "day_heat1") &
                          !(is_heat2_detected == F & i_heat == "day_heat2"), ]

    n_ai_done <- nrow(cows_first_ai_melt)
    if (n_ai_done != 0) {
      day_rp[1:n_ai_done,
             ':='(cow_id = cows_first_ai_melt$cow_id,
                  infection_status = cows_first_ai_melt$infection_status,
                  day_rp = cows_first_ai_melt$day,
                  type = sample(c("ai_am", "ai_pm"), n_ai_done, replace = T))]
      day_rp_last_row <- n_ai_done
    }
  }

  # 初回以降の授精
  rows_open <- which(cows$n_ai != 0 & is.na(cows$date_got_pregnant))

  if (length(rows_open) != 0) {
    cows_ai <- cows[rows_open,
                    .(cow_id, infection_status, n_ai, day_heat,
                      day_last_heat_detected, n_heat_from_ai)]
    n_cows_ai <- nrow(cows_ai)

    # 妊娠していないが、人工授精直後の発情を見逃したため妊娠鑑定される牛 その1
    cows_ai_done <- cows_ai[n_heat_from_ai == 1, ]
    n_cow_heat_missed <- nrow(cows_ai_done)
    if (n_cow_heat_missed != 0) {
      day_rp[day_rp_last_row + (1:n_cow_heat_missed),
             ':='(cow_id = cows_ai_done$cow_id,
                  infection_status = cows_ai_done$infection_status,
                  day_rp = 15 * (cows_ai_done$day_last_heat_detected <= 15) +
                             30 * (cows_ai_done$day_last_heat_detected > 15),
                  type = "missed_heat")]
      day_rp_last_row <- day_rp_last_row + n_cow_heat_missed
    }

    cows_ai[, ':='(day_heat1 = day_heat,
                   day_next_heat = day_heat +
                     heat_cycle(n_cows_ai, param_calculated))]
    cows_ai[day_next_heat <= 30, day_heat2 := day_next_heat]
    cows_ai[day_next_heat <= 30,
            day_next_heat := day_next_heat + heat_cycle(.N, param_calculated)]
    cows_ai[, ':='(day_next_heat = ifelse(day_next_heat - 30 > 0,
                                          day_next_heat - 30, 1),
                   is_heat1_detected =
                     is_heat_detected(n_cows_ai, param_calculated),
                   is_heat2_detected =
                     is_heat_detected(n_cows_ai, param_calculated))]
    cows_ai[is_heat1_detected == T,
            is_ai1_successed := is_ai_successed(.N, param_calculated)]

    cows_ai[is_ai1_successed == T, ':='(day_heat2 = NA_real_,
                                        day_next_heat = NA_real_)]
    cows_ai[, n_heat := 1 + !is.na(day_heat2)]
    cows_ai[n_heat == 1, is_heat2_detected := NA]

    cows_ai[is_heat2_detected == T,
            is_ai2_successed := is_ai_successed(.N, param_calculated)]
    cows_ai[, n_ai := n_ai + is_heat1_detected +
              ifelse(n_heat == 1, 0, is_heat2_detected)]

    cows_ai[, n_heat_from_ai := (n_heat_from_ai + 1) * !is_heat1_detected]

    # 妊娠していないが、人工授精直後の発情を見逃したため妊娠鑑定される牛 その2
    cows_ai_done <- cows_ai[n_heat_from_ai == 1, ]
    n_cow_heat_missed <- nrow(cows_ai_done)
    if (n_cow_heat_missed != 0) {
      day_rp[day_rp_last_row + (1:n_cow_heat_missed),
        ':='(cow_id = cows_ai_done$cow_id,
             infection_status = cows_ai_done$infection_status,
             day_rp = 15 * (cows_ai_done$day_last_heat_detected <= 15) +
                      30 * (cows_ai_done$day_last_heat_detected > 15),
             type = "missed_heat")]
      day_rp_last_row <- day_rp_last_row + n_cow_heat_missed
    }
    cows_ai[!is.na(day_heat2),
            n_heat_from_ai := (n_heat_from_ai + 1) * (!is_heat2_detected)]

    cows_ai[, ':='(day_last_heat = ifelse(is.na(day_heat2),
                                          day_heat1, day_heat2),
                   day_last_heat_detected =
                     ifelse(is_heat2_detected & n_heat == 2,
                            day_heat2,
                            ifelse(is_heat1_detected,
                                   day_heat1, day_last_heat_detected)),
                   is_pregnant = (is_ai1_successed | is_ai2_successed))]
    cows_ai[is.na(is_pregnant), is_pregnant := F]

    cows[rows_open, ':='(n_ai = cows_ai$n_ai,
                         day_heat = cows_ai$day_next_heat,
                         day_last_heat = cows_ai$day_last_heat,
                         day_last_heat_detected =
                           cows_ai$day_last_heat_detected,
                         n_heat_from_ai = cows_ai$n_heat_from_ai)]
    # TODO: リピートブリーダーは処分するかもしれないのでとりあえず授精回数を記録しておく。

    cows[rows_open[cows_ai$is_pregnant], ':='(date_got_pregnant = i,
                                              n_ai = 0)]

    cows_ai_melt <- melt.data.table(cows_ai,
                      measure.vars = c("day_heat1", "day_heat2"),
                      variable.name = "i_heat", value.name = "day")
    cows_ai_melt <-
      cows_ai_melt[!is.na(day) &
                   !(is_heat1_detected == F & i_heat == "day_heat1") &
                   !(is_heat2_detected == F & i_heat == "day_heat2"), ]

    n_ai_done <- nrow(cows_ai_melt)
    if (n_ai_done != 0) {
      day_rp[day_rp_last_row + (1:n_ai_done),
        ':='(cow_id = cows_ai_melt$cow_id,
             infection_status = cows_ai_melt$infection_status,
             day_rp = cows_ai_melt$day,
             type = sample(c("ai_am", "ai_pm"), n_ai_done, replace = T))]
      day_rp_last_row <- day_rp_last_row + n_ai_done
    }

  }

  # 妊娠鑑定
  rows_pregnant <- which(cows$date_got_pregnant == i - 1 |
                           cows$date_got_pregnant == i - 2)
  cows_pregnant <- cows[rows_pregnant, ]
  n_pregnant <- nrow(cows_pregnant)
  if (n_pregnant != 0) {
    day_rp[day_rp_last_row + (1:n_pregnant),
           ':='(cow_id = cows_pregnant$cow_id,
                infection_status = cows_pregnant$infection_status,
                day_rp =
                 15 * (cows_pregnant$day_last_heat <= 15) +
                 30 * (cows_pregnant$day_last_heat > 15),
                type = "pregnant_diagnosis")]
    day_rp_last_row <- day_rp_last_row + n_pregnant
  }


  # 繁殖検診
  rows_delivered <- which(cows$date_last_delivery == i - 1 |
                           cows$date_last_delivery == i - 2)
  cows_delivered <- cows[rows_delivered, ]
  n_delivered <- nrow(cows_delivered)
  if (n_delivered != 0) {
    day_rp[day_rp_last_row + (1:n_delivered),
           ':='(cow_id = cows_delivered$cow_id,
                infection_status = cows_delivered$infection_status,
                day_rp = 15 * (cows_delivered$day_last_heat <= 15) +
                           30 * (cows_delivered$day_last_heat > 15),
                type = "health_check")]
    day_rp_last_row <- day_rp_last_row + n_delivered
  }

  # 水平感染の判定
  # 直検を「分娩後検診」「人工授精（午前）」「人工授精（午後）」「妊娠鑑定」の4グループに分ける。
  # 分娩後検診と妊娠鑑定は月二回(15日・30日)、人工授精は毎日行う。
  # 各日・各グループにおいて感染牛の直後に直検が行われた牛は感染リスクがあるとする。
  # (最初は直後でなくてもよいとしていたが、明らかに直検による感染が多すぎたので修正)

  if (day_rp_last_row != 0) {
    rp_inf_check <- one_day_rp[rep(1, day_rp_last_row), ]
    rp_inf_check[, i_rp := sample.int(.N), by = .(day_rp, type)]
    rp_inf_check <- rp_inf_check[order(day_rp, type, i_rp), ]
    rp_inf_check[,
                 is_after_inf := (shift(infection_status, type = "lag") != "s"),
                 by = .(day_rp, type)]
    rp_inf_check[infection_status == "s",
                 is_infected := (is_after_inf &
                                   is_infected_rp(.N, param_calculated))]
    cows_inf_rp <- rp_inf_check[is_infected == T, cow_id]
    rows_inf_rp <- which(cows$cow_id %in% cows_inf_rp)
    cows[rows_inf_rp,
         ':='(infection_status = "ial",
              date_ial = i,
              cause_infection = "rp")]
    cows[rows_inf_rp,
         c("date_ipl_expected", "date_ebl_expected") :=
           n_month_to_progress(susceptibility_ial_to_ipl,
                               susceptibility_ipl_to_ebl,
                               i, param_calculated)]
  }

  return(cows)
}


## ---- change_stage

#' Change stage of cows and move cows between barns accordinglly
#'
#' @param cows See [cow_table].
#' @param groups See [barn_table].
#' @param i The number of months from the start of the simulation.
#' @param param_group See [param_group].
#' @param param_calculated Return from [calc_param()].
#' @param param_processed Return from [process_param()].
#'
#' @return A list consists of `cow_table` and `barn_table`.
#' @export
change_stage <- function(cows, groups, i, param_group, param_calculated,
                         param_processed) {
  # TODO: groups と barns が入り混じってるのでbarnsに統一したい

  # TODO: とりあえず12-23がheifer
  param_group_id <- 1:(param_group$n_group)

  # Calf to heifer
  row_c2h <- which(cows$age == 12)
  cows[row_c2h, ':='(stage = "heifer",
                     parity = 0,
                     group_id = param_group_id[2])]
  if (param_processed$is_ts[1]) {
    groups[[1]] <- remove_from_group(groups[[1]], cows[row_c2h, cow_id])
  }
  if (param_processed$is_ts[2]) {
    groups[[param_group_id[2]]] <-
      find_empty_chamber(groups[[param_group_id[2]]], cows[row_c2h, ])
  }
    # TODO: 牛舎が満杯状態のときも考慮しないと
    # TODO: 牛舎内で牛の位置を変えることも考慮しなければ。（例えば同一牛舎内で搾乳牛エリアと乾乳牛エリアを分けているなど）

  # Heifer to milking
  row_h2m <- which(cows$stage == "heifer" & (i - cows$date_got_pregnant) == 10)
  cows[row_h2m, ':='(stage = "milking",
                     date_last_delivery = i,
                     parity = 1,
                     date_got_pregnant = NA,
                     group_id = param_group_id[3],
                     day_heat = sample.int(30, .N, replace = T) * 1,
                     day_last_heat = sample.int(30, .N, replace = T) * 1)]
  if (param_processed$is_ts[2]) {
    # TODO: 同上。これ同じことやってるので別のfunctionを作りたい
    groups[[param_group_id[2]]] <-
      remove_from_group(groups[[param_group_id[2]]], cows[row_h2m, cow_id])
  }
  if (param_processed$is_ts[3]) {
    groups[[param_group_id[3]]] <-
      find_empty_chamber(groups[[param_group_id[3]]], cows[row_h2m, ])
  }

  # Dry to milking
  row_d2m <- which((i - cows$date_got_pregnant) == 10)
  cows[row_d2m, ':='(stage = "milking",
                     date_last_delivery = i,
                     parity = parity + 1,
                     date_got_pregnant = NA,
                     group_id = param_group_id[3],
                     day_heat = sample.int(30, .N, replace = T) * 1,
                     day_last_heat = sample.int(30, .N, replace = T) * 1)]
  if (param_processed$is_ts[4]) {
    # TODO: 同上
    # DryとMilkingが同じところで飼われてるなら、牛舎内移動だけですむはず
    groups[[param_group_id[4]]] <-
      remove_from_group(groups[[param_group_id[4]]], cows[row_d2m, cow_id])
  }
  if (param_processed$is_ts[3]) {
    groups[[param_group_id[3]]] <-
      find_empty_chamber(groups[[param_group_id[3]]], cows[row_d2m, ])
  }

  # Milking to dry
  row_m2d <- which(cows$stage == "milking" &
                     is_dried(i - cows$date_last_delivery, param_calculated))
  cows[row_m2d, ':='(stage = "dry",
                     group_id = param_group_id[4])]

  if (param_processed$is_ts[3]) {
    # TODO: 同上
    groups[[param_group_id[3]]] <-
      remove_from_group(groups[[param_group_id[3]]], cows[row_m2d, cow_id])
  }
  if (param_processed$is_ts[4]) {
    groups[[param_group_id[4]]] <-
      find_empty_chamber(groups[[param_group_id[4]]], cows[row_m2d, ])
  }
  # TODO: このタイミングで廃用になる牛がいるだろうので考慮

  return(list(cows = cows, groups = groups))
}



## ---- change_infection_status

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

  rows_s <- which(cows$infection_status == "s")

  is_inf_insects <- is_infected_insects(rows_s, month, param_calculated)
  rows_inf_insects <-
    rows_s[is_infected_insects(rows_s, month, param_calculated)]
  rows_inf_needles <-
    rows_s[!(is_inf_insects) & is_infected_needles(rows_s, param_calculated)]
  if (length(rows_inf_insects) != 0) {
    cows[rows_inf_insects,
         ':='(infection_status = "ial",
              date_ial = i,
              cause_infection = "insects"
              )]
    cows[rows_inf_insects,
         c("date_ipl_expected", "date_ebl_expected") :=
           n_month_to_progress(susceptibility_ial_to_ipl,
                               susceptibility_ipl_to_ebl,
                               i, param_calculated)]
  }
  if (length(rows_inf_needles) != 0) {
    cows[rows_inf_needles,
         ':='(infection_status = "ial",
              date_ial = i,
              cause_infection = "needles")]
    cows[rows_inf_needles,
         c("date_ipl_expected", "date_ebl_expected") :=
           n_month_to_progress(susceptibility_ial_to_ipl,
                               susceptibility_ipl_to_ebl,
                               i, param_calculated)]
  }

  rows_new_ipl <- which(cows$date_ipl_expected == i)
  if (length(rows_new_ipl) != 0) {
    cows[rows_new_ipl,
         ":="(infection_status = "ipl",
              date_ipl = i)]
  }

  rows_new_ebl <- which(cows$date_ebl_expected == i)
  if (length(rows_new_ebl) != 0) {
    cows[rows_new_ebl,
         ":="(infection_status = "ebl",
              date_ebl = i)]
  }

  return(cows)
}



## ---- add_newborns
#' Add newborns to a cow_table
#'
#' @param cows See [cow_table].
#' @param i The number of months from the start of the simulation.
#' @param last_cow_id The ID of a cow at the last non-empty row of `cows`.
#' @param param_calculated Return from [calc_param()].
#' @param param_processed Return from [process_param()].
#'
#' @return A list consisted of two elements: `cows` and `last_cow_id`.
#' @export
add_newborns <- function(cows, i, last_cow_id, param_calculated,
                         param_processed) {
  rows_mothers <- which(cows$date_last_delivery == i)  # Here, date_last_delivery == i (not i - 12), because date_last_delivery is changed by change_stage().
  # TODO: とりあえず12ヶ月ごとに出産するとする。stageを変える関数と出産を決める関数を別にしていいのかもあとで考える。
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
                    is_introduced = F,
                    is_in_common_ranch = F,
                    is_grazed = F,  # TODO: とりあえず放牧はしていないことにする
                    parity = 0,
                    n_ai = 0,
                    day_heat = sample.int(30, n_newborns, replace = T) * 1,
                    infection_status = "s")]

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
    rows_infected_vertical <-
      is_infected_vertical(newborns$status_mother, param_calculated)
    newborns[rows_infected_vertical,
             ':='(infection_status = "ial",
                  date_ial = i,
                  cause_infection = "vertical"
                  )]
    newborns[rows_infected_vertical,
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
      newborns[, cow_id := last_cow_id + seq_len(n_newborns_born)]
      last_cow_id <- last_cow_id + n_newborns_born
      # Here, last_cow_id instead of max(cows$cow_id, na.rm = T) is used,
      # because these two values are different when the last newborn died already and no calve was born since then.
      cows[rows_newborns, ] <-
        newborns[, c("id_mother", "id_calf", "n_newborns_per_cow",
                     "status_mother", "is_freemartin") := NULL]
    }

  }
  return(list(cows = cows, last_cow_id = last_cow_id))
}



## ---- check_removal
#' Check death and sale of current cows
#'
#' @param cows See [cow_table].
#' @param groups See [barn_table].
#' @param i The number of months from the start of the simulation.
#' @param param_calculated Return from [calc_param()].
#' @param param_processed Return from [process_param()].
#'
#' @return A list consisted of [cow_table] and [barn_table].
#' @export
check_removal <- function(cows, groups, i, param_calculated, param_processed) {
  # Removal by death
  rows_removed_death <- which(cows$date_death_expected == i)
  cows[rows_removed_death, ':='(is_owned = F,
                                date_death = i,
                                cause_removal =
                                  ifelse(cause_removal == "will_die",
                                         "dead", "slaughtered"))]

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
                                 cause_removal = "ebl")]
  }
  rows_overlooked <- setdiff(rows_new_ebl, rows_removed_ebl)
  if (length(rows_overlooked) != 0) {
    n_month_until_ebl_die <-
      n_month_until_ebl_die(rows_overlooked, param_calculated)
    cows[seq_len(nrow(cows)) %in% rows_overlooked &
         date_death_expected >= i + n_month_until_ebl_die,
         ':='(date_death_expected = i + n_month_until_ebl_die,
              cause_removal = "will_die")]
  }  # TODO: ここテスト

  rows_removed <- c(rows_removed_death, rows_removed_sold, rows_removed_ebl)
  groups_removed <- cows[rows_removed, group_id]
  for (group in param_processed$ts_group) {
    rows_removed_this_group <- rows_removed[groups_removed == group]
    groups[[group]] <-
      remove_from_group(groups[[group]], cows[rows_removed_this_group, cow_id])
  }

  return(list(cows = cows, groups = groups))
}



## ---- set_i_month
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


## ---- extract_owned_cows
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
