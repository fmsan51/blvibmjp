#' Calculate months to progress the disease
#'
#' When a cow is infected, the number of months necessary to progress the disease is calculated at the month a cow is infected.
#'
#' Probabilities to:
#' - be newly infected (s->ial),
#' - develop persistent lymphocytosis (ial->ipl), and
#' - show symptom (ipl->ebl),
#' in the month.
#'
#' Total infection risk = Infection risk in free-stall or grazing
#'   * (hours_grazing / 24)
#'   + Infection risk from neighborhood cows * (1 - hours_grazing / 24)
#'
#' @param susceptibility_ial_to_ipl A numeric.
#' @param susceptibility_ipl_to_ebl A numeric.
#' @param i The number of months from the start of a simulation.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A list consisted of a month.
#' @export
n_month_to_progress <- function(susceptibility_ial_to_ipl,
                                susceptibility_ipl_to_ebl,
                                i,
                                param_calculated) {
  # TODO: ここsusceptibility...って結局何してるんだ？
  n_cows <- length(susceptibility_ial_to_ipl)
  months_ial_to_ebl <- prop_ial_period <- months_ial_to_ipl <- numeric(n_cows)
  neg_months_ial_to_ipl <- rep(T, n_cows)
  while (any(neg_months_ial_to_ipl)) {
    months_ial_to_ebl[neg_months_ial_to_ipl] <- rweibull(
                           n_cows,
                           shape = param_calculated$ebl_progress_shape,
                           scale = param_calculated$ebl_progress_scale
                         ) * 12
    prop_ial_period[neg_months_ial_to_ipl] <- rnorm(n_cows,
                             mean = param_calculated$mean_prop_ial_period,
                             sd = param_calculated$sd_prop_ial_period)
    prop_ial_period[prop_ial_period < 0] <- 0
    months_ial_to_ipl[neg_months_ial_to_ipl] <- rweibull(
                           n_cows,
                           shape = param_calculated$ebl_progress_shape,
                           scale = param_calculated$ebl_progress_scale *
                             prop_ial_period
                         ) * 12
    neg_months_ial_to_ipl <- months_ial_to_ipl < 0
  }
  months_ial_to_ebl <- ceiling(months_ial_to_ebl)
  months_ial_to_ipl <- ceiling(months_ial_to_ipl)
  # シミュレーションには「その月に起きた出来事」を反映する方針 (＝月の最終日におけるステータス) なのでroundではなくceiling
  # TODO: 他のroundを使ってる部分について、本当にそれでいいか考慮
  months_ipl_to_ebl <- months_ial_to_ebl - months_ial_to_ipl
  months_ial_to_ipl[!susceptibility_ial_to_ipl] <- NA_real_
  months_ipl_to_ebl[!susceptibility_ipl_to_ebl] <- NA_real_
  months <- list(ial_to_ipl = months_ial_to_ipl,
                 ipl_to_ebl = months_ipl_to_ebl)
  return(months)
}
# TODO: test


#' Whether a EBL cow is detected
#'
#' @param id_cow_ebl The ID of EBL cows.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_ebl_detected <- function(id_cow_ebl, param_calculated) {
  runif(length(id_cow_ebl)) < param_calculated$prob_ebl_detected
}


#' The number of months until EBL cows die
#'
#' @param rows_cow_overlooked The row IDs of EBL-yet-non-detected cows.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A numeric vector.
n_month_until_ebl_die <- function(rows_cow_overlooked, param_calculated) {
  # TODO: なんでここrow? idにしよう
  months <- rexp(length(rows_cow_overlooked), param_calculated$rate_ebl_die)
  months <- ceiling(months)
  return(months)
}


#' Whether cows are infected by insects
#'
#' @param n_cows The number of cows.
#' @param month The current month (1, 2, ..., 12).
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_infected_insects <- function(n_cows, month, param_calculated) {
  prob_inf_insects <- param_calculated$probs_inf_insects_month[1:12 == month]
  is_infected <- runif(n_cows) < prob_inf_insects
  return(is_infected)
}


#' Whether cows are infected in a communal pasture
#'
#' @param n_cows The number of cows.
#' @param param_farm See [param_farm].
#'
#' @return A logical vector.
is_infected_compas <- function(n_cows, param_farm) {
  runif(n_cows) < param_farm$prob_seroconv_compas
}


#' Whether cows are infected in chambers next to infected cows in tie-stall barns
#'
#' @param n_cows The number of cows.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_infected_in_exposed_chamber <- function(n_cows, param_calculated) {
  inf_status <- rep(NA_character_, n_cows)
  hr <- param_calculated$hr_having_infected_neighbor
  is_infected <- runif(n_cows) < param_farm$prob_inf_tiestall_baseline * hr
  inf_cause <- sample(c("neighbor", "insect"), size = sum(is_infected), 
                      replace = T, prob = c(hr - 1, 1))
  inf_status[is_infected] <- inf_cause
  return(inf_status)
}


#' Whether cows are infected in chambers not next to infected cows in tie-stall barns
#'
#' @param n_cows The number of cows.
#' @param param_calculated See [calc_param].
#'
#' @return A logical vector.
is_infected_in_non_exposed_chamber <- function(n_cows, param_calculated) {
  runif(n_cows) < param_calculated$prob_inf_tiestall_baseline
}


#' Whether cows are infected by direct contact
#'
#' @return A logical vector.
is_infected_contact <- function() {
  # TODO: 中身はあとで考える
  # これまだどこからも繋がってない
}

# hr_neighbor_inf <- exp(rnorm(1, mean = log(12.43), sd = (log(51.98) - log(12.43)) / qnorm(0.975)))
# risk_unexposed <- prob_ial / (0.396 + hr_neighbor_inf * 0.694)
# risk_exposed <- risk_unexposed * hr_neighbor_inf
# r_neighborhood_infection <- c(risk_unexposed, risk_exposed)

# TODO: そういや全国平均感染率は感染農場も非感染農場も一緒にしてるんだった。あとで感染農場のみにしぼって計算し直す。


#' Whether cows are infected by contaminated needles
#'
#' @param n_cows The number of cows.
#' @param cows See [cow_table].
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_infected_needles <- function(n_cows, cows, param_calculated) {
  # Studies successed to prove infection by contaminated needles
  # (in Japan) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2835688/?tool=pmcentrez&report=abstract
  # Several studies failed to prove infection by contaminated needles
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1255626/pdf/cjvetres00045-0186.pdf
  # https://academic.oup.com/aje/article/117/5/621/102629
  # (in Japan) https://www.sciencedirect.com/science/article/pii/S0034528813003767
  #   By same authors with a "successed" paper in Japan, probably with more samples
  n_infected <- cows[is_owned & infection_status != "s", .N]
  is_infected_needles <- runif(n_cows) < 
    param_calculated$prob_inf_needles * (n_infected / n_cows)
  return(is_infected_needles)
}
# TODO: Gauge dehorning https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1236184/pdf/compmed00003-0104.pdf
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1255626/pdf/cjvetres00045-0186.pdf 


#' Whether cows are infected by rectal palpation
#'
#' @param n_cows_palpated ID of rectally palpated cows.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_infected_rp <- function(n_cows_palpated, param_calculated) {
 # TODO: ここ他のis_infected_xxxにそろえてid_cow_xxxにしたほうがいいかも。それとも他をn_xxxにする？
  runif(n_cows_palpated) < param_calculated$prob_inf_rp
}


#' Wheter newborns are infected vertically
#'
#' @param status_mother The `infection_status` of dams.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_infected_vertical <- function(status_mother, param_calculated) {
  n_calf <- length(status_mother)
  is_vert_inf_ial <-
    runif(n_calf) < param_calculated$prob_vert_inf_ial *
    (status_mother == "ial")
  is_vert_inf_ipl <-
    runif(n_calf) < param_calculated$prob_vert_inf_ipl *
    (status_mother == "ipl" | status_mother == "ebl")
  is_vert_inf <- (is_vert_inf_ial | is_vert_inf_ipl)
  return(is_vert_inf)
}


#' Wheter newborns are infected by colostrum milk
#'
#' @param status_mother The `infection_status` of dams.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_infected_by_colostrum <- function(status_mother, param_calculated) {
  runif(length(status_mother)) < 
    param_calculated$prob_inf_colostrum * (status_mother != "s")
}


#' Whether introduced cows are infected
#'
#' @return A logical vector
is_infected_introduced <- function() {
  # TODO: 中身はあとで考える
  # これまだどこからも繋がってない
}


#' Whether cows are infected at a communal ranch
#'
#' When a cow is come back from a communal ranch, the probability of infection at communal ranch was calculated.
#'
#' @return A logical vector.
is_infected_comranch <- function() {
  # TODO: 中身はあとで考える
  # これまだどこからも繋がってない
}


#' Whether the first AIs for milking cows are conducted
#'
#' @param n_month_from_delivery The month past from the last delivery.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_ai_started_milking <- function(n_month_from_delivery, param_calculated) {
  prob_start_ai <- pnorm(n_month_from_delivery,
                         mean = param_calculated$mean_date_start_ai,
                         sd = param_calculated$sd_date_start_ai)
  is_ai_started_milking <-
    (runif(length(n_month_from_delivery)) <= prob_start_ai)
  return(is_ai_started_milking)
}


#' Whether the first AIs for hifers are conducted
#'
#' @param ages Age of heifers.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_ai_started_heifer <- function(ages, param_calculated) {
  prob_first_ai <- pnorm(ages,
                         mean = param_calculated$mean_age_first_ai,
                         sd = param_calculated$sd_age_first_ai)
  prob_first_ai[ages < param_calculated$lower_lim_first_ai] <- 0
  is_ai_started_heifer <- (runif(length(ages)) <= prob_first_ai)
  return(is_ai_started_heifer)
}


#' Whether a heat is detected
#'
#' @param n_cows The number of cows which come into heat.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_heat_detected <- function(n_cows, param_calculated) {
  runif(n_cows) < param_calculated$prob_heat_detected
}


#' Whether a cow is concepted at the first or later AI
#'
#' @param n_cows The number of cows on which the first AI were conducted.
#' @param param_calculated Return from [calc_param()].
#'
#' @rdname is_ai_successed
#' @return A logical vector.
is_first_ai_successed <- function(n_cows, param_calculated) {
  runif(n_cows) < param_calculated$prob_first_ai_success
}


#' @rdname is_ai_successed
is_ai_successed <- function(n_cows, param_calculated) {
  runif(n_cows) < param_calculated$prob_ai_success
}


#' Heat cycle
#'
#' @param n_cows The number of cows to which heat cycle should be calculated.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A numeric vector.
heat_cycle <- function(n_cows, param_calculated) {
  round(rnorm(n_cows, mean = 21, sd = param_calculated$sd_heat))
}


#' Whether a cow is dried
#'
#' @param months_from_delivery The number of months past from the last delivery.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_dried <- function(months_from_delivery, param_calculated) {
  # TODO: ここ基準の前後1ヶ月以内で必ず乾乳することになってるのでどうにかしたい
  (months_from_delivery > param_calculated$lower_lim_dried) |
  ((months_from_delivery == param_calculated$lower_lim_dried) &
     (runif(length(months_from_delivery)) < 
        param_calculated$prop_dried_shorter))
}


#' Calculate susceptibility of newborns to the pathogen
#'
#' @param n_newborns The number of newborns.
#' @param susceptibility_ial_to_ipl_dam,susceptibility_ipl_to_ebl_dam The dam's susceptibility to the pathogen.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A list consisted of susceptibility of newborns.
susceptibility <- function(n_newborns,
                           susceptibility_ial_to_ipl_dam,
                           susceptibility_ipl_to_ebl_dam,
                           param_calculated) {
  inherit_from_dam <- runif(n_newborns) < 0.5
  ial_to_ipl <- fifelse(inherit_from_dam,
                        susceptibility_ial_to_ipl_dam,
                        runif(n_newborns) < param_calculated$prob_develop_ipl)
  ipl_to_ebl <- fifelse(inherit_from_dam,
                        susceptibility_ipl_to_ebl_dam,
                        runif(n_newborns) < param_calculated$prob_develop_ebl &
                          ial_to_ipl)
  susceptibility <- list(ial_to_ipl = ial_to_ipl, ipl_to_ebl = ipl_to_ebl)
  return(susceptibility)
}

# TODO: test


#' The number of newborns per dam
#'
#' @param n_dams The number of dams.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A numeric vector consisted of 1 and 2.
n_newborn_per_dam <- function(n_dams, param_calculated) {
  (runif(n_dams) < param_calculated$prob_twin) + 1
}
# Probability to be triplets or more is ignored.


#' Sex ratio of newborns
#'
#' `sex_newborns()` returns sex of singleton newborns.
#' `sex_twins()` returns sex of pairs of twin newborns.
#' Probability to be triplets or more is ignored (=0).
#'
#' @param n_newborns,n_calves The number of newborns.
#' @param param_calculated Return from [calc_param()].
#'
#' @rdname sex_newborns
#' @return A character vector consisted of "male", "female" and "freemartin".
sex_newborns <- function(n_newborns, param_calculated) {
  c("female", "male")[(runif(n_newborns) > param_calculated$prob_female) + 1]
  # Equals to sample(c(...), n_newborns, replace = T, prob = c(prob, 1 - prob))
}


#' @rdname sex_newborns
sex_twins <- function(n_calves, param_calculated) {
  # TODO: なんでここn_newbornsじゃなくてn_calvesなんだ？
  sex_pairs <- sample(c("male-male", "male-freemartin", "female-female"),
                      size = (n_calves / 2),
                      prob = param_calculated$probs_sex_pairs)
  sex_calves <- strsplit(paste(sex_pairs, collapse = "-"), split = "-")[[1]]
  return(sex_calves)
}
# TODO: 性判別精液は双子が少ない？？


#' Whether a newborn will be a replacement
#'
#' @param n_calves The number of newborns.
#' @param herd_size The current herd size.
#' @param n_delivered The number of delivered cows in a herd.
#' @param param_farm See [param_farm].
#' @param param_processed Return from [process_param()].
#'
#' @name is_replacement
#' @references 「平成28年度 乳用種初生牛の経営に関する調査報告書」の「表40 調査対象経営の乳用種雌子牛の仕向け状況（規模別）」
#' @encoding UTF-8
#' @return A logical vector or a numeric value.
set_prob_rep <- function(n_delivered, param_farm) {
  # TODO: これ他の場所に移動した方がいいか確認する
  prob_rep <- if (!is.na(param_farm$prop_replacement)) {
    param_farm$prop_replacement
  } else if (n_delivered < 30) {
    4 / (0.2 + 4)
  } else if (n_delivered < 50) {
    4.6 / (1 + 4.6)
  } else if (n_delivered < 80) {
    9.9 / (1.7 + 9.9)
  } else if (n_delivered < 100) {
    26.7 / (1 + 26.7)
  } else {
    44.5 / (3.2 + 44.5)
  }
  return(prob_rep)
}


#' @name is_replacement
is_replacement <- function(n_calves, herd_size, param_processed) {
  spaces <- (param_processed$herd_size_limits - herd_size) *
              (param_processed$herd_size_limits > herd_size)
  spaces <- spaces * (spaces <= n_calves) + n_calves * (spaces > n_calves)

  n_selected <- rbinom(1, n_calves, param_processed$prob_rep)
  if (n_selected < spaces[1]) {
    n_selected <- spaces[1]
  } else if (n_selected > spaces[2]) {
    n_selected <- spaces[2]
  }

  is_replacement <- rep(F, n_calves)
  is_replacement[sample.int(n_calves, n_selected)] <- T

  return(is_replacement)
}


#' Whether a delivery end up in stillbirth or abortion
#'
#' Here, stillbirth means stillbirth and abortion.
#'
#' @param parity Parity of dams.
#' @param param_calculated Return from [calc_param()].
#'
#' @return A logical vector.
is_stillbirth <- function(parity, param_calculated) {
  prob_sb <- param_calculated$prob_sb_1 * (parity == 1) +
    param_calculated$prob_sb_2 * (parity == 2) +
    param_calculated$prob_sb_3 * (parity == 3) +
    param_calculated$prob_sb_4 * (parity == 4) +
    param_calculated$prob_sb_5 * (parity >= 5)
  is_sb <- (runif(length(parity)) <= prob_sb)
  return(is_sb)
}
# TODO: 双子だったら死産率上がりそう→調べる
# TODO: 母牛死亡について考慮
# TODO: ここstillbirthとabortionの両方調べたっけ


#' Expected age of death or slaugher
#'
#' @param n_cows The number of cows to calculate expected age of death.
#' @param param_calculated Return from [calc_param()].
#'
#' @rdname longevity
#' @return A numeric vector.
age_die <- function(n_cows, param_calculated) {
  is_exp <- runif(n_cows)
  value <- rexp(n_cows,
                rate = param_calculated$die_e_rate) *
    (is_exp < param_calculated$die_prop) +
    rgamma(n_cows,
           shape = param_calculated$die_g_shape,
           rate = param_calculated$die_g_rate) *
    (is_exp >= param_calculated$die_prop)
  return(trunc(value))
}


#' @name longevity
age_slaughtered <- function(n_cows, param_calculated) {
  value <- rgamma(n_cows,
                  shape = param_calculated$slaughter_shape,
                  rate = param_calculated$slaughter_rate)
  return(trunc(value))
}


#' @name longevity
longevity <- function(n_cows, param_calculated) {
  # TODO: これcause_deathとかのが正確やな
  longevity <- list(
    age = numeric(n_cows),
    cause = rep("will_be_slaughtered", n_cows)
  )
  will_die <- runif(n_cows) <= param_calculated$prob_died
  longevity$age <- age_die(n_cows, param_calculated) * will_die +
    age_slaughtered(n_cows, param_calculated) * (!will_die)
  longevity$cause[will_die] <- "will_die"
  return(longevity)
}
