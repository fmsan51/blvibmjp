## ---- infection_status_change ----
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
#'
#' @return A list consisted of a month
#' @export
months_progression <- function(susceptibility_ial_to_ipl,
                               susceptibility_ipl_to_ebl,
                               i) {
  # TODO: ここsusceptibility...って結局何してるんだ？
  n_cows <- length(susceptibility_ial_to_ipl)
  params <- params_this_run$params_disease_progress
  months_ial_to_ebl <- prop_ial_period <- months_ial_to_ipl <- numeric(n_cows)
  neg_months_ial_to_ipl <- rep(T, n_cows)
  while (any(neg_months_ial_to_ipl)) {
    months_ial_to_ebl[neg_months_ial_to_ipl] <- rweibull(
                           n_cows,
                           shape = params$shape,
                           scale = params$scale
                         ) * 12
    prop_ial_period[neg_months_ial_to_ipl] <- rnorm(n_cows,
                             mean = params$mean_prop_ial_period,
                             sd = params$sd_prop_ial_period)
    prop_ial_period[prop_ial_period < 0] <- 0
    months_ial_to_ipl[neg_months_ial_to_ipl] <- rweibull(
                           n_cows,
                           shape = params$shape,
                           scale = params$scale * prop_ial_period
                         ) * 12
    neg_months_ial_to_ipl <- months_ial_to_ipl < 0
  }
  months_ial_to_ebl <- ceiling(months_ial_to_ebl)
  months_ial_to_ipl <- ceiling(months_ial_to_ebl)
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


## ---- is_ebl_detected
#' Whether a EBL cow is detected
#'
#' @param id_cow_ebl The ID of EBL cows.
#'
#' @return A logical vector
is_ebl_detected <- function(id_cow_ebl) {
  sample(c(T, F), size = length(id_cow_ebl), replace = T,
         prob = params_this_run$probs_ebl_detected)
}

#' The number of months until EBL cows die
#'
#' @param rows_cow_overlooked The row IDs of EBL-yet-non-detected cows.
#'
#' @return A numeric vector
months_until_ebl_die <- function(rows_cow_overlooked) {
  # TODO: なんでここrow? idにしよう
  months <- rexp(length(rows_cow_overlooked), params_this_run$rate_ebl_die)
  months <- ceiling(months)
  return(months)
}


## ---- infection_route ----

## ---- is_infected_insects
#' Wheter cows are infected by insects
#'
#' @param id_cow_s ID of infected cows.
#' @param month The current month (1, 2, ..., 12)
#'
#' @return A logical vector
is_infected_insects <- function(id_cow_s, month) {
  prob_inf_insetcs <- params_this_run$probs_inf_insects_month[1:12 == month]
  is_infected <- sample(c(T, F), size = length(id_cow_s), replace = T,
                        prob = c(prob_inf_insetcs, 1 - prob_inf_insetcs))
  return(is_infected)
}


## ---- is_infected_contact

#' Whether cows are infected by direct contact
#'
#' @param id_cow_s ID of infected cows.
#'
#' @return A logical vector
is_infected_contact <- function() {
  # TODO: 中身はあとで考える
  # これまだどこからも繋がってない
}

# hr_neighbor_inf <- exp(rnorm(1, mean = log(12.43), sd = (log(51.98) - log(12.43)) / qnorm(0.975)))
# risk_unexposed <- prob_ial / (0.396 + hr_neighbor_inf * 0.694)
# risk_exposed <- risk_unexposed * hr_neighbor_inf
# r_neighborhood_infection <- c(risk_unexposed, risk_exposed)

# TODO: そういや全国平均感染率は感染農場も非感染農場も一緒にしてるんだった。あとで感染農場のみにしぼって計算し直す。


## ---- is_infected_needles
#' Whether cows are infected by contaminated needles
#'
#' @param id_cow_s `cow_id` of infected cows
#'
#' @return A logical vector
is_infected_needles <- function(id_cow_s) {
  sample(c(T, F), size = length(id_cow_s), replace = T,
         prob = params_this_run$probs_inf_needles)
}


## ---- is_infected_rp
#' Whether cows are infected by rectal palpation
#'
#' @param n_cows_palpated IDs of rectally palpated cows
#'
#' @return A logical vector
is_infected_rp <- function(n_cows_palpated) {
  # TODO: ここ他のis_infected_xxxにそろえてid_cow_xxxにしたほうがいいかも。それとも他をn_xxxにする？
  sample(c(T, F), size = n_cows_palpated, replace = T,
         prob = params_this_run$probs_inf_rp)
}



## ---- is_infected_vertical
#' Wheter newborns are infected vertically
#'
#' @param status_mother The `infection_status` of dams.
#'
#' @return A logical vector
is_infected_vertical <- function(status_mother) {
  n_calf <- length(status_mother)
  is_vert_inf_ial <-
    sample(c(T, F), size = n_calf, replace = T,
           prob = params_this_run$probs_vert_inf_ial) &
    (status_mother == "ial")
  is_vert_inf_ipl <-
    sample(c(T, F), size = n_calf, replace = T,
           prob = params_this_run$probs_vert_inf_ipl) &
    (status_mother == "ipl" | status_mother == "ebl")
  is_vert_inf <- (is_vert_inf_ial | is_vert_inf_ipl)
  return(is_vert_inf)
}


## ---- is_infected_introduced
#' Whether introduced cows are infected
#'
#' @return A logical vector
is_infected_introduced <- function() {
  # TODO: 中身はあとで考える
  # これまだどこからも繋がってない
}


## ---- is_infected_comranch
#' Whether cows are infected at a communal ranch
#'
#' When a cow is come back from a communal ranch, the probability of infection at communal ranch was calculated.
#'
#' @return A logical vector
is_infected_comranch <- function() {
  # TODO: 中身はあとで考える
  # これまだどこからも繋がってない
}



## ---- artificial_insemination ----

## ---- is_ai_started_milking
#' Whether the first AIs for milking cows are conducted
#'
#' @param n_month_from_delivery The month past from the last delivery.
#'
#' @return A logical vector
is_ai_started_milking <- function(n_month_from_delivery) {
  prob_start_ai <- pnorm(n_month_from_delivery,
                         mean = params_this_run$mean_date_start_ai,
                         sd = params_this_run$sd_date_start_ai)
  is_ai_started_milking <-
    (runif(length(n_month_from_delivery)) <= prob_start_ai)
  return(is_ai_started_milking)
}


## ---- is_ai_started_heifer
#' Whether the first AIs for hifers are conducted
#'
#' @param ages Age of heifers
#'
#' @return A logical vector
is_ai_started_heifer <- function(ages) {
  prob_first_ai <- pnorm(ages,
                         mean = params_this_run$mean_age_first_ai,
                         sd = params_this_run$sd_age_first_ai)
  prob_first_ai[ages < params_this_run$lower_lim_first_ai] <- 0
  is_ai_started_heifer <- (runif(length(ages)) <= prob_first_ai)
  return(is_ai_started_heifer)
}


## ---- is_heat_detected
#' Whether a heat is detected
#'
#' @param n_cows The number of cows which come into heat.
#'
#' @return A logical vector
is_heat_detected <- function(n_cows) {
  sample(c(T, F), size = n_cows, replace = T,
         prob = params_this_run$probs_heat_detected)
}



## ---- is_first_ai_successed
#' Whether a cow is concepted at the first or later AI
#'
#' @param n_cows The number of cows on which the first AI were conducted
#'
#' @rdname is_ai_successed
#' @return A logical vector
is_first_ai_successed <- function(n_cows) {
  sample(c(T, F), size = n_cows, replace = T,
         prob = params_this_run$probs_first_ai_success)
}


## ---- is_ai_successed
#' @rdname is_ai_successed
is_ai_successed <- function(n_cows) {
  sample(c(T, F), size = n_cows, replace = T,
         prob = params_this_run$probs_ai_success)
}


## ---- heat_cycle
#' Heat cycle
#'
#' @param n_cows The number of cows to which heat cycle should be calculated
#'
#' @return A numeric vector
heat_cycle <- function(n_cows) {
  round(rnorm(n_cows, mean = 21, sd = params_this_run$sd_heat))
}


## ---- milking_status ----


## ---- is_dried
#' Whether a cow is dried
#'
#' @param months_from_delivery The number of months past from the last delivery
#'
#' @return A logical vector
is_dried <- function(months_from_delivery) {
  # TODO: ここ基準の前後1ヶ月以内で必ず乾乳することになってるのでどうにかしたい
  (months_from_delivery > params_this_run$lower_lim_dried) |
  ((months_from_delivery == params_this_run$lower_lim_dried) &
     (runif(length(months_from_delivery)) < params_this_run$prop_dried_shorter))
}


## ---- reproduction ----


## ---- susceptibility
#' Calculate susceptibility of newborns to the pathogen
#'
#' @param n_newborns The number of newborns
#' @param susceptibility_ial_to_ipl_dam,susceptibility_ipl_to_ebl_dam The dam's susceptibility to the pathogen
#'
#' @return A list consisted of susceptibility of newborns
susceptibility <- function(n_newborns,
                           susceptibility_ial_to_ipl_dam,
                           susceptibility_ipl_to_ebl_dam) {
  inherit_from_dam <- sample(c(T, F), n_newborns, replace = T)
  ial_to_ipl <- ifelse(inherit_from_dam,
                       susceptibility_ial_to_ipl_dam,
                       sample(c(T, F), n_newborns, replace = T,
                              prob = params_this_run$probs_develop_ipl))
  ipl_to_ebl <- ifelse(inherit_from_dam,
                       susceptibility_ipl_to_ebl_dam,
                       sample(c(T, F), n_newborns, replace = T,
                              prob = params_this_run$probs_develop_ebl) &
                       ial_to_ipl)
  susceptibility <- list(ial_to_ipl = ial_to_ipl, ipl_to_ebl = ipl_to_ebl)
  return(susceptibility)
}

# TODO: test


## ---- n_newborn_per_dam
#' The number of newborns per dam
#'
#' @param n_dams The number of dams
#'
#' @return A numeric vector consisted of 1 and 2
n_newborn_per_dam <- function(n_dams) {
  sample(2:1, n_dams, replace = T, prob = params_this_run$probs_twin)
}
# Probability to be triplets or more is ignored.



## ---- sex_ratio
#' Sex ratio of newborns
#'
#' `sex_newborns()` returns sex of singleton newborns.
#' `sex_twins()` returns sex of pairs of twin newborns.
#' Probability to be triplets or more is ignored (=0).
#'
#' @param n_newborns,n_calves The number of newborns.
#'
#' @rdname sex_newborns
#' @return A character vector consisted of "male", "female" and "freemartin"
sex_newborns <- function(n_newborns) {
  sample(c("female", "male"), size = n_newborns, replace = T,
         prob = params_this_run$probs_female)
}

#' @rdname sex_newborns
sex_twins <- function(n_calves) {
  # TODO: なんでここn_newbornsじゃなくてn_calvesなんだ？
  sex_pairs <- sample(c("male-male", "male-freemartin", "female-female"),
                      size = (n_calves / 2),
                      prob = params_this_run$probs_sex_pairs)
  sex_calves <- strsplit(paste(sex_pairs, collapse = "-"), split = "-")[[1]]
  return(sex_calves)
}
# TODO: 性判別精液は双子が少ない？？

## ---- is_replacement
#' Whether a newborn will be a replacement
#'
#' @param n_calves The number of newborns.
#' @param capacity Capacity of a herd.
#' @param herd_size The current herd size.
#' @param n_delivered The number of delivered cows in a herd.
#'
#' @rdname is_replacement
#' @references 「平成28年度 乳用種初生牛の経営に関する調査報告書」の「表40 調査対象経営の乳用種雌子牛の仕向け状況（規模別）」より
#' @return A logical vector or a numeric value (`set_prob_rep()`)
set_prob_rep <- function(n_delivered) {
# TODO: これ 1 default_parametersに移す
  prob_rep <- if (!is.na(PARAMS_FARM$prop_replacement)) {
    PARAMS_FARM$prop_replacement
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

#' @rdname is_replacement
is_replacement <- function(n_calves, capacity, herd_size) {
  # capacity: upper and lower limits of number of cattle should be kept in the farm
  spaces <- (capacity - herd_size) * (capacity > herd_size)
  spaces <- spaces * (spaces <= n_calves) + n_calves * (spaces > n_calves)

  n_selected <- rbinom(1, n_calves, params_calculated$prob_rep)
  if (n_selected < spaces[1]) {
    n_selected <- spaces[1]
  } else if (n_selected > spaces[2]) {
    n_selected <- spaces[2]
  }

  is_replacement <- rep(F, n_calves)
  is_replacement[sample.int(n_calves, n_selected)] <- T

  return(is_replacement)
}


## ---- is_stillbirth
#' Whether a delivery end up in stillbirth or abortion
#'
#' Here, stillbirth means stillbirth and abortion
#'
#' @param parity Parity of dams
#'
#' @return A logical vector
is_stillbirth <- function(parity) {
  prob_sb <- params_this_run$prob_sb_1 * (parity == 1) +
    params_this_run$prob_sb_2 * (parity == 2) +
    params_this_run$prob_sb_3 * (parity == 3) +
    params_this_run$prob_sb_4 * (parity == 4) +
    params_this_run$prob_sb_5 * (parity >= 5)
  is_sb <- (runif(length(parity)) <= prob_sb)
  return(is_sb)
}
# TODO: 双子だったら死産率上がりそう→調べる
# TODO: 母牛死亡について考慮
# TODO: ここstillbirthとabortionの両方調べたっけ

## ---- longevity ----
#' Expected age of death or slaugher
#'
#' @param n_cows The number of cows to calculate expected age of death
#'
#' @rdname longevity
#' @return A numeric vector
age_die <- function(n_cows) {
  is_exp <- runif(n_cows)
  value <- rexp(n_cows,
                rate = params_this_run$params_die[[4]][2]) *
    (is_exp < params_this_run$params_die[[4]][1]) +
    rgamma(n_cows,
           shape = params_this_run$params_die[[4]][3],
           rate = params_this_run$params_die[[4]][4]) *
    (is_exp >= params_this_run$params_die[[4]][1])
  return(trunc(value))
}

#' @rdname longevity
age_slaughtered <- function(n_cows) {
  value <- rgamma(n_cows,
                  shape = params_this_run$params_slaughtered[[4]][1],
                  rate = params_this_run$params_slaughtered[[4]][2])
  return(trunc(value))
}

#' @rdname longevity
longevity <- function(n_cows) {
  # TODO: これcause_deathとかのが正確やな
  longevity <- list(
    age = numeric(n_cows),
    cause = rep("will_be_slaughtered", n_cows)
  )
  will_die <- runif(n_cows) <= params_this_run$prob_died
  longevity$age <- age_die(n_cows) * will_die +
    age_slaughtered(n_cows) * (!will_die)
  longevity$cause[will_die] <- "will_die"
  return(longevity)
}







