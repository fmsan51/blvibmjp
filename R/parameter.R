#' Parameters about a simulation which should be set by users
#'
#' - `simulation_length`: Length of simulation (months). (default: 60)
#' - `n_simulation`: The number of simulation. (default: 1)
#' - `simulation_start`: The month simulation starts (1 = Jan, 2 = Feb, ...). (default: 1)
#' - `input_csv`: Path to a csv file which contains cattle information.
#' - `output_dir`: Directory to output files. (default: data/output)
#' - `output_filename`: The name of the output files. (default: "simulation")
#'
#' @seealso [param_farm] [param_area]
#' @export
param_simulation <- list(
  # TODO: The function to confirm the necessary parameters are set or not
  simulation_length = 60,
  n_simulation = 1,
  simulation_start = 1,
  input_csv = NA_character_,
  output_dir = "data/output",
  output_filename = "simulation"
)


#' Parameters about farms which should be set by users
#'
#' The following parameters are passed to [calc_param()] and the parameter will be calculated.
#'
#' - `prop_female` (0-1): Proportion of female of newborns. (default: average in Hokkaido)
#' - `prop_replacement` (0-1): Proportion of calvest to be replacements in female newborns. (default: average in Hokkaido)
#' - `prop_died` (1-0): Proportion of dead cows in removed cows (Died / (Died + Slaughtered)). (default: average in Hokkaido)
#' - `prop_heat_detected` (0-1): Proportion of detected heats in total heats. (default: average in Hokkaido)
#' - `calving_interval`: Calving interval in day. (default: average in Hokkaido)
#' - `n_mean_ai`: Mean number of AI done until conception.
#' - `mean_age_first_ai`, `sd_age_first_ai`: Age (in month) of the first AI for heifers. $~Norm(mean, sd)$. (default: mean = mode of Hokkaido, sd = `2 / qnorm(0.975)`)
#' - `mean_day_start_ai`, `sd_day_start_ai`: Day of first AI after a delivery. $~Norm(mean, sd)$. (default: mean = mean of Hokkaido, sd = `10 / qnorm(0.975)`)
#' - `months_grazing` (1-12), `hours_grazing` (0-23): Months and hours for grazing. (default: no grazing)
#' - `capacity_in_head` c(lower, upper): Lower/upper limit of the herd size. Set either this or `capacity_as_ratio` below.
#' - `capacity_as_ratio` c(lower, upper): Lower/upper limit of the herd as ratio to the initial herd size (lower limit = `lower * initial_herd_size`, upper limit = `upper * initial_herd_size`). Set either this or `capacity_in_head` above. When both of `capacity_in_head` and `capacity_as_ratio` is NA, `capacity_as_ratio` is set to `c(0.9, 1.1)`.
#' - `use_communal_pasture` (logical): whether use a communal pasture. (default: FALSE)
#' - `n_introduced` c(calf, heifer, delivered): The number of introduced cows for five years. (default: c(0, 0, 0))
#' - `days_qualantine`: Length of qualantine period (in days) for introduced cows in which introduced cows contacted no cows but introduced ones at the same time. (default: 0)
#' - `control_insects` (logical or 0-1): wheter conduct control measures against insects. When specified by a number from 0 to 1, it means that the number of bloodsucking insects decrease to this proportion (i.e., `control_insects = 0.8` means that the number of insects becomes 80%). When `TRUE`, it is assumed that insects in a farm decrease to 50%. (default: FALSE)
#' - `change_needles` (logical): whether use one needles for one cow. (default: TRUE)
#' - `change_gloves` (logical): whether use one glove for one cow for rectal palpation. (default: TRUE)
#' - `days_milking`: Length of milking period (in days). (default: average in Hokkaido)
#'
#' @seealso [param_simulation] [param_area] [calc_param]
#' @export
param_farm <- list(
  # TODO: Unify "farm" and "herd"
  # TODO: The function to confirm the necessary parameters are set or not
  prop_female = NA,
  prop_replacement = NA,
  prop_died = NA,

  # Reproductivity parameters
  prop_heat_detected = NA,
  calving_interval = NA,  # TODO: currently used nowhere
  n_mean_ai = NA,  # TODO: currently used nowhere
  mean_age_first_ai = NA,
  sd_age_first_ai = NA,
  mean_day_start_ai = NA,
  sd_day_start_ai = NA,

  months_grazing = NA,
  hours_grazing = NA,

  n_introduced = c(0, 0, 0),
  days_qualantine = 0,
  capacity_in_head = NA,
  capacity_as_ratio = NA,
  # TODO: Warn if capacity doesn't follow current number of cows
  # TODO: Warn if both of capacity_in_head and capacity_as_ratio are set

  use_communal_pasture = F,

  control_insects = F,
  change_needles = NA,
  # TODO: Make it to prop
  change_gloves = T,
  # TODO: ditto

  days_milking = NA
)


#' Parameters about areas which should be set by users
#'
#' - `calf_area_id`: Numeric vector. Set the `area_id`(s) for newborn calves. For detail of `area_id`, see `[area_table]`.
#' - `calf_area_priority`: Specify priority for calf areas. `NA` is allowed. For detail of `priority`, see [area_table].
#'
#' @seealso [param_simulation] [param_farm]
#' @export
param_area <- list(
  # TODO: The function to confirm the necessary parameters are set or not
  calf_area_id = NA_real_,  # TODO: Make multiple area settable
  calf_area_priority = NA_real_

  # Does a farm decide the chamber layout of milking cows based on lactation stage?
  # is_layout_on_stage = F,

  # For a farm which decide the chamber layout based on parity, specify how cows are areaed.
  # For example, c(1, 2, 4) means there are three areas (1, 2-3, >4).
  # layout_on_parity = NA

  # FYI: areaing of dry cows and mixsing of breeding and dry cows
  # https://www.snowseed.co.jp/wp/wp-content/uploads/grass/grass_200509_03.pdf
)


#' Overwrite default parameters with herd specific parameters
#'
#' It's used to overwrite default parameters (averages of Hokkaido or Japan) with farm specific parameters.
#' When 'parameter' is not NA (= when a farm specific parameter was set), it returns the farm specific parameters.
#' When 'parameter' is NA (= when a farm specific parameter was not set), it returns dafault parameters.
#'
#' @param parameter The farm specific parameter
#' @param default The default parameter
#'
#' @return A list of overwritten parameters
set_param <- function(parameter, default) {
  if (anyNA(parameter)) {  # Much faster than (is.na(parameter[1]))
    parameter <- default
  } else if (length(parameter) != length(default)) {
    parameter <- rep(parameter, length.out = length(default))
  }
  return(parameter)
}


#' Calculate parameters necessary to the simulation.
#'
#' Calculate parameters to the simulation and overwrite the default setting if necessary.
#'
#' Parameters processed by [process_param()] are deteministic. Parameters calculated by [calc_param()] are stochastic.
#'
#' @param param_farm See [param_farm].
#' @param modification A list used to overwrite the defaut parameter like `list(parameter_name = new_value, ...)`.
#'
#' @return A parameter list.
#' @export
calc_param <- function(param_farm, modification = NULL) {
  param <- list()

  ## infection_status_change ----
  # Changes of infection status
  # Memo: We don't need to consider infection from rectal palpation here,
  #   because ordinal farms are assumed to change gloves for each rectal palpation.
  #   TODO: Some farmers don't change gloves. consider again.

  param$mean_prop_ial_period <-  0.3
  param$sd_prop_ial_period <- (0.3 - 0.2) / qnorm(0.975)
  # Length of periods from PL to EBL is not well known. (several months to years)
  param$ebl_progress_shape <- 3.3
  param$ebl_progress_scale <- 7.8
  # Periods until an infected cattle develops EBL: rweibull(n, shape, scale) * 12 (Tsutsui et al, 2016)

  ## Probabilities of disease progress ----
  # Proportion of ial cattle which develops ipl
  param$prob_develop_ipl <- 0.3  # 30% of infected cattle develops ipl (OIE terrestrial manual)
  # Proportion of blv infected cattle which develops ebl
  param$prob_develop_ebl <- 0.014 / param$prob_develop_ipl  # 1.4% of BLV-infected cattle develops ebl (Tsutsui et al, 2016)

  # Probability that an BLV-infected cow is detected
  # 39.7% of infected cows are detected (Tsutui et al, 2015)
  # This 39.7% are assumed to found at the month in which infection stage moved from Ial to Ipl.
  # (Because Tsutsui et al. assumed as the same and there is no data about the length of period from clinical onset to detection.)
  param$prob_ebl_detected <- rnorm(1, mean = 0.397,
                                   sd = (0.397 - 0.358) / qnorm(0.975))

  # Months until EBL cattle die
  param$rate_ebl_die <- 1 / 2  # Average months until die is set to 2m

  ## infection_insects ----

  ## Probability of infection by bloodsucking insects per month per cattle ----
  # Read preps/Parameters_num_insects.Rmd
  probs_inf_insects_month <- c(0, 0, 0, 0,  # Jan to Apr
                               0.0028,  # May
                               0.0107,  # Jun
                               0.0146,  # Jul
                               0.0063,  # Aug
                               0.0406,  # Sep
                               0.0140,  # Oct
                               0.0001,  # Nov
                               0        # Dec
                               )
  control_insects <- param_farm$control_insects
  if (is.logical(control_insects)) {
    insects_pressure <- fifelse(control_insects, 0.5, 1)
  } else {
    insects_pressure <- control_insects
  }
  param$probs_inf_insects_month <- probs_inf_insects_month * insects_pressure

  ## infection_contact ----


  ## infection_needles ----

  # Infection by using same needles among infected and non-infected cattle
  # Infection probability per day
  # TODO: temporary, just by inspiration
  change_needles <- set_param(param_farm$change_needles, T)
  param$prob_inf_needles <- fifelse(change_needles, 0, 0.005)

  ## infection_rp ----
  # Infection by rectal palpation
  # 3/4 cows get infected by 4 rectal palpations right after infected cows (Kohara et al, 2016)
  # The probability of infection per try is calculated.
  # The probability is 0.034 in [Lack of evidence of transmission of bovine leukemia virus by rectal palpation of dairy cows. - PubMed - NCBI](https://www.ncbi.nlm.nih.gov/pubmed/2557314)

  # 直検1回ごとの感染確率
  change_gloves <- set_param(param_farm$change_gloves, T)
  param$prob_inf_rp <- fifelse(change_gloves, 0, 1 - (1 - 3 / 4) ^ (1 / 4))


  ## infection_vertical ----

  # Vertical infection
  ## Probability of vertical infection for calves born from BLV-infected dams ----
  # Vet Microbiol. 2002 Jan 23;84(3):275-82.  Vertical transmission of bovine leukemia virus and bovine immunodeficiency virus in dairy cattle herds.  Prorobability of vertical infection is 0
  # https://www.jstage.jst.go.jp/article/jvma1951/34/9/34_9_423/_article/-char/ja  0, too
  # http://veterinaryrecord.bmj.com/content/176/10/254.long  1/22
  # https://www.bayer-chikusan.jp/research-pdf/douyaku-71.pdf 24 / 129. The viral load of the dam has significant association.
  # 0-1% or 10+%
  # http://veterinaryrecord.bmj.com/content/176/10/254.long 10% in ial, 50% in ipl (Used in this simulation)

  param$prob_vert_inf_ial <- (4 + 5) / 95
  param$prob_vert_inf_ipl <- (10 + 4) / 29

  # TODO: check
  # Piper CE. et al. Postnatal and prenatal transmission of the bovine leukemia virus under natural conditions. Journal of the National Cancer Institute. 1979, 62, 165-168.


  ## infection_introduced ----


  ## infection_comranch ----


  ## artificial_insemination ----


  # First AI after delivery
  # From Gyugun Kentei Seisekihyo (H25-29) by Hokkaido Rakuno Kentei Kensa Kyokai (HRK)
  # The date of the first AI after a delivery of PREVIOUS year
  # (because the data of the current year is only known from Feb to Dec)
  mean_date_start_ai <- c(88, 88, 88, 88, 89) / 30
  lims_date_start_ai <- set_param(param_farm$mean_day_start_ai,
                                  c(min(mean_date_start_ai),
                                    max(mean_date_start_ai)))
  # TODO: It's assumed that 95% of cows start AI within one month
  param$sd_date_start_ai <- set_param(param_farm$sd_day_start_ai / 30,
                                      1 / qnorm(0.975))
  param$mean_date_start_ai <- runif(1, min = lims_date_start_ai[1],
                                    max = lims_date_start_ai[2])


  # First AI for heifer
  mean_age_first_ai <- c(427, 427, 435, 432) / 365 * 12  # NOTE: From Gyugun Kentei Seisekihyo by HRK
  lims_age_first_ai <- set_param(param_farm$mean_age_first_ai,
                                 c(min(mean_age_first_ai), max(mean_age_first_ai)))
  # TODO: It's assumed that 95% of cows will get pregnant within one month
  param$sd_age_first_ai <- set_param(param_farm$sd_age_first_ai, 1 / qnorm(0.975))
  param$mean_age_first_ai <- runif(1, min = lims_age_first_ai[1],
                                   max = lims_age_first_ai[2])
  param$lower_lim_first_ai <- 12


  # Detection of heats
  prop_heat_detected <- c(0.60, 0.60, 0.60, 0.59, 0.59)  # Probability of detection of heat from Nenkan Kentei Seiseki from HRK (H23-28)
  param$prob_heat_detected <- set_param(param_farm$prop_heat_detected,
                                        runif(1, min = min(prop_heat_detected),
                                              max = max(prop_heat_detected)))


  # Proportion of success of the first AI
  # From Gyugun Kentei Seisekihyo by HRK
  # (because the data of the current year is only known from Feb to Dec)
  prop_first_ai_success <- c(0.32, 0.34, 0.34, 0.33, 0.35)
  param$prob_first_ai_success <- runif(1,
                                       min(prop_first_ai_success),
                                       max(prop_first_ai_success))


  # Proportion of success of AI after the first
  #
  # p1 + (E(p2) - p2) * (1 - p1) = mean_ai
  # (p1: the prob in which the first AI successes; p2: the prop in which AI after the first successes)
  # The probability in which the AI after the first successes
  mean_ai <- c(2.4, 2.3, 2.3, 2.3, 2.3)  # Mean of the number of AI conducted
  prop_ai_success <- (1 - param$prob_first_ai_success) / (mean_ai - 1)
  lims_ai_success <- c(min(prop_ai_success), max(prop_ai_success))
  param$prob_ai_success <- runif(1, min(prop_ai_success), max(prop_ai_success))


  # Heat cycle
  # https://doi.org/10.1017/S1751731118001830
  param$sd_heat <- 1  # SD of length of heat cycle (days)


  ## milking_stage ----


  # Length of milking period
  # TODO: Use farm_parameter
  length_milking <- set_param(param_farm$days_milking, 363) / 30
  param$lower_lim_dried <- length_milking %/% 1
  param$prop_dried_shorter <- 1 - length_milking %% 1


  ## reproduction ----

  # Sex ratio and probability to be twins
  # From Gyugun Kentei Seiseki Matome by HRK
  prop_m <- c(0.4655, 0.4643, 0.4616, 0.4545, 0.4552, 0.4531)
  prop_f <- c(0.4350, 0.4357, 0.4393, 0.4485, 0.4548, 0.4585)
  prop_mm <- c(0.0080, 0.0082, 0.0076, 0.0074, 0.0076, 0.0075)
  prop_ff <- c(0.0076, 0.0076, 0.0074, 0.0073, 0.0074, 0.0070)
  prop_fm <- c(0.0137, 0.0140, 0.0135, 0.0134, 0.0134, 0.0123)
  prop_twin <- c(0.0294, 0.0298, 0.0285, 0.0282, 0.0284, 0.0268)

  ## Probability to be twins ----
  ratio_twin <- prop_twin / (prop_m + prop_f + prop_twin)
  lims_twin <- c(min(ratio_twin), max(ratio_twin))
  param$prob_twin <- runif(1, min = lims_twin[1], max = lims_twin[2])

  ## Sex ratio ----
  sex_ratio_f <- prop_f / (prop_m + prop_f)
  lims_female <- set_param(param_farm$prop_female,
                           c(min(sex_ratio_f), max(sex_ratio_f)))
  param$prob_female <- runif(1, min = lims_female[1], max = lims_female[2])

  ## Sex ratio for twins ----
  sex_ratio_mm <- prop_mm / (prop_mm + prop_ff + prop_fm)
  sex_ratio_ff <- prop_ff / (prop_mm + prop_ff + prop_fm)
  lims_mm <- c(min(sex_ratio_mm), max(sex_ratio_mm))
  lims_ff <- c(min(sex_ratio_ff), max(sex_ratio_ff))

  if (!is.na(param_farm$prop_female)) {
    if (length(param_farm$prop_female) == 1) {
      prop_female <- c(param_farm$prop_female, param_farm$prop_female)
    }
    tend_mm <- sex_ratio_mm / (prop_m ^ 2)
    tend_ff <- sex_ratio_ff / (prop_f ^ 2)
    lims_mm <- c((1 - prop_female[2]) ^ 2 * min(tend_mm),
                 (1 - prop_female[1]) ^ 2 * max(tend_mm))
    lims_ff <- c(prop_female[1] ^ 2 * min(tend_ff),
                 prop_female[2] ^ 2 * max(tend_ff))
    if (lims_mm[2] + lims_ff[2] > 1) {
      lims_mm <- c((1 - prop_female[2]) ^ 2, (1 - prop_female[1]) ^ 2)
      lims_ff <- c(prop_female[1] ^ 2, prop_female[2] ^ 2)
    }
  }

  prob_mm <- runif(1, min = lims_mm[1], max = lims_mm[2])
  prob_ff <- runif(1, min = lims_ff[1], max = lims_ff[2])
  param$probs_sex_pairs <- c(prob_mm, 1 - prob_mm - prob_ff, prob_ff)


  # Failure of delivery (stillbirth/abortion)
  param$prob_sb_1 <- runif(1, min = 0.0834, max = 0.1070)  # Parity = 1 (Heifer)
  param$prob_sb_2 <- runif(1, min = 0.0473, max = 0.0563)  # 2
  param$prob_sb_3 <- runif(1, min = 0.0487, max = 0.0572)  # 3
  param$prob_sb_4 <- runif(1, min = 0.0526, max = 0.0604)  # 4
  param$prob_sb_5 <- runif(1, min = 0.0582, max = 0.0620)  # >5


  ## longevity ----

  ## Longevity ----
  # See preps/Parameters_age_distribution.Rmd

  # No. of slaughtered Holstein females in Hokkaido
  n_slaughtered <- c(81580, 80220, 81597, 81632, 81377)
  # No. of died Holstein females in Hokkaido
  n_died <- c(63361, 62949, 65395, 66143, 63437)
  prop_died <- n_died / (n_slaughtered + n_died)
  lims_died <- set_param(param_farm$prop_died,
                         c(min(prop_died), max(prop_died)))
  param$prob_died <- runif(1, min = lims_died[1], max = lims_died[2])

  # Death
  param_die <- list(
    prop = c(0.1781149, 0.19206973, 0.18295625, 0.18357473, 0.17160985),
    e_rate = c(0.69504391, 0.66565927, 0.62963687, 0.55892655, 0.62194143),
    g_shape = c(3.94254619, 4.0635499, 4.11764786, 4.11193613, 4.06322732),
    g_rate = c(0.06274259, 0.06433635, 0.06515513, 0.06596917, 0.06591569)
  )
  param_die_min <- sapply(param_die, min)
  param_die_max <- sapply(param_die, max)
  param_die_set <- runif(4, min = param_die_min, max = param_die_max)
  param$die_prop <- param_die_set[1]
  param$die_e_rate <- param_die_set[2]
  param$die_g_shape <- param_die_set[3]
  param$die_g_rate <- param_die_set[4]

  # Slaughtering
  param_slaughtered <- list(
    shape = c(5.207747, 5.172914, 5.182164, 4.918844, 5.134622),
    rate = c(0.07337904, 0.07226869, 0.07165169, 0.0678387, 0.06992455)
  )
  param_slaughtered_min <- sapply(param_slaughtered, min)
  param_slaughtered_max <- sapply(param_slaughtered, max)
  param_slaughtered_set <- runif(2,
                                 min = param_slaughtered_min,
                                 max = param_slaughtered_max)
  param$slaughter_shape <- param_slaughtered_set[1]
  param$slaughter_rate <- param_slaughtered_set[2]

  param <- c(modification, param)
  param <- param[!duplicated(names(param))]

  return(param)
}


#' Calculate parameters based on other parameters
#'
#' - `param_output_filename`: Name of a file to which output simulation parameters.
#' - `herd_size_limits`: Lower and upper limits of the number of cattle should be kept in the herd.
#' - `prob_rep`: The result of [set_prob_rep()]. The probability that a newborn female calf will be a replacement cow.
#' - `graze_cows`: Whether cows are grazed or not.
#'
#' Parameters processed by [process_param()] are deteministic. Parameters calculated by [calc_param()] are stochastic.
#'
#' @param setup_cows_res A result of [setup_cows()].
#' @param param_simulation See [param_simulation].
#' @param param_farm See [param_farm].
#'
#' @return A list of calculated parameters.
#' @export
process_param <- function(setup_cows_res, param_simulation, param_farm) {
  herd_size <- sum(setup_cows_res$init_cows$is_owned, na.rm = T)

  list(
    param_output_filename = paste0("param_", param_simulation$output_filename),
    herd_size_limits = if (!anyNA(param_farm$capacity_in_head)) {
        param_farm$capacity_in_head
      } else if (!anyNA(param_farm$capacity_as_ratio)) {
        herd_size * capacity_as_ratio
      } else {
        herd_size * c(0.9, 1.1)
      },
    prob_rep = set_prob_rep(
      setup_cows_res$init_cows[stage %in% c("milking", "dry"), .N],
      param_farm),
    graze_cows = anyNA(param_farm$hours_grazing)
  )
}
# TODO: Is this function really necessary?

