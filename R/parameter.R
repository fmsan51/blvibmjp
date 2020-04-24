#' Parameters about a simulation which should be set by users
#'
#' Parameters about a simulation
#'
#' Parameters related with simulation:
#'
#' - `simulation_length`: Length of simulation (months). (default: 60)
#' - `n_simulation`: The number of simulation. (default: 1)
#' - `simulation_start`: The month simulation starts (1 = Jan, 2 = Feb, ...). (default: 1)
#' - `input_csv`: Path to a csv file which contains cattle information. Can be `NA` if `processed_data` is supplied to [simulate_blv_spread()].
#' - `output_dir`: Directory to output files. (default: data/output)
#' - `output_filename`: The name of the output files. (default: "simulation")
#'
#' Parameters depends on a farm:
#'
#' - `prop_female` (0-1): Proportion of female of newborns. (default: average in Hokkaido)
#' - `prop_replacement` (0-1): Proportion of calves to be replacements in female newborns. (default: average in Hokkaido)
#' - `prop_died` (1-0): Proportion of dead cows in removed cows (Died / (Died + Slaughtered)). (default: average in Hokkaido)
#' - `prop_heat_detected` (0-1): Proportion of detected heats in total heats. (default: average in Hokkaido)
#' - `calving_interval`: Calving interval in day. (default: average in Hokkaido)
#' - `age_first_ai`: Age (in month) of the first AI for heifers. (default: average in Hokkaido)
#' - `day_start_ai`: Day of first AI after a delivery. (default: average in Hokkaido)
#' - `capacity_in_head` c(lower, upper): Lower/upper limit of the herd size. Set either this or `capacity_as_ratio` below.
#' - `capacity_as_ratio` c(lower, upper): Lower/upper limit of the herd as ratio to the initial herd size (lower limit = `lower * initial_herd_size`, upper limit = `upper * initial_herd_size`). Set either this or `capacity_in_head` above. When both of `capacity_in_head` and `capacity_as_ratio` is NA, `capacity_as_ratio` is set to `c(0.9, 1.1)`.
#' - `prob_seroconversion_in_pasture` (0-1): probability of seroconversion when a cow is send to a communal pasture. (default: 0.5)
#' - `n_introduced` c(calf, heifer, delivered): The number of introduced cows for five years. (default: c(0, 0, 0))
#' - `days_qualantine`: Length of qualantine period (in days) for introduced cows in which introduced cows contacted no cows but introduced ones at the same time. (default: 0)
#' - `control_insects` (logical or 0-1): wheter conduct control measures against insects. When specified by a number from 0 to 1, it means that the number of bloodsucking insects decrease to this proportion (i.e., `control_insects = 0.8` means that the number of insects becomes 80%). When `TRUE`, it is assumed that insects in a farm decrease to 50%. (default: FALSE)
#' - `change_needles` (logical): whether use one needles for one cow. (default: TRUE)
#' - `change_gloves` (logical): whether use one glove for one cow for rectal palpation. (default: TRUE)
#' - `feed_raw_colostrum` (logical): wheter feed non-pasteurized colostrum milk to newborn calves. (default: FALSE)
#' - `cull_infected_cows` ("no"/"all"/"highrisk"): Whether cull infected cows. "all" means cull infected cows even when they do not show simptoms and "highrisk" means cull PL or EBL cows only. Culling is conducted when a new female calf is born and you can set frequency of culling by `cull_frequency` described next.
#' - `culling_frequency` (numeric): This parameter can be set to specify the frequency of culling to cull an infected cow to every $n$ (= `culling_frequency`) female calves.
#' - `test_frequency` (1-12): Frequency of BLV tests in a year. Only integers can be set.
#' - `test_method`: Method of BLV test. Character indicating test method (immunodiffusion/ELISA/PHA/nested PCR/real-time PCR) or a vector consisted of two numerics which mean sensitivity and specificity of the test.
#' - `days_milking`: Length of milking period (in days). (default: average in Hokkaido)
#'
#' @export
param <- list(
  # TODO: The function to confirm the necessary parameters are set or not
  simulation_length = 60,
  n_simulation = 1,
  simulation_start = 1,
  input_csv = NA_character_,
  output_dir = "data/output",
  output_filename = "simulation",

  prop_female = NA,
  prop_replacement = NA,
  prop_died = NA,

  # Reproductivity parameters
  prop_heat_detected = NA,
  calving_interval = NA,
  age_first_delivery = NA,
  days_open = NA,
  days_milking = NA,
  age_first_ai = NA,
  day_start_ai = NA,

  n_introduced = c(0, 0, 0),
  days_qualantine = 0,
  capacity_in_head = NA,
  capacity_as_ratio = NA,
  # TODO: Warn if capacity doesn't follow current number of cows
  # TODO: Warn if both of capacity_in_head and capacity_as_ratio are set

  prob_seroconversion_in_pasture = 0.5,
  # Probability of seroconversion in communal pastures
  # Reports about seroconversion in communal pastures
  # - Niigata: 60%, 47%, 50%, 51% (2013-2016) -> 5.6% (2017)
  #   Ohkatsu et al, 2018. https://www.pref.niigata.lg.jp/uploaded/attachment/26756.pdf (https://www.pref.niigata.lg.jp/sec/chikusan/1356889019293.html)
  # - Tohoku: 0-11.5% (2006-2008)
  #   Terada, 2009. Tohoku Nogyo Kenkyu, 62, 87-88. http://www.naro.affrc.go.jp/org/tarc/to-noken/DB/DATA/062/062-087.pdf (http://www.naro.affrc.go.jp/org/tarc/to-noken/DB/issue/no_062.html)
  # - Yamagata: 0% (with countermeasures)
  #   Watanabe and Kugota, 2016. p2 of http://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/attach/pdf/index-3.pdf (https://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/index.html)
  # - Yamagata: 0% (with countermeasures)
  #   Tuchiya and Kiguchi, 2014. https://www.pref.yamagata.jp/ou/sogoshicho/okitama/325048/gakujyusujyoho/gyohatu/H26-2.pdf (https://www.pref.yamagata.jp/ou/sogoshicho/okitama/325048/gakujyusujyoho/gyosekihappyokai.html)
  #   Kiguchi and Mori, 2015. https://www.pref.yamagata.jp/ou/sogoshicho/okitama/325048/gakujyusujyoho/gyohatu/H27-1.pdf (https://www.pref.yamagata.jp/ou/sogoshicho/okitama/325048/gakujyusujyoho/gyosekihappyokai.html)
  # - Yamagata: 51.8% (2007) -> 22.3% (2015), 49.4% (2007) -> 1.5% (2015)
  #   Ohkawara and Morita, 2016. p3 of http://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/attach/pdf/index-3.pdf (https://www.pref.yamagata.jp/ou/sogoshicho/okitama/325048/gakujyusujyoho/gyosekihappyokai.html)
  #   (Change of prevalence) https://www.pref.yamagata.jp/ou/sogoshicho/shonai/337051/2019eiseidayori/2019No.9.pdf (https://www.pref.yamagata.jp/ou/sogoshicho/shonai/337051/2019kachikueiseidayori.html)
  # - Ibaraki: 97.4% (Apr. 2014) -> 33.3 (2015)
  #   Kodato et al, 2016. p1-7 of https://www.pref.ibaraki.jp/nourinsuisan/chikusan/kachiku/kaho/documents/endai1.pdf (https://www.pref.ibaraki.jp/nourinsuisan/chikusan/kachiku/kaho/h27gyouseki.html)
  # - Nagano: 7.9% (2014), 4.5% (2015) (with countermeasures)
  #   Yahikozawa, 2016. p58 of http://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/attach/pdf/index-3.pdf (https://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/index.html)
  # - Iwate: 0% (with countermeasures)
  #   Kitagawa and Takeda, 2011. p56 of https://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/pdf/syoroku_52.pdf (https://www.maff.go.jp/j/syouan/douei/katiku_yobo/k_kaho/index.html)

  control_insects = F,
  change_needles = T,
  change_gloves = T,
  # TODO: Make it to prop
  feed_raw_colostrum = F,

  cull_infected_cows = "no",
  culling_frequency = 1,
  test_frequency = 0,
  test_method = NA
)


#' Overwrite default parameters with herd specific parameters
#'
#' It's used to overwrite default parameters (averages of Hokkaido or Japan) with farm specific parameters.
#' When `parameter` is not NA (= when a farm specific parameter was set), it returns the farm specific parameters.
#' When `parameter` is NA (= when a farm specific parameter was not set), it returns dafault parameters.
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
#' @param param See [param].
#' @param modification A list used to overwrite the defaut parameter like `list(parameter_name = new_value, ...)`.
#'
#' @seealso [calc_param_pre()] [calc_param_both()]
#' @return A parameter list.
calc_param <- function(param, modification = NULL) {
  res <- list()

  ## infection_status_change ----
  # Changes of infection status
  # Memo: We don't need to consider infection from rectal palpation here,
  #   because ordinal farms are assumed to change gloves for each rectal palpation.
  #   TODO: Some farmers don't change gloves. consider again.

  res$prop_ial_period <-  0.3
  # TODO: Reconsider this parameter
  # Length of periods from PL to EBL is not well known. (several months to years)
  res$ebl_progress_shape <- 3.3
  res$ebl_progress_scale <- 7.8
  # Periods until an infected cattle develops EBL: rweibull(n, shape, scale) * 12 - Tsutsui et al, 2016. https://doi.org/10.1016/j.prevetmed.2015.11.019


  ## Probabilities of disease progress ----
  # NOTE: prob_develop_ipl and prob_develop_ebl are calculated in calc_param_both().

  # Probability that an BLV-infected cow is detected
  # 39.7% of infected cows are detected. This 39.7% are assumed to found at the month in which infection stage moved from Ial to Ipl.
  # (Because they assumed as the same and there is no data about the length of period from clinical onset to detection.)
  # - Tsutsui et al, 2016. https://doi.org/10.1016/j.prevetmed.2015.11.019
  res$prob_ebl_detected <- rnorm(1, mean = 0.397, sd = (0.397 - 0.358) / q975)

  # Months until EBL cattle die
  res$rate_ebl_die <- 1 / 2  # Average months until die is set to 2m
  # TODO: temporary, just by inspiration


  ## blv_test ----
  # Test frequency
  if (param$test_frequency == 0) {
    res$test_months <- numeric(0)
  } else {
    test_months <- ceiling(12 / param$test_frequency * 1:param$test_frequency)
    test_months <- (test_months + param$simulation_start + 11) %% 12 + 1
    # Add 11 to avoid the first test occurs at i_month = 1
    # when test_frequency = 1
    # Add 1 because n %% 12 contains 0
    res$test_months <- test_months
  }

  # A list of BLV test methods available in Japan was obtained from here:
  # Mekata, 2016. https://doi.org/10.4190/jjlac.6.221
  if (anyNA(param$test_method)) {
    # Not is.na() because length of test_method can be two
    res$test_sensitivity <- 0
    res$test_specificity <- 0
  } else if (param$test_method == "immunodiffusion") {
    res$test_sensitivity <- 0.981
    res$test_specificity <- 0.967
    # Molloy et al, 1990. https://doi.org/10.1016/0166-0934(90)90086-U
  } else if (param$test_method == "ELISA") {
    # Monti et al, 2005. https://doi.org/10.1177%2F104063870501700507
    estimates <- data.table(se_est = c(0.994, 0.994, 0.976, 0.893),
                            se_lwr = c(0.982, 0.980, 0.951, 0.857),
                            se_upr = c(1.000, 0.999, 0.993, 0.927),
                            sp_est = c(0.985, 0.987, 0.970, 0.849),
                            sp_lwr = c(0.962, 0.958, 0.927, 0.784),
                            sp_upr = c(1.000, 0.998, 0.996, 0.913))
    estimates[, `:=`(se_se = mean(c(se_upr - se_est, se_est - se_lwr)) / q975,
                     sp_se = mean(c(sp_upr - sp_est, sp_est - sp_lwr)) / q975)]
    estimate <- estimates[sample.int(.N, 1), ]
    res$test_sensitivity <- rnorm(1, estimates$se_est, estimates$se_se)
    res$test_specificity <- rnorm(1, estimates$sp_est, estimates$sp_se)
  } else if (param$test_method == "PHA") {
    estimates <- data.table(sensitivity = numeric(2),
                            specificity = numeric(2))
    # Calculate treating a result of nested PCR as gold standard
    # Abe et al, 2012. http://www.pref.tochigi.lg.jp/g68/documents/4abe.pdf (http://www.pref.tochigi.lg.jp/g68/jigyougaiyou23.html)
    estimates[1, `:=`(sensitivity = 1,
                      specificity = 10 / (10 + 16))]
    # Division of Pathology, Kyoto City Institute of Health and Environmental Sciences, 2006. Annual Report of Kyoto City Institute of Health and Environmental Sciences, 73. https://www.city.kyoto.lg.jp/hokenfukushi/cmsfiles/contents/0000118/118365/O6.pdf (https://www.city.kyoto.lg.jp/hokenfukushi/page/0000118365.html)
    estimates[2, `:=`(sensitivity = 0.909,
                      specificity = 0.984)]
    estimate <- estimates[sample.int(.N, 1), ]
    res$test_sensitivity <- estimate$sensitivity
    res$test_specificity <- estimate$specificity
  } else if (param$test_method == "nested PCR") {
    # Monti et al, 2005. https://doi.org/10.1177%2F104063870501700507
    estimates <- data.table(se_est = c(0.928, 0.929, 0.916),
                            se_lwr = c(0.901, 0.895, 0.878),
                            se_upr = c(0.956, 0.955, 0.945),
                            sp_est = c(0.767, 0.770, 0.755),
                            sp_lwr = c(0.696, 0.694, 0.674),
                            sp_upr = c(0.828, 0.836, 0.828))
    estimates[, `:=`(se_se = mean(c(se_upr - se_est, se_est - se_lwr)) / q975,
                     sp_se = mean(c(sp_upr - sp_est, sp_est - sp_lwr)) / q975)]
    estimate <- estimates[sample.int(.N, 1), ]
    res$test_sensitivity <- rnorm(1, estimates$se_est, estimates$se_se)
    res$test_specificity <- rnorm(1, estimates$sp_est, estimates$sp_se)
  } else if (param$test_method == "real-time PCR") {
    # Calculate treating a result of nested PCR as gold standard
    estimates <- data.table(sensitivity = numeric(3),
                            specificity = numeric(3))
    # Hayashi et al, 2016. https://www.pref.aomori.lg.jp/soshiki/kenmin/ao-kaho/files/27gyohatu_BLV.pdf (https://www.pref.aomori.lg.jp/soshiki/kenmin/ao-kaho/chosashiken.html)
    estimates[1, `:=`(sensitivity = 4 / 5,
                      specificity = 1)]
    # Miyoshi and Okazaki, 2017. http://www.pref.tochigi.lg.jp/g68/documents/28-08.pdf (http://www.pref.tochigi.lg.jp/g68/jigyougaiyou28.html)
    estimates[2, `:=`(sensitivity = (27 + 1) / (27 + 1 + 1 + 1),
                      specificity = 1)]
    # Soda et al, 2015. Saitamaken Chosa Kenkyu Seiseki Houkokusho, 56. https://www.pref.saitama.lg.jp/a0908/gyousekihappyou/documents/h26_09.pdf (https://www.pref.saitama.lg.jp/a0908/gyousekihappyou/gyousekihappyou.html)
    estimates[3, `:=`(sensitivity = 1,
                      specificity = 1)]
    estimate <- estimates[sample.int(.N, 1), ]
    res$test_sensitivity <- estimate$sensitivity
    res$test_specificity <- estimate$specificity
  } else {
    res$test_sensitivity <- param$test_method[1]
    res$test_specificity <- param$test_method[2]
  }


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
  control_insects <- param$control_insects
  if (is.logical(control_insects)) {
    insects_pressure <- fifelse(control_insects, 0.5, 1)
  } else {
    insects_pressure <- control_insects
  }
  res$probs_inf_insects_month <- probs_inf_insects_month * insects_pressure

  ## infection_tiestall ----
  ## infection_neighbor ----

  res$prob_inf_tiestall_baseline <- res$probs_inf_insects_month
  res$hr_having_infected_neighbor <-
    exp(rnorm(1, mean = 2.52, sd = 0.73))
  # Kobayashi et al, 2015. https://doi.org/10.1292/jvms.15-0007


  ## infection_free ----
  free_pressure <-
    rnorm(1, mean = 1.19, sd = mean(c(1.19 - 1.01, 1.39 - 1.19)) / q975)
  # Kobayashi et al, 2014. https://doi.org/10.1016/j.rvsc.2013.11.014
  res$prob_inf_free <- res$probs_inf_insects_month * free_pressure

  res$average_prop_inf_in_free <-
    rnorm(1, mean = 0.409, sd = mean(c(0.409 - 0.404, 0.414 - 0.409)) / q975)
  # Murakami et al, 2013. https://doi.org/10.1292/jvms.12-0374


  ## infection_needles ----

  # Infection by using same needles among infected and non-infected cattle
  # Infection probability per day
  # TODO: temporary, just by inspiration
  res$prob_inf_needles <- fifelse(param$change_needles, 0, 0.001)

  ## infection_rp ----
  # Infection by rectal palpation
  # 3/4 cows get infected by 4 rectal palpations right after infected cows - Kohara, Konnai and Onuma, 2016. http://doi.org/10.14943/jjvr.54.1.25
  # The probability of infection per try is calculated.
  # The probability is 0.034 in Lassauzet, Thurmond and Walton, 1989. https://www.ncbi.nlm.nih.gov/pubmed/2557314

  # 直検1回ごとの感染確率
  # Kohara, Konnai and Onuma, 2016.
  # TODO: Recalculate
  res$prob_inf_rp <- fifelse(param$change_gloves, 0, 1 - (1 - 3 / 4) ^ (1 / 4))


  ## infection_vertical ----

  # Vertical infection
  # 0% - Meas et al, 2002. https://doi.org/10.1016/s0378-1135(01)00458-8
  # 0% - Kajikawa et al, 1981. https://doi.org/10.12935/jvma1951.34.423
  # 18.6% (24/129: 10.8% (14) - transplacental: 7.7% (10) - birth canal) The viral load of the dam has significant association. - Mekata et al, 2015. http://dx.doi.org/10.1136/vr.102464

  res$prob_vert_inf_ial <- (4 + 5) / 95
  res$prob_vert_inf_ipl <- (10 + 4) / 29

  ## infection_by_colostrum ----

  # Probability of infection by feeding raw colostrum milk of BLV-infected dams
  # Frequency of infection by colostrum may be smaller than that by contact. 3/(25+16) cavles raised on colostrum and milk from BLV-infected dams get infected within 5 months. - Ferrer and Piper, 1981. https://www.ncbi.nlm.nih.gov/pubmed/6272983
  # Probability of BLV infection after freeze-thaw can be considered as 0 - Kanno et al, 2014. https://doi.org/10.1292/jvms.13-0253
  res$prob_inf_colostrum <- fifelse(param$feed_raw_colostrum, 3 / (25 + 26), 0)


  ## infection_introduced ----


  ## artificial_insemination ----
  # First AI after delivery
  # From Gyugun Kentei Seisekihyo (H25-29) by Hokkaido Rakuno Kentei Kensa Kyokai (HRK)
  # The date of the first AI after a delivery of PREVIOUS year
  # (because the data of the current year is only known from Feb to Dec)
  date_start_ai <- c(88, 88, 88, 88, 89) / days_per_month
  lims_date_start_ai <- set_param(param$day_start_ai,
                                  c(min(date_start_ai), max(date_start_ai)))
  res$date_start_ai <-
    runif(1, min = lims_date_start_ai[1], max = lims_date_start_ai[2])


  # First AI for heifer
  # From Gyugun Kentei Seisekihyo by HRK
  age_first_ai <- c(427, 427, 435, 432) / days_per_month
  lims_age_first_ai <-
    set_param(param$age_first_ai, c(min(age_first_ai), max(age_first_ai)))
  res$age_first_ai <-
    runif(1, min = lims_age_first_ai[1], max = lims_age_first_ai[2])


  # Detection of heats
  prop_heat_detected <- c(0.60, 0.60, 0.60, 0.59, 0.59)  # Probability of detection of heat from Nenkan Kentei Seiseki from HRK (H23-28)
  res$prob_heat_detected <- set_param(param$prop_heat_detected,
                                        runif(1, min = min(prop_heat_detected),
                                              max = max(prop_heat_detected)))


  # Proportion of success of the first AI
  # From Gyugun Kentei Seisekihyo by HRK
  # (because the data of the current year is only known from Feb to Dec)
  prop_first_ai_success <- c(0.32, 0.34, 0.34, 0.33, 0.35)
  res$prob_first_ai_success <- runif(1,
                                       min(prop_first_ai_success),
                                       max(prop_first_ai_success))


  # Proportion of success of AI after the first
  #
  # p1 + (E(p2) - p2) * (1 - p1) = mean_ai
  # (p1: the prob in which the first AI successes; p2: the prop in which AI after the first successes)
  # The probability in which the AI after the first successes
  mean_ai <- c(2.4, 2.3, 2.3, 2.3, 2.3)  # Mean of the number of AI conducted
  prop_ai_success <- (1 - res$prob_first_ai_success) / (mean_ai - 1)
  lims_ai_success <- c(min(prop_ai_success), max(prop_ai_success))
  res$prob_ai_success <- runif(1, min(prop_ai_success), max(prop_ai_success))


  # Heat cycle
  # Martin et al, 2019. https://doi.org/10.1017/S1751731118001830
  res$mean_heat_0 <- 20.5  # Heifer
  res$mean_heat_1 <- 20.7  # Delivered
  res$sd_heat_0 <- 1.0  # Heifer
  res$sd_heat_1 <- 1.1  # Delivered


  ## milking_stage ----


  # Length of milking period
  res$length_milking <- set_param(param$days_milking, 363) / days_per_month


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
  res$prob_twin <- runif(1, min = lims_twin[1], max = lims_twin[2])

  ## Sex ratio ----
  sex_ratio_f <- prop_f / (prop_m + prop_f)
  lims_female <- set_param(param$prop_female,
                           c(min(sex_ratio_f), max(sex_ratio_f)))
  res$prob_female <- runif(1, min = lims_female[1], max = lims_female[2])

  ## Sex ratio for twins ----
  sex_ratio_mm <- prop_mm / (prop_mm + prop_ff + prop_fm)
  sex_ratio_ff <- prop_ff / (prop_mm + prop_ff + prop_fm)
  lims_mm <- c(min(sex_ratio_mm), max(sex_ratio_mm))
  lims_ff <- c(min(sex_ratio_ff), max(sex_ratio_ff))

  if (!is.na(param$prop_female)) {
    if (length(param$prop_female) == 1) {
      prop_female <-
        c(param$prop_female, param$prop_female)
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
  res$probs_sex_pairs <- c(prob_mm, 1 - prob_mm - prob_ff, prob_ff)


  # Failure of delivery (stillbirth/abortion)
  res$prob_sb_1 <- runif(1, min = 0.0834, max = 0.1070)  # Parity = 1 (Heifer)
  res$prob_sb_2 <- runif(1, min = 0.0473, max = 0.0563)  # 2
  res$prob_sb_3 <- runif(1, min = 0.0487, max = 0.0572)  # 3
  res$prob_sb_4 <- runif(1, min = 0.0526, max = 0.0604)  # 4
  res$prob_sb_5 <- runif(1, min = 0.0582, max = 0.0620)  # >5


  res <- c(res, calc_param_both(param))


  res <- c(modification, res)
  res <- res[!duplicated(names(res))]

  return(res)
}


#' Calculate parameters based on other parameters
#'
#' - `param_output_filename`: Name of a file to which output simulation parameters.
#' - `herd_size_limits`: Lower and upper limits of the number of cattle should be kept in the herd.
#' - `max_herd_size`: The maximum herd size allowed in a simulation. Used to reserve memory to store cow data while simulation.
#' - `init_max_cow_id`: The largest `cow_id` in `cows`.
#' - `prob_rep`: The result of [set_prob_rep()]. The probability that a newborn female calf will be a replacement cow.
#'
#' Parameters processed by [process_param()] are deteministic. Parameters calculated by [calc_param()] are stochastic.
#'
#' @param cows See [cow_table].
#' @param param See [param].
#'
#' @return A list of calculated parameters.
process_param <- function(cows, param) {
  if (!anyNA(param$capacity_in_head)) {
    herd_size_limits <- param$capacity_in_head
  } else if (!anyNA(param$capacity_as_ratio)) {
    herd_size_limits <- nrow(cows) * capacity_as_ratio
  } else {
    herd_size_limits <- nrow(cows) * c(0.9, 1.1)
  }
  res <- list(
    param_output_filename = paste0("param_", param$output_filename),
    herd_size_limits = herd_size_limits,
    max_herd_size = herd_size_limits[2] * 2,
    init_max_cow_id = max(cows$cow_id, na.rm = T),
    prob_rep = set_prob_rep(sum(cows$parity != 1, na.rm = T), param)
    )

  return(res)
}


#' Calculate parameters necessary to prepare_data()
#'
#' Calculate parameters which are used only in [prepare_data()] and overwrite the default setting if necessary.
#'
#' @param param See [param].
#' @param modification A list used to overwrite the defaut parameter like `list(parameter_name = new_value, ...)`.
#'
#' @seealso [calc_param()] [calc_param_both()]
#' @return A parameter list.
calc_param_pre <- function(param, modification = NULL) {
  res <- list()

  # Nyuken (H23-27)
  # calving_interval, age_first_delivery, months_open, months_milking is used only in prepare_cows()
  calving_interval <- c(432, 430, 432, 429, 427) / days_per_month
  res$calving_interval <- set_param(
    param$calving_interval / days_per_month,
    runif(1, min = min(calving_interval), max = max(calving_interval))
    )
  age_first_delivery <- c(25.2, 25.1, 25.0, 25.0, 24.8)
  res$age_first_delivery <- set_param(
    param$age_first_delivery,
    runif(1, min = min(age_first_delivery), max = max(age_first_delivery))
    )
  months_open <- c(160, 159, 159, 155, 154) / days_per_month
  res$months_open <- set_param(
    param$days_open / days_per_month,
    runif(1, min = min(months_open), max = max(months_open))
    )
  months_milking <- c(366, 363, 365, 364, 363) / days_per_month
  res$months_milking <- set_param(
    param$days_milking / days_per_month,
    runif(1, min = min(months_milking), max = max(months_milking))
    )

  res <- c(res, calc_param_both(param))

  res <- c(modification, res)
  res <- res[!duplicated(names(res))]

  return(res)
}


#' Calculate parameters used in both of prepare_data() and simulation
#'
#' Calculate parameters which are in both of [prepare_data()] and simulation
#'
#' @param param See [param].
#'
#' @seealso [calc_param()] [calc_param_pre()]
#' @return A parameter list.
calc_param_both <- function(param) {
  res <- list()

  ## Longevity ----
  # See preps/Parameters_age_distribution.Rmd

  # No. of slaughtered Holstein females in Hokkaido
  n_slaughtered <- c(81580, 80220, 81597, 81632, 81377)
  # No. of died Holstein females in Hokkaido
  n_died <- c(63361, 62949, 65395, 66143, 63437)
  prop_died <- n_died / (n_slaughtered + n_died)
  lims_died <- set_param(param$prop_died,
                         c(min(prop_died), max(prop_died)))
  res$prob_died <- runif(1, min = lims_died[1], max = lims_died[2])

  # Death
  param_die <- list(
    prop = c(0.1781149, 0.19206973, 0.18295625, 0.18357473, 0.17160985),
    e_rate = c(0.69504391, 0.66565927, 0.62963687, 0.55892655, 0.62194143),
    g_shape = c(3.94254619, 4.0635499, 4.11764786, 4.11193613, 4.06322732),
    g_rate = c(0.06274259, 0.06433635, 0.06515513, 0.06596917, 0.06591569)
  )
  param_die_min <- vapply(param_die, min, 1)
  param_die_max <- vapply(param_die, max, 1)
  param_die_set <- runif(4, min = param_die_min, max = param_die_max)
  res$die_prop <- param_die_set[1]
  res$die_e_rate <- param_die_set[2]
  res$die_g_shape <- param_die_set[3]
  res$die_g_rate <- param_die_set[4]

  # Slaughtering
  param_slaughtered <- list(
    shape = c(5.207747, 5.172914, 5.182164, 4.918844, 5.134622),
    rate = c(0.07337904, 0.07226869, 0.07165169, 0.0678387, 0.06992455)
  )
  param_slaughtered_min <- vapply(param_slaughtered, min, 1)
  param_slaughtered_max <- vapply(param_slaughtered, max, 1)
  param_slaughtered_set <- runif(2,
                                 min = param_slaughtered_min,
                                 max = param_slaughtered_max)
  res$slaughter_shape <- param_slaughtered_set[1]
  res$slaughter_rate <- param_slaughtered_set[2]


  ## Disease progress ----
  # Proportion of ial cattle which develops ipl
  res$prob_develop_ipl <- 0.3  # 30% of infected cattle develops ipl (OIE terrestrial manual)
  # Proportion of blv infected cattle which develops ebl
  res$prob_develop_ebl <- 0.014 / res$prob_develop_ipl  # 1.4% of BLV-infected cattle develops ebl - Tsutsui et al, 2016. https://doi.org/10.1016/j.prevetmed.2015.11.019


  return(res)
}


#' Validate parameters
#'
#' @param param See [param].
#'
#' @return A parameter list.
validate_param <- function(param) {
  if (param$simulation_length <= 0 | !is.wholenumber(param$simulation_length)) {
    stop("simulation_length must be a positive integer")
  }
  invisible(NULL)
}


#' Whether a newborn will be a replacement
#'
#' @param n_calves The number of newborns.
#' @param herd_size The current herd size.
#' @param n_delivered The number of delivered cows in a herd.
#' @param param See [param].
#' @param param_sim A list which combined [param], a result of [process_param()] and a result of [calc_param()].
#'
#' @name is_replacement
#' @references 「平成28年度 乳用種初生牛の経営に関する調査報告書」の「表40 調査対象経営の乳用種雌子牛の仕向け状況（規模別）」
#' @encoding UTF-8
#' @return A logical vector or a numeric value.
set_prob_rep <- function(n_delivered, param) {
  prob_rep <- if (!is.na(param$prop_replacement)) {
    param$prop_replacement
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

