# Parameters get from papers, reports, experiments, etc. are set in this simulation.
# 文献や実験を参考に設定したパラメーターは全てこのファイルに記述する。

# TODO: シミュレーションをどういうパラメーターで行ったか、どこかに出力するようにしよう


## ---- set_param(parameter, default) ----
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
  # TODO: ↓これ消そう
  #' @note This function is used to find this environment (= the environment test_params defined). Be careful to use the function in the other file.
  if (anyNA(parameter)) {  # Much faster than (is.na(parameter[1]))
    parameter <- default
  } else if (length(parameter) != length(default)) {
    parameter <- rep(parameter, length.out = length(default))
  }
  return(parameter)
}


## ---- calculate_parameters ----
#' Calculate parameters necessary to the simulation.
#' 
#' Calculate parameters to the simulation and overwrite the default setting if necessary.  
#' `calc_params_once()` calculate parameters at the start of the whole simulations. At that moment, it returns an empty list.  
#' `calc_params_repeatedly()` calculate paramaters at the start of the each iteration of the simulation.
#' 
#' @param modification A list like `list(parameter_name_to_overwrite = new_parameter)`.
#' @param i_simulation The iteration indicator of the simulation.
#' 
#' @rdname calc_params
#' @return An empty list (`calc_params_once()`) or an parameter list (`calc_params_repeatedly()`)
#' @export
calc_params_once <- function(modification = NULL) {
  # TODO: このfunctionの必要性を検討した上で、不必要なら消す。
  params_initial <- list(
  )
  
  if (!is.null(modification)) {
    params_initial <- c(modification, params_initial)
    params_initial <- params_initial[!duplicated(names(params_initial))]
  }
  return(params_initial)
}

#' @rdname calc_params
#' @export
calc_params_repeatedly <- function(modification = NULL, i_simulation = NULL) {
  
  
  ## infection_status_change ----
  # Changes of infection status  感染ステータスの変化
  # Memo: We don't need to consider infection from rectal palpation here,
  #   because ordinal farms are assumed to change gloves for each rectal palpation.
  #   直検による感染をprob_ialから引く必要はないと考える。
  #   一般的な農場は直検手袋を毎回交換していると想定しているため。
  #   TODO: 直検手袋交換しない農場も多いらしい。要検討。
  
  params_disease_progress <- list(
    mean_prop_ial_period = 0.3,
    sd_prop_ial_period = (0.3 - 0.2) / qnorm(0.975),
    # Length of periods from PL to EBL is not well known.
    # PL発症からEBLまでの期間については不明 (数ヶ月～数年) 。
    shape = 3.3, 
    scale = 7.8
    # Periods until an infected cattle develops EBL: rweibull(n, shape, scale) * 12 (Tsutsui et al, 2016)
  )
  dp_mean_prop_ial_period <- params_disease_progress$mean_prop_ial_period
  dp_sd_prop_ial_period <- params_disease_progress$sd_prop_ial_period
  dp_shape <- params_disease_progress$shape
  dp_scale <- params_disease_progress$scale
  
  ## Probabilities of disease progress
  # Proportion of ial cattle which develops ipl
  prob_develop_ipl <- 0.3  # 30% of infected cattle develops ipl (OIE terrestrial manual)
  probs_develop_ipl <- c(prob_develop_ipl, 1 - prob_develop_ipl)
  # Proportion of blv infected cattle which develops ebl
  prob_develop_ebl <- 0.014 / prob_develop_ipl  # 1.4% of BLV-infected cattle develops ebl (Tsutsui et al, 2016)
  probs_develop_ebl <- c(prob_develop_ebl, 1 - prob_develop_ebl)
  
  # Probability that an BLV-infected cow is detected  感染牛が発見される確率
  # Tsutsui et al. (2015) より、発症牛の39.7%が発見される。39.7%はステージが「Ial->Ipl」に移ったその月に発見され、残りは死亡までの間発見されないと仮定する。（参考論文でもそうなってる？のと、発症から発見までの期間のデータなんてあるはずがない）。
  prob_ebl_detected <- rnorm(1, mean = 0.397, 
                             sd = (0.397 - 0.358) / qnorm(0.975))
  probs_ebl_detected <- c(prob_ebl_detected, 1 - prob_ebl_detected)
  
  # Months until EBL cattle die  発症牛が死亡するまでの月数
  rate_ebl_die <- 1 / 2  # Average months until die is set to 2m
  
  ## infection_insects ----
  
  ## Probability of infection by bloodsucking insects per month per cattle
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
  insects_pressure <- 1
  
  ## infection_contact ----
  
  
  ## infection_needles ----
  
  # Infection by using same needles among infected and non-infected cattle
  # 注射針の使い回しによる感染
  
  # 1ヶ月ごとの感染確率
  # TODO: とりあえず適当
  change_needles <- set_param(PARAMS_FARM$change_needles, T)
  prob_inf_needles <- ifelse(change_needles, 0, 0.005)
  probs_inf_needles <- c(prob_inf_needles, 1 - prob_inf_needles)
  
  ## infection_rp ----
  # Infection by rectal palpation  直検による感染
  # 4回の直検により3/4頭がBLV+に。(Kohara, 2016)
  # 各直検による感染確率はそれぞれ一定であるとして、1回ごとの感染確率を計算する。
  # [Lack of evidence of transmission of bovine leukemia virus by rectal palpation of dairy cows. - PubMed - NCBI](https://www.ncbi.nlm.nih.gov/pubmed/2557314) では0.034となってる
  
  # 直検1回ごとの感染確率
  change_gloves <- set_param(PARAMS_FARM$change_gloves, T)
  prob_inf_rp <- ifelse(change_gloves, 0, 1 - (1 - 3 / 4) ^ (1 / 4))
  probs_inf_rp <- c(prob_inf_rp, 1 - prob_inf_rp)
  
  
  ## infection_vertical ----
  
  # Vertical infection  垂直感染
  ## Probability of vertical infection for calves born from BLV-infected dams
  # Vet Microbiol. 2002 Jan 23;84(3):275-82.  Vertical transmission of bovine leukemia virus and bovine immunodeficiency virus in dairy cattle herds.  これだと垂直感染は0だな
  # https://www.jstage.jst.go.jp/article/jvma1951/34/9/34_9_423/_article/-char/ja  これも垂直感染は0
  # これだと 1/22
  # http://veterinaryrecord.bmj.com/content/176/10/254.long これだと 24 / 129。母牛のウイルス量が有意に関連。
  # 0-1% か 10数% の二極
  # https://www.bayer-chikusan.jp/research-pdf/douyaku-71.pdf
  # http://veterinaryrecord.bmj.com/content/176/10/254.long を参考に、ialでは約10%、 iplでは約50%とする。
  
  prob_vert_inf_ial <- (4 + 5) / 95
  probs_vert_inf_ial <- c(prob_vert_inf_ial, 1 - prob_vert_inf_ial)
  prob_vert_inf_ipl <- (10 + 4) / 29
  probs_vert_inf_ipl <- c(prob_vert_inf_ipl, 1 - prob_vert_inf_ipl)
  
  # 海外のだったけどよさそうだった
  # Piper CE. et al. Postnatal and prenatal transmission of the bovine leukemia virus under natural conditions. Journal of the National Cancer Institute. 1979, 62, 165-168.
  
  
  ## infection_introduced ----
  
  
  ## infection_comranch ----
  
  
  ## artificial_insemination ----
  
  
  # First AI after delivery  分娩後初回授精開始日
  # 北酪検、牛群検定成績表(H25-29)より
  # 分娩後初回授精開始日の「前年」成績 （当年成績は2-12月までの結果しかないので）
  date_start_ai <- c(88, 88, 88, 88, 89) / 30
  lims_date_start_ai <- set_param(PARAMS_FARM$day_start_ai[1],
                                  c(min(date_start_ai), max(date_start_ai)))
  # 前後1月以内に95%が授精を開始するとする
  sd_date_start_ai <- set_param(PARAMS_FARM$day_start_ai[2] / 30,
                                1 / qnorm(0.975))
  mean_date_start_ai <- runif(1, min = lims_date_start_ai[1],
                              max = lims_date_start_ai[2])
  
  
  # First AI for heifer  未経産牛の初回授精月齢
  age_first_ai <- c(427, 427, 435, 432) / 365 * 12  #TODO: 牛群検定成績まとめより
  lims_age_first_ai <- set_param(PARAMS_FARM$age_first_ai[1],
                                 c(min(age_first_ai), max(age_first_ai)))
  # 前後1ヶ月以内に95%が授精するとする
  sd_age_first_ai <- set_param(PARAMS_FARM$age_first_ai[2], 1 / qnorm(0.975))
  mean_age_first_ai <- runif(1, min = lims_age_first_ai[1],
                             max = lims_age_first_ai[2])
  lower_lim_first_ai <- 12
  
  
  # Detection of heats  発情発見率
  prop_heat_detected <- c(0.60, 0.60, 0.60, 0.59, 0.59)  # 発情発見率
  # 北海道酪農検定検査協会、年間検定成績より (H23-28)
  prob_heat_detected <- set_param(PARAMS_FARM$prop_heat_detected,
                                  runif(1, min = min(prop_heat_detected),
                                        max = max(prop_heat_detected)))
  probs_heat_detected <- c(prob_heat_detected, 1 - prob_heat_detected)
  
  
  # Proportion of success of the first AI  初回授精受胎率
  # 北酪検、牛群検定成績表(H25-29)より
  # 初回授精受胎率の「前年」成績 （当年成績は2-12月までの結果しかないので）
  prop_first_ai_success <- c(0.32, 0.34, 0.34, 0.33, 0.35)
  prob_first_ai_success <- runif(1, min(prop_first_ai_success),
                                 max(prop_first_ai_success))
  probs_first_ai_success <- c(prob_first_ai_success, 1 - prob_first_ai_success)
  
  
  # Proportion of success of AI after the first  2回目以降の授精の受胎率
  # 初回授精成功率をp1、2回目以降の成功率をp2とするとき、
  # p1 + (E(p2) - p2) * (1 - p1) = mean_ai
  # 2回目以降の受胎率
  mean_ai <- c(2.4, 2.3, 2.3, 2.3, 2.3)  # 平均授精回数
  prop_ai_success <- (1 - prob_first_ai_success) / (mean_ai - 1)
  lims_ai_success <- c(min(prop_ai_success), max(prop_ai_success))
  prob_ai_success <- runif(1, min(prop_ai_success), max(prop_ai_success))
  probs_ai_success <- c(prob_ai_success, 1 - prob_ai_success)
  
  
  # Heat cycle  発情周期
  # https://doi.org/10.1017/S1751731118001830
  sd_heat <- 1  # SD of length of heat cycle (days)
  
  
  ## milking_stage ----
  
  
  # Length of milking period  搾乳日数
  # TODO: 搾乳を長く続ける農家さんもいると思うのでそれを反映させたい
  length_milking <- set_param(PARAMS_FARM$days_milking, 363) / 30
  lower_lim_dried <- length_milking %/% 1
  prop_dried_shorter <- 1 - length_milking %% 1
  
  
  ## reproduction ----
  
  # Sex ratio and probability to be twins  性比
  # 牛群検定成績まとめより
  prop_m <- c(0.4655, 0.4643, 0.4616, 0.4545, 0.4552, 0.4531)
  prop_f <- c(0.4350, 0.4357, 0.4393, 0.4485, 0.4548, 0.4585)
  prop_mm <- c(0.0080, 0.0082, 0.0076, 0.0074, 0.0076, 0.0075)
  prop_ff <- c(0.0076, 0.0076, 0.0074, 0.0073, 0.0074, 0.0070)
  prop_fm <- c(0.0137, 0.0140, 0.0135, 0.0134, 0.0134, 0.0123)
  prop_twin <- c(0.0294, 0.0298, 0.0285, 0.0282, 0.0284, 0.0268)
  
  ## Probability to be twins  双子になる確率
  ratio_twin <- prop_twin / (prop_m + prop_f + prop_twin)
  lims_twin <- c(min(ratio_twin), max(ratio_twin))
  prob_twin <- runif(1, min = lims_twin[1], max = lims_twin[2])
  probs_twin <- c(prob_twin, 1 - prob_twin)
  
  ## Sex ratio
  sex_ratio_f <- prop_f / (prop_m + prop_f)
  lims_female <- set_param(PARAMS_FARM$prop_female,
                           c(min(sex_ratio_f), max(sex_ratio_f)))
  prob_female <- runif(1, min = lims_female[1], max = lims_female[2])
  probs_female <- c(prob_female, 1 - prob_female)
  
  ## Sex ratio for twins
  sex_ratio_mm <- prop_mm / (prop_mm + prop_ff + prop_fm)
  sex_ratio_ff <- prop_ff / (prop_mm + prop_ff + prop_fm)
  lims_mm <- c(min(sex_ratio_mm), max(sex_ratio_mm))  # TODO: 同上
  lims_ff <- c(min(sex_ratio_ff), max(sex_ratio_ff))  # TODO: 同上
  
  if (!is.na(PARAMS_FARM$prop_female)) {
    if (length(PARAMS_FARM$prop_female) == 1) {
      prop_female <- c(PARAMS_FARM$prop_female, PARAMS_FARM$prop_female)
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
  probs_sex_pairs <- c(prob_mm, 1 - prob_mm - prob_ff, prob_ff)
  
  
  # Failure of delivery (stillbirth/abortion)  死流産
  prob_sb_1 <- runif(1, min = 0.0834, max = 0.1070)  # Parity = 1 (Heifer)
  prob_sb_2 <- runif(1, min = 0.0473, max = 0.0563)  # 2
  prob_sb_3 <- runif(1, min = 0.0487, max = 0.0572)  # 3
  prob_sb_4 <- runif(1, min = 0.0526, max = 0.0604)  # 4
  prob_sb_5 <- runif(1, min = 0.0582, max = 0.0620)  # >5
  
  
  ## longevity ----
  
  ## Longevity  寿命（死亡・と畜）
  # See preps/Parameters_age_distribution.Rmd
  
  # No. of slaughtered Holstein females in Hokkaido
  n_slaughtered <- c(81580, 80220, 81597, 81632, 81377)
  # No. of died Holstein females in Hokkaido
  n_died <- c(63361, 62949, 65395, 66143, 63437)
  prop_died <- n_died / (n_slaughtered + n_died)
  lims_died <- set_param(PARAMS_FARM$prop_died,
                         c(min(prop_died), max(prop_died)))
  prob_died <- runif(1, min = lims_died[1], max = lims_died[2])
  
  # Death  死亡
  params_die <- list(
    prop = c(0.1781149, 0.19206973, 0.18295625, 0.18357473, 0.17160985),
    e_rate = c(0.69504391, 0.66565927, 0.62963687, 0.55892655, 0.62194143),
    g_shape = c(3.94254619, 4.0635499, 4.11764786, 4.11193613, 4.06322732),
    g_rate = c(0.06274259, 0.06433635, 0.06515513, 0.06596917, 0.06591569)
  )
  params_die_min <- sapply(params_die, min)
  params_die_max <- sapply(params_die, max)
  params_die_set <- runif(4, min = params_die_min, max = params_die_max)
  pd_prop <- params_die_set[1]
  pd_e_rate <- params_die_set[2]
  pd_g_shape <- params_die_set[3]
  pd_g_rate <- params_die_set[4]
  
  # Slaughtering  と畜
  params_slaughtered <- list(
    shape = c(5.207747, 5.172914, 5.182164, 4.918844, 5.134622),
    rate = c(0.07337904, 0.07226869, 0.07165169, 0.0678387, 0.06992455)
  )
  params_slaughtered_min <- sapply(params_slaughtered, min)
  params_slaughtered_max <- sapply(params_slaughtered, max)
  params_slaughtered_set <- runif(2,
                                  min = params_slaughtered_min,
                                  max = params_slaughtered_max)
  ps_shape <- params_slaughtered_set[1]
  ps_rate <- params_slaughtered_set[2]

  ## set_params_initial ----
  
  params_initial <- list(

    mean_prop_ial_period = dp_mean_prop_ial_period,
    sd_prop_ial_period = dp_sd_prop_ial_period,
    dp_shape = dp_shape, 
    dp_scale = dp_scale,
    # params_disease_progress = params_disease_progress,
    
    prob_develop_ipl = prob_develop_ipl,
    prob_develop_ebl = prob_develop_ebl,
    prob_ebl_detected = prob_ebl_detected,
    rate_ebl_die = rate_ebl_die,
    insects_pressure = insects_pressure,
    prob_inf_needles = prob_inf_needles,
    prob_inf_rp = prob_inf_rp,
    prob_vert_inf_ial = prob_vert_inf_ial,
    prob_vert_inf_ipl = prob_vert_inf_ipl,
    sd_date_start_ai = sd_date_start_ai,
    mean_date_start_ai = mean_date_start_ai,
    sd_age_first_ai = sd_age_first_ai,
    mean_age_first_ai = mean_age_first_ai,
    lower_lim_first_ai = lower_lim_first_ai,
    prob_heat_detected = prob_heat_detected,
    prob_first_ai_success = prob_first_ai_success,
    prob_ai_success = prob_ai_success,
    sd_heat = sd_heat,
    lower_lim_dried = lower_lim_dried,
    prop_dried_shorter = prop_dried_shorter,
    prob_twin = prob_twin,
    prob_female = prob_female,
    prob_mm = prob_mm,
    prob_ff = prob_ff,
    prob_sb_1 = prob_sb_1,
    prob_sb_2 = prob_sb_2,
    prob_sb_3 = prob_sb_3,
    prob_sb_4 = prob_sb_4,
    prob_sb_5 = prob_sb_5,
    prob_died = prob_died,
    
    pd_prop = pd_prop,
    pd_e_rate = pd_e_rate,
    pd_g_shape = pd_g_shape,
    pd_g_rate = pd_g_rate,
    # params_die = params_die,
    
    ps_shape = ps_shape,
    ps_rate = ps_rate
    # params_slaughtered = params_slaughtered
  )
  
  if (!is.null(modification)) {
    if (length(modification) == 1) {
      params_initial <- c(modification, params_initial)
    } else {
      params_initial <- c(modification[[i_simulation]], params_initial)
    }
    params_initial <- params_initial[!duplicated(names(params_initial))]
  }
  
  params_initial$params_disease_progress <- list(
    mean_prop_ial_period = params_initial$mean_prop_ial_period,
    sd_prop_ial_period = params_initial$sd_prop_ial_period,
    shape = params_initial$dp_shape, 
    scale = params_initial$dp_scale
  )
  # params_initial$probs_xxx <- c(params_initial$prob_xxx, 1 - params_initial$prob_xxx)
  params_initial$probs_develop_ipl <- c(params_initial$prob_develop_ipl, 1 - params_initial$prob_develop_ipl)
  params_initial$probs_develop_ebl <- c(params_initial$prob_develop_ebl, 1 - params_initial$prob_develop_ebl)
  params_initial$probs_ebl_detected <- c(params_initial$prob_ebl_detected, 1 - params_initial$prob_ebl_detected)
  params_initial$probs_inf_insects_month <- params_initial$insects_pressure * probs_inf_insects_month
  params_initial$probs_inf_needles <- c(params_initial$prob_inf_needles, 1 - params_initial$prob_inf_needles)
  params_initial$probs_inf_rp <- c(params_initial$prob_inf_rp, 1 - params_initial$prob_inf_rp)
  params_initial$probs_vert_inf_ial <- c(params_initial$prob_vert_inf_ial, 1 - params_initial$prob_vert_inf_ial)
  params_initial$probs_vert_inf_ipl <- c(params_initial$prob_vert_inf_ipl, 1 - params_initial$prob_vert_inf_ipl)
  params_initial$probs_heat_detected <- c(params_initial$prob_heat_detected, 1 - params_initial$prob_heat_detected)
  params_initial$probs_first_ai_success <- c(params_initial$prob_first_ai_success, 1 - params_initial$prob_first_ai_success)
  params_initial$probs_ai_success <- c(params_initial$prob_ai_success, 1 - params_initial$prob_ai_success)
  params_initial$probs_twin <- c(params_initial$prob_twin, 1 - params_initial$prob_twin)
  params_initial$probs_female <- c(params_initial$prob_female, 1 - params_initial$prob_female)
  params_initial$probs_sex_pairs <- c(params_initial$prob_mm, 1 - params_initial$prob_mm - params_initial$prob_ff, params_initial$prob_ff)
  params_initial$params_die <- list(
    1, 2, 3,
    c(params_initial$pd_prop, params_initial$pd_e_rate, params_initial$pd_g_shape, params_initial$pd_g_rate)
  )
  params_initial$params_slaughtered <- list(
    1, 2, 3,
    c(params_initial$ps_rate, params_initial$ps_shape)
  )
  
  return(params_initial)
}


## ---- test_params ----

#' Only used in test
#' @note DON'T TOUCH THIS though it seems meaningless. This variable is only used in test runs and parameters are automatically set.
# test_params <- list() 
# TODO: パッケージにするなら削除

