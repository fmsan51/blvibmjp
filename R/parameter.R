#' Parameters about a simulation which should be set by users
#'
#' - `simulation_length`: Length of simulation (months). (default: 60)
#' - `n_simulation`: The number of simulation. (default: 1)
#' - `simulation_start`: The month simulation starts (1 = Jan, 2 = Feb, ...). (default: 1)
#' - `input_csv`: Path to a csv file which contains cattle information.
#' - `output_dir`: Directory to output files. (default: data/output)
#' - `output_filename`: The name of the output files. (default: "simulation")
#'
#' @seealso [param_farm] [param_group]
#' @export
param_simulation <- list(
  # TODO: 必要なパラメータが設定されてるか確認するためのfunctionを作成する
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
#' - `n_introduced` c(calf, heifer, delivered): The number of introduced cows for five years. (default: c(0, 0, 0)) 
#' - `days_qualantine`: Length of qualantine period (in days) for introduced cows in which introduced cows contacted no cows but introduced ones at the same time. (default: 0)
#' - `change_needles` (logical): whether use one needles for one cow. (default: TRUE)
#' - `change_gloves` (logical): whether use one glove for one cow for rectal palpation. (default: TRUE)
#' - `days_milking`: Length of milking period (in days). (default: average in Hokkaido)
#'
#' @seealso [param_simulation] [param_group] [calc_param]
#' @export
param_farm <- list(
  # TODO: farm と herdの表記ゆれ修正
  # TODO: 必要なパラメータが設定されてるか確認するためのfunctionを作成する
  csv = NA,  # TODO: これ本当にここで設定するべきか？ 他の部分に移動させる
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
  # TODO: capacityが現在の頭数を上回ってるor下回ってるようなら警告
  # TODO: capacity_in_headとcapacity_as_ratioが両方セットされたら警告


  change_needles = NA,
  # TODO: これpropとして設定できるようになりたい
  change_gloves = F,
  # TODO: これpropとして設定できるようになりたい

  days_milking = NA
)


#' Parameters about barns which should be set by users
#'
#' - `calf_area_id`: Numeric vector. Set the `area_id`(s) for newborn calves specified in `[area_table]`.
#' - `calf_area_priority`: Specify priority for calf areas. `NA` is allowed. See explanation of `priority` in `[area_table]` for meaning of `priority`.
#' - `qualantine_area_id`: Numeric vector. Set the `area_id` for introduced cows. `NA` is allowed if the herd never introduce cows.
#' - `xy_chamber`: The number of chambers per lane and the number of lanes.
#' - `is_calf_separated`: Whether each calf is separated.
#' - `is_milking_dry_separated`: (For a farm which miling and dry cows are kept in same tie-stall barn,) whether dry cows are separated from milking cows.
#'
#' @seealso [param_simulation] [param_farm]
#' @export
param_group <- list(
  # TODO: 必要なパラメータが設定されてるか確認するためのfunctionを作成する
  # TODO: barnとgroupの構造についてもっと検討、それに応じてこのparameter listも検討
  calf_area_id = NA_real_,
  calf_area_priority = NA_real_,
  qualantine_area_id = NA_real_,
  xy_chamber = NA,
  is_calf_separated = NA,
  
  # (For a farm which miling and dry cows are kept in same tie-stall barn,)
  # are dry cows are separated from milking cows?
  # (NA means milking cows and dry cows are in different barns.)
  # (搾乳牛と乾乳牛を同一のタイストール牛舎で飼育している農場について、)
  # 搾乳牛と乾乳牛を分けているか？
  is_milking_dry_separated = NA
  # 搾乳牛と乾乳牛が同一牛舎で飼養されている、
  # かつ搾乳牛と乾乳牛の飼養エリアを分けている、
  # かつ牛舎がタイストールである場合、
  # 牛房IDの前半11/13を搾乳牛エリア、牛房IDの後半2/13を乾乳牛エリアとする。
  # (H27牛群検定成績まとめより、北海道の平均搾乳日数が363日、乾乳日数が65日のため。)

  # Does a farm decide the chamber layout of milking cows based on lactation stage?
  # is_layout_on_stage = F,

  # For a farm which decide the chamber layout based on parity, specify how cows are grouped.
  # For example, c(1, 2, 4) means there are three groups (1, 2-3, >4).
  # layout_on_parity = NA

  # 乾乳牛の群分け？ 育成牛と乾乳牛の混合？
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
  # TODO: 各種parameterについて本当にこれでいいか再検討
  param <- list()

  ## infection_status_change ----
  # Changes of infection status  感染ステータスの変化
  # Memo: We don't need to consider infection from rectal palpation here,
  #   because ordinal farms are assumed to change gloves for each rectal palpation.
  #   直検による感染をprob_ialから引く必要はないと考える。
  #   一般的な農場は直検手袋を毎回交換していると想定しているため。
  #   TODO: 直検手袋交換しない農場も多いらしい。要検討。

  param$mean_prop_ial_period <-  0.3
  param$sd_prop_ial_period <- (0.3 - 0.2) / qnorm(0.975)
  # Length of periods from PL to EBL is not well known.
  # PL発症からEBLまでの期間については不明 (数ヶ月～数年) 。
  param$ebl_progress_shape <- 3.3
  param$ebl_progress_scale <- 7.8
  # Periods until an infected cattle develops EBL: rweibull(n, shape, scale) * 12 (Tsutsui et al, 2016)

  ## Probabilities of disease progress ----
  # Proportion of ial cattle which develops ipl
  prob_develop_ipl <- 0.3  # 30% of infected cattle develops ipl (OIE terrestrial manual)
  param$probs_develop_ipl <- c(prob_develop_ipl, 1 - prob_develop_ipl)
  # Proportion of blv infected cattle which develops ebl
  prob_develop_ebl <- 0.014 / prob_develop_ipl  # 1.4% of BLV-infected cattle develops ebl (Tsutsui et al, 2016)
  param$probs_develop_ebl <- c(prob_develop_ebl, 1 - prob_develop_ebl)

  # Probability that an BLV-infected cow is detected  感染牛が発見される確率
  # Tsutsui et al. (2015) より、発症牛の39.7%が発見される。39.7%はステージが「Ial->Ipl」に移ったその月に発見され、残りは死亡までの間発見されないと仮定する。（参考論文でもそうなってる？のと、発症から発見までの期間のデータなんてあるはずがない）。
  prob_ebl_detected <- rnorm(1, mean = 0.397,
                             sd = (0.397 - 0.358) / qnorm(0.975))
  param$probs_ebl_detected <- c(prob_ebl_detected, 1 - prob_ebl_detected)

  # Months until EBL cattle die  発症牛が死亡するまでの月数
  param$rate_ebl_die <- 1 / 2  # Average months until die is set to 2m

  ## infection_insects ----

  ## Probability of infection by bloodsucking insects per month per cattle ----
  # Read preps/Parameters_num_insects.Rmd
  param$probs_inf_insects_month <- c(0, 0, 0, 0,  # Jan to Apr
                                     0.0028,  # May
                                     0.0107,  # Jun
                                     0.0146,  # Jul
                                     0.0063,  # Aug
                                     0.0406,  # Sep
                                     0.0140,  # Oct
                                     0.0001,  # Nov
                                     0        # Dec
                                     )
  param$insects_pressure <- 1

  ## infection_contact ----


  ## infection_needles ----

  # Infection by using same needles among infected and non-infected cattle
  # 注射針の使い回しによる感染

  # 1ヶ月ごとの感染確率
  # TODO: とりあえず適当
  change_needles <- set_param(param_farm$change_needles, T)
  prob_inf_needles <- ifelse(change_needles, 0, 0.005)
  param$probs_inf_needles <- c(prob_inf_needles, 1 - prob_inf_needles)

  ## infection_rp ----
  # Infection by rectal palpation  直検による感染
  # 4回の直検により3/4頭がBLV+に。(Kohara, 2016)
  # 各直検による感染確率はそれぞれ一定であるとして、1回ごとの感染確率を計算する。
  # [Lack of evidence of transmission of bovine leukemia virus by rectal palpation of dairy cows. - PubMed - NCBI](https://www.ncbi.nlm.nih.gov/pubmed/2557314) では0.034となってる

  # 直検1回ごとの感染確率
  change_gloves <- set_param(param_farm$change_gloves, T)
  prob_inf_rp <- ifelse(change_gloves, 0, 1 - (1 - 3 / 4) ^ (1 / 4))
  param$probs_inf_rp <- c(prob_inf_rp, 1 - prob_inf_rp)


  ## infection_vertical ----

  # Vertical infection  垂直感染
  ## Probability of vertical infection for calves born from BLV-infected dams ----
  # Vet Microbiol. 2002 Jan 23;84(3):275-82.  Vertical transmission of bovine leukemia virus and bovine immunodeficiency virus in dairy cattle herds.  これだと垂直感染は0だな
  # https://www.jstage.jst.go.jp/article/jvma1951/34/9/34_9_423/_article/-char/ja  これも垂直感染は0
  # これだと 1/22
  # http://veterinaryrecord.bmj.com/content/176/10/254.long これだと 24 / 129。母牛のウイルス量が有意に関連。
  # 0-1% か 10数% の二極
  # https://www.bayer-chikusan.jp/research-pdf/douyaku-71.pdf
  # http://veterinaryrecord.bmj.com/content/176/10/254.long を参考に、ialでは約10%、 iplでは約50%とする。

  prob_vert_inf_ial <- (4 + 5) / 95
  param$probs_vert_inf_ial <- c(prob_vert_inf_ial, 1 - prob_vert_inf_ial)
  prob_vert_inf_ipl <- (10 + 4) / 29
  param$probs_vert_inf_ipl <- c(prob_vert_inf_ipl, 1 - prob_vert_inf_ipl)

  # 海外のだったけどよさそうだった
  # Piper CE. et al. Postnatal and prenatal transmission of the bovine leukemia virus under natural conditions. Journal of the National Cancer Institute. 1979, 62, 165-168.


  ## infection_introduced ----


  ## infection_comranch ----


  ## artificial_insemination ----


  # First AI after delivery  分娩後初回授精開始日
  # 北酪検、牛群検定成績表(H25-29)より
  # 分娩後初回授精開始日の「前年」成績 （当年成績は2-12月までの結果しかないので）
  mean_date_start_ai <- c(88, 88, 88, 88, 89) / 30
  lims_date_start_ai <- set_param(param_farm$mean_day_start_ai,
                                  c(min(mean_date_start_ai),
                                    max(mean_date_start_ai)))
  # 前後1月以内に95%が授精を開始するとする
  # TODO: ↑ここ要検討
  param$sd_date_start_ai <- set_param(param_farm$sd_day_start_ai / 30,
                                      1 / qnorm(0.975))
  param$mean_date_start_ai <- runif(1, min = lims_date_start_ai[1],
                                    max = lims_date_start_ai[2])


  # First AI for heifer  未経産牛の初回授精月齢
  mean_age_first_ai <- c(427, 427, 435, 432) / 365 * 12  #TODO: 牛群検定成績まとめより
  lims_age_first_ai <- set_param(param_farm$mean_age_first_ai,
                                 c(min(mean_age_first_ai), max(mean_age_first_ai)))
  # 前後1ヶ月以内に95%が授精するとする
  param$sd_age_first_ai <- set_param(param_farm$sd_age_first_ai, 1 / qnorm(0.975))
  param$mean_age_first_ai <- runif(1, min = lims_age_first_ai[1],
                                   max = lims_age_first_ai[2])
  param$lower_lim_first_ai <- 12


  # Detection of heats  発情発見率
  prop_heat_detected <- c(0.60, 0.60, 0.60, 0.59, 0.59)  # 発情発見率
  # 北海道酪農検定検査協会、年間検定成績より (H23-28)
  prob_heat_detected <- set_param(param_farm$prop_heat_detected,
                                  runif(1, min = min(prop_heat_detected),
                                        max = max(prop_heat_detected)))
  param$probs_heat_detected <- c(prob_heat_detected, 1 - prob_heat_detected)


  # Proportion of success of the first AI  初回授精受胎率
  # 北酪検、牛群検定成績表(H25-29)より
  # 初回授精受胎率の「前年」成績 （当年成績は2-12月までの結果しかないので）
  prop_first_ai_success <- c(0.32, 0.34, 0.34, 0.33, 0.35)
  prob_first_ai_success <- runif(1,
                                 min(prop_first_ai_success),
                                 max(prop_first_ai_success))
  param$probs_first_ai_success <- c(prob_first_ai_success,
                                    1 - prob_first_ai_success)


  # Proportion of success of AI after the first  2回目以降の授精の受胎率
  # 初回授精成功率をp1、2回目以降の成功率をp2とするとき、
  # p1 + (E(p2) - p2) * (1 - p1) = mean_ai
  # 2回目以降の受胎率
  mean_ai <- c(2.4, 2.3, 2.3, 2.3, 2.3)  # 平均授精回数
  prop_ai_success <- (1 - prob_first_ai_success) / (mean_ai - 1)
  lims_ai_success <- c(min(prop_ai_success), max(prop_ai_success))
  prob_ai_success <- runif(1, min(prop_ai_success), max(prop_ai_success))
  param$probs_ai_success <- c(prob_ai_success, 1 - prob_ai_success)


  # Heat cycle  発情周期
  # https://doi.org/10.1017/S1751731118001830
  param$sd_heat <- 1  # SD of length of heat cycle (days)


  ## milking_stage ----


  # Length of milking period  搾乳日数
  # TODO: 搾乳を長く続ける農家さんもいると思うのでそれを反映させたい
  length_milking <- set_param(param_farm$days_milking, 363) / 30
  param$lower_lim_dried <- length_milking %/% 1
  param$prop_dried_shorter <- 1 - length_milking %% 1


  ## reproduction ----

  # Sex ratio and probability to be twins  性比
  # 牛群検定成績まとめより
  prop_m <- c(0.4655, 0.4643, 0.4616, 0.4545, 0.4552, 0.4531)
  prop_f <- c(0.4350, 0.4357, 0.4393, 0.4485, 0.4548, 0.4585)
  prop_mm <- c(0.0080, 0.0082, 0.0076, 0.0074, 0.0076, 0.0075)
  prop_ff <- c(0.0076, 0.0076, 0.0074, 0.0073, 0.0074, 0.0070)
  prop_fm <- c(0.0137, 0.0140, 0.0135, 0.0134, 0.0134, 0.0123)
  prop_twin <- c(0.0294, 0.0298, 0.0285, 0.0282, 0.0284, 0.0268)

  ## Probability to be twins  双子になる確率 ----
  ratio_twin <- prop_twin / (prop_m + prop_f + prop_twin)
  lims_twin <- c(min(ratio_twin), max(ratio_twin))
  prob_twin <- runif(1, min = lims_twin[1], max = lims_twin[2])
  param$probs_twin <- c(prob_twin, 1 - prob_twin)

  ## Sex ratio ----
  sex_ratio_f <- prop_f / (prop_m + prop_f)
  lims_female <- set_param(param_farm$prop_female,
                           c(min(sex_ratio_f), max(sex_ratio_f)))
  prob_female <- runif(1, min = lims_female[1], max = lims_female[2])
  param$probs_female <- c(prob_female, 1 - prob_female)

  ## Sex ratio for twins ----
  sex_ratio_mm <- prop_mm / (prop_mm + prop_ff + prop_fm)
  sex_ratio_ff <- prop_ff / (prop_mm + prop_ff + prop_fm)
  lims_mm <- c(min(sex_ratio_mm), max(sex_ratio_mm))  # TODO: 同上
  lims_ff <- c(min(sex_ratio_ff), max(sex_ratio_ff))  # TODO: 同上

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


  # Failure of delivery (stillbirth/abortion)  死流産
  param$prob_sb_1 <- runif(1, min = 0.0834, max = 0.1070)  # Parity = 1 (Heifer)
  param$prob_sb_2 <- runif(1, min = 0.0473, max = 0.0563)  # 2
  param$prob_sb_3 <- runif(1, min = 0.0487, max = 0.0572)  # 3
  param$prob_sb_4 <- runif(1, min = 0.0526, max = 0.0604)  # 4
  param$prob_sb_5 <- runif(1, min = 0.0582, max = 0.0620)  # >5


  ## longevity ----

  ## Longevity  寿命（死亡・と畜） ----
  # See preps/Parameters_age_distribution.Rmd

  # No. of slaughtered Holstein females in Hokkaido
  n_slaughtered <- c(81580, 80220, 81597, 81632, 81377)
  # No. of died Holstein females in Hokkaido
  n_died <- c(63361, 62949, 65395, 66143, 63437)
  prop_died <- n_died / (n_slaughtered + n_died)
  lims_died <- set_param(param_farm$prop_died,
                         c(min(prop_died), max(prop_died)))
  param$prob_died <- runif(1, min = lims_died[1], max = lims_died[2])

  # Death  死亡
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

  # Slaughtering  と畜
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
#' - `is_ts`: Wheter barns are tie-stall.
#' - `ts_group`: Barn ID of tie-stall barns.
#' - `is_md_separated_in_ts`: Whether milking and dry cows are separated.
#' - `capacity`: The result of [set_capacity()].
#' - `prob_rep`: The result of [set_prob_rep()]. The probability that a newborn female calf will be a replacement cow.
#'
#' Parameters processed by [process_param()] are deteministic. Parameters calculated by [calc_param()] are stochastic.
#'
#' @param setup_cows_res A result of [setup_cows()].
#' @param param_simulation See [param_simulation].
#' @param param_farm See [param_farm].
#' @param param_group See [param_group].
#'
#' @return A list of calculated parameters.
#' @export
process_param <- function(setup_cows_res,
                          param_simulation, param_farm, param_group) {
  list(
    param_output_filename = paste0("param_", param_simulation$output_filename),
    is_ts = is_ts(param_group),
    ts_group = which(is_ts(param_group)),
    # is_md_separated_in_ts = is_md_separated_in_ts(param_group),
    capacity = set_capacity(setup_cows_res$init_last_cow_id, param_farm),
    prob_rep = set_prob_rep(
      setup_cows_res$init_cows[stage %in% c("milking", "dry"), .N],
      param_farm)
  )
}

