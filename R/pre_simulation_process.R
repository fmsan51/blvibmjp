#' Process raw cow data to suitable form
#'
#' Transform an input csv into a suitable form, which is in a form of [cow_table].
#'
#' An input csv file can have following columns. The csv file must contain `age` column.
#'
#' - `cow_id`: It can be use-defined cow ID (not have to be a 9- or 10-digits code). If not set, `cow_id` is allocated sequencially (From 1 to the number of input cows).
#' - `age`: Age in month. Either one of `age` or `date_birth` must be set.
#' - `date_birth`: Birth day. If `age` is not set, `age` is calculated from this and `today` argument in the function.
#' - `sex`: One of "female", "male" or "freemartin". If not set, all cows are assumed to be "female".
#' - `is_replacement`: If a farm keeps non-replacement cows (e.g. a male newborn which will be send to a livestock market), set this parameter. If not set, `age` = 0 female cows are assumed to be a replacement according to `prop_replacement` parameter in `param_farm` and all male cows are assumed to be a non-replacement.
#' - `stage`, `parity`, `date_last_delivery`, `date_got_pregnant`, `date_dried`: If not set, they will be calculated in according to parameters related with reproduction in [param_farm].
#' - `is_to_test_pregnancy`: If not set, `FALSE` is set.
#' - `n_ai`: If not set, it is assumed to be 0.
#' - `infection_status`: At least one of this variable or `modify_prevalence` argument must be set. If `NA` is included, that cow is assumed to be a non-infected cow. When `modify_prevalence` is set, prevalence is modified to make prevalence equal to the value of `modify_prevalence`.
#' - `date_ial`, `date_ipl`, `date_ebl`: Specify the date when infection status was confirmed. If `NULL`, `0` is set.
#' - `area_id`: If not set, cows are divided to four areas based on `stage`. If `NA`s are included, cows are allocated to areas in which cows with the same stage and parity are kept.
#' - `month_in_area`: If not set, it is assumed to be 0. This parameter has no effect when a farm does not use `month_in_area` as a criteria for area movement. See [area_table] for detail of area movement.
#' - `chamber_id`: If not set, it is randomly allocated later in [setup_cows()].
#' - `is_isolated`: If not set, `FALSE` is set.
#'
#' For further detail of each variables, see [cow_table].
#'
#' @param csv File path of input csv file. See the Detail section to know about form of input csv.
#' @param data data.frame as a input instead of `csv`. See the Detail section to know about form of input data.
#' @param today A Date class object or a character in "YYYY/MM/DD" format. The date used to calculate `age` from `date_birth` when `age` is not set. `today` is automatically calculated when both of `age` and `date_birth` are filled and `date_birth` is in form of Date rather than number (which means that the cow was born $n$ month ago) and the value passed to this argument is ignored.
#' @param create_calf_data logical or a numeric. Create data for young cows based on cow data in the input. Set this argument when the input does not contain data for young cows (e.g. when you use Nyuken data). If `TRUE`, create cows younger than the youngest cows in the input. If a numeric is set, create cows equal to or younger than that age.
#' @param modify_prevalence double (0-1). If not `NULL`, modify `infection_status` column to make prevalence to the specified value.
#' @param param_calculated The result from [calc_param].
#' @param n_chambers Set if a farm owns tie-stall barns. Specify the number of chambers in each tie-stall barn like `c(area_id = the_number_of_chambers_in_the_area, ...)`.
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_BLV_spread()].
process_raw_csv <- function(csv, data = NULL, today = Sys.Date(), 
                            create_calf_data = F, modify_prevalence = NULL,
                            param_calculated = calc_param(param_farm,
                                                          param_simulation),
                            n_chambers = NULL) {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  cols_in_input <- intersect(colnames(a_new_calf), colnames(input))
  n_cows <- nrow(input)
  cows <- a_new_calf[rep(1, n_cows), ]
  cows[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

  # First, calculate values in columns which users can specify.
  if (anyNA(cows$cow_id)) {
    is_na <- is.na(cows$cow_id)
    n_na <- sum(is_na)
    cows$cow_id[is_na] <- setdiff(seq_len(n_cows), cows$cow_id)[seq_len(n_na)]
  }

  # Convert date variables from character to Date
  if (is.character(today)) {
    today <- ymd(today)
  }
  date_vars <- grep("^date_", colnames(a_new_calf), value = T)
  is_date_as_chr <- cows[, vapply(.SD, is.character, T), .SDcols = date_vars]
  date_as_chr <- date_vars[is_date_as_chr]
  # if statement here is to prevent warning due to 0-length RHS of :=
  if (length(date_as_chr) != 0) {
    cows[, (date_as_chr) := cows[, lapply(.SD, ymd), .SDcols = date_as_chr]]
  }

  is_na_age <- is.na(cows$age)
  is_na_date_birth <- is.na(cows$date_birth)
  if (any(!is_na_age & !is_na_date_birth) & !is.numeric(cows$date_birth)) {
    today <- mean(cows$date_birth + months(cows$age), na.rm = T)
  }
  if (anyNA(cows$age)) {
    if (is.numeric(cows$date_birth)) {
      cows$age[is_na_age] <- (cows$date_birth * -1)[is_na_age]
    } else {
      cows$age[is_na_age] <-
        (interval(cows$date_birth, today) %/% months(1))[is_na_age]
    }
  }
  if (anyNA(cows$date_birth)) {
    if (!is.numeric(cows$date_birth)) {
      cows$date_birth <- interval(today, cows$date_birth) %/% months(1)
    }
    cows$date_birth[is_na_date_birth] <- (cows$age * -1)[is_na_date_birth]
  }
  is_date_as_num <- cows[, vapply(.SD, is.numeric, T), .SDcols = date_vars]
  date_not_as_num <- date_vars[!is_date_as_num]
  # if statement here is to prevent warning due to 0-length RHS of :=
  if (length(date_not_as_num) != 0) {
  cows[, (date_not_as_num) :=
       cows[, lapply(.SD, function(x) interval(today, x) %/% months(1)),
            .SDcols = date_not_as_num]]
  }

  if (anyNA(cows$sex)) {
    cows$sex[is.na(cows$sex)] <- "female"
  }


  # create_calf_data here because it uses age of cows as a criteria
  if (!identical(create_calf_data, F)) {
    if (is.logical(create_calf_data) && create_calf_data) {
      age_make_calf <- min(cows$age)
    } else if (is.numeric(create_calf_data)) {
      age_make_calf <- create_calf_data
    }
    n_mid_cows <- sum(age_make_calf < cows$age & cows$age <= age_make_calf * 2)
    n_old_cows <- sum(age_make_calf * 2 < cows$age &
                      cows$age <= age_make_calf * 3) 
    n_cows_add <- n_mid_cows * (n_mid_cows / n_old_cows)
    cows_add_age <- sample(age_make_calf, n_cows_add, replace = T)
    cows_add <- a_new_calf[rep(1, n_cows_add), ]
    cows_add[, `:=`(cow_id = max(cows$cow_id) + 1:n_cows_add,
                    age = cows_add_age,
                    date_birth = cows_add_age * -1,
                    sex = "female")]
    cows <- rbindlist(list(cows, cows_add))
    n_cows <- n_cows + n_cows_add
  }


  # TODO: Improve this to be calcuated stochastic
  delivery_age_table <-
    integerize(param_calculated$age_first_delivery + 
               param_calculated$calving_interval * 0:9)
  if (anyNA(cows$parity)) {
    cows[is.na(parity) & (stage == "heifer" | stage == "calf"), parity := 0]
    cows[is.na(parity) & (is.na(stage) | stage == "dry" | stage == "milking"),
         parity := vapply(age, function(x) sum(x >= delivery_age_table), 1)]
    cows[!is.na(date_last_delivery) & parity == 0, parity := 1]
    cows[!is.na(date_got_pregnant) & parity == 0, parity := 1]
  }
  if (any(is.na(cows$date_last_delivery) & cows$parity != 0)) {
    cows[is.na(date_last_delivery) & parity != 0,
         date_last_delivery := fifelse(age <= delivery_age_table[parity], 0,
           trunc(runif(.N, 0, param_calculated$calving_interval - 1)) * -1)]
  }

  if (anyNA(cows$is_replacement)) {
    is_na <- is.na(cows$is_replacement)
    prob_rep <- set_prob_rep(sum(cows$parity != 0), param_calculated)
    cows[is_na,
         is_replacement := (sex == "female" & (age > 0 | runif(.N) < prob_rep))]
  }

  is_na_date_got_pregnant <- is.na(cows$date_got_pregnant) & cows$parity != 0
  if (any(is_na_date_got_pregnant)) {
    months_open <- integerize(param_calculated$months_open)
    cows[is_na_date_got_pregnant,
         date_got_pregnant :=
           fifelse(date_last_delivery * -1 > months_open,
                   date_last_delivery + months_open, NA_real_)]
  }
  cows[date_got_pregnant < -10 |
       ((date_got_pregnant == -10 | date_got_pregnant == -9) &
         stage == "milking"),
       date_got_pregnant := NA_real_]
  is_na_date_dried <- is.na(cows$date_dried) & 
                        (is.na(cows$stage) | cows$stage != "milking")
  months_milking <- integerize(param_calculated$months_milking)
  if (any(is_na_date_dried)) {
    cows[is_na_date_dried,
         date_dried := fifelse(date_last_delivery * -1 > months_milking,
                               date_last_delivery + months_milking, NA_real_)]
  }
  cows[stage == "milking", date_dried := NA]
  if (anyNA(cows$stage)) {
    is_na <- is.na(cows$stage)
    cows[is_na & parity == 0, stage := fifelse(age < 4, "calf", "heifer")]
    cows[is_na & parity != 0,
         stage := fifelse(is.na(date_dried), "milking", "dry")]
  }

  cows$is_to_test_pregnancy[is.na(cows$is_to_test_pregnancy)] <- F
  cows$n_ai[is.na(cows$n_ai)] <- 0

  if (!is.null(modify_prevalence)) {
    appropreate_n_inf <- round(n_cows * modify_prevalence)
    inf_count <- table(cows$infection_status != "s", useNA = "always")
    max_n_inf <- inf_count["TRUE"] + inf_count["<NA>"]
    is_na <- is.na(cows$infection_status)
    if (appropreate_n_inf < inf_count["TRUE"]) {
      cows[sample(which(infection_status != "s"),
                  inf_count["TRUE"] - appropreate_n_inf),
           infection_status := "s"]
      cows$infection_status[is_na] <- "s"
    } else if (appropreate_n_inf <= max_n_inf) {
      cows[sample(which(is_na), max_n_inf - appropreate_n_inf),
           infection_status := "ial"]
      cows$infection_status[is.na(cows$infection_status)] <- "s"
    } else {
      cows[sample(which(infection_status == "s"),
                  appropreate_n_inf - max_n_inf),
           infection_status := "ial"]
      cows$infection_status[is_na] <- "ial"
    }
  } else if (anyNA(cows$infection_status)) {
    cows$infection_status[is.na(cows$infection_status)] <- "s"
  }

  cows[infection_status != "s" & is.na(date_ial), date_ial := 0]
  cows[(infection_status == "ipl" | infection_status == "ebl") &
        is.na(date_ipl),
       date_ipl := 0]
  cows[infection_status == "ebl" & is.na(date_ebl), date_ebl := 0]
  cows$cause_infection[cows$infection_status != "s"] <- "initial"

  if (anyNA(cows$area_id)) {
    cow_stage <- c("calf", "heifer", "milking", "dry")
    join_on <- c("stage", "parity")
    area_by_stage_and_parity <-
      cows[, list(freq_area = names(sort(table(area_id), decreasing = T))[1]),
           by = join_on]
    area_by_stage_and_parity <-
      area_by_stage_and_parity[CJ(stage = cow_stage,
                                  parity = parity, unique = T),
                               on = join_on]
    cows <- area_by_stage_and_parity[cows, on = join_on]
    cows[, `:=`(area_id = fcoalesce(area_id, as.numeric(freq_area)),
                freq_area = NULL)]
    area_id_in_input <- unique(na.omit(cows$area_id))
    empty_area_id <- setdiff(seq_len(length(area_id_in_input) + 4),
                             area_id_in_input)[1:4]
    area_by_stage <-
      cows[, list(freq_area = names(sort(table(area_id), decreasing = T))[1]),
           by = "stage"]
    area_by_stage <- area_by_stage[CJ(stage = cow_stage), on = "stage"]
    area_by_stage[,
      freq_area := fcoalesce(as.numeric(freq_area), as.numeric(empty_area_id))]
    cows <- area_by_stage[cows, on = "stage"]
    cows[, `:=`(area_id = fcoalesce(area_id, freq_area),
                freq_area = NULL)]
  }

  cows[is.na(months_in_area), months_in_area := 0]
  cows[is.na(is_isolated), is_isolated := F]


  # Next, calculate values in columns users should not specify.
  rows_to_calc_longevity <- seq_len(n_cows)
  n_rows_to_calc_longevity <- n_cows
  while (n_rows_to_calc_longevity > 0) {
    calculated_longevity <-
      longevity(n_rows_to_calc_longevity, param_calculated)
    cows[rows_to_calc_longevity,
         `:=`(date_death_expected = date_birth + calculated_longevity$age,
              cause_removal = calculated_longevity$cause)]
    rows_to_calc_longevity <- which(cows$date_death_expected <= 0)
    n_rows_to_calc_longevity <- length(rows_to_calc_longevity)
  }

  cows$is_owned <- T
  cows[is.na(date_got_pregnant), day_heat := sample.int(30, .N, replace = T)]
  # TODO: Think better implementation of is_to_test_pregnancy

  cows$is_detected[cows$infection_status != "s"] <- T
  susceptibility <- runif(n_cows)
  cows$susceptibility_ial_to_ipl <-
    susceptibility < param_calculated$prob_develop_ipl
  cows$susceptibility_ial_to_ipl[
    cows$infection_status == "ipl" | cows$infection_status == "ebl"] <- T
  cows$susceptibility_ipl_to_ebl <-
    susceptibility < param_calculated$prob_develop_ebl
  cows$susceptibility_ipl_to_ebl[cows$infection_status == "ebl"] <- T

  if (!is.null(n_chambers)) {
    for (i_area in names(n_chambers)) {
      empty_chambers <- setdiff(seq_len(n_chambers[i_area]),
                                cows$chamber_id[cows$area_id == i_area])
      cows[area_id == i_area & is.na(chamber_id),
           chamber_id := sample(empty_chambers, .N)]
    }
  }

  cows$i_month <- 0

  cows <- cows[, .SD, .SDcols = colnames(a_new_calf)]
  return(cows)
}

