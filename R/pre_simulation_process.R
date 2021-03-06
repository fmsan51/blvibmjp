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
#' - `is_replacement`: If a farm keeps non-replacement cows (e.g. a male newborn which will be send to a livestock market), set this parameter. If not set, `age` = 0 female cows are assumed to be a replacement according to `prop_replacement` parameter in `param` and all male cows are assumed to be a non-replacement.
#' - `stage`, `parity`, `date_last_delivery`, `date_got_pregnant`, `date_dried`: If not set, they will be calculated in according to parameters related with reproduction in [param].
#' - `is_to_test_pregnancy`: If not set, `FALSE` is set.
#' - `n_ai`: If not set, it is assumed to be 0.
#' - `infection_status`: At least one of this variable or `modify_prevalence` argument must be set. Valid categories are follows: "al", "pl" and "ebl" (case insensitive). Other values or `NA` will be coerced to "s" (= non-infected). When `modify_prevalence` is set, prevalence is modified to make prevalence equal to the first value of `modify_prevalence`.
#' - `date_ial`, `date_ipl`, `date_ebl`: Specify the date when infection status was confirmed. If `NULL`, `0` is set.
#' - `area_id`: If not set, cows are divided to four areas based on `stage` ("calf" = 1, "heifer" = 2, "milking" = 3, "dry" = 4). If `NA`s are included, cows are allocated to areas in which cows with the same stage and parity are kept. If `area_id` is written in character, argument `area_name` must be set.
#' - `months_in_area`: If not set, it is assumed to be 0. This parameter has no effect when a farm does not use `months_in_area` as a criteria for area movement. See [area_table] for detail of area movement.
#' - `chamber_id`: If not set, it is randomly allocated later in [setup_cows()].
#' - `is_isolated`: If not set, `FALSE` is set.
#'
#' For further detail of each variable, see [cow_table].
#'
#' @param csv File path of an input csv file. See the Detail section to know about form of input csv.
#' @param data data.frame as a input instead of `csv`. See the Detail section to know about form of input data.
#' @param output_file The name of an output file (must be a csv file). If `NULL`, no output file is created.
#' @param today A Date class object or a character in "YYYY/MM/DD" format. The date used to calculate `age` from `date_birth` when `age` is not set. `today` is automatically calculated when both of `age` and `date_birth` are filled and `date_birth` is in form of Date rather than number (which means that the cow was born $n$ month ago) and the value passed to this argument is ignored.
#' @param create_calf_data logical or a numeric. Create data for young cows based on cow data in the input. Set this argument when the input does not contain data for young cows (e.g. when you use Nyuken data). If `TRUE`, create cows younger than the youngest cows in the input. If a numeric is set, create cows equal to or younger than that age.
#' @param modify_prevalence One or two numbers within a range of 0 to 1. If the parameter is not `NULL`, modify `infection_status` column to make proportion of infected cows (when `modify_prevalence is a number) or`ial` and `ipl + ebl` cows (when `modify_prevalence` is two numbers) accordingly.
#' @param param See [param].
#' @param area_name If `area_id` is specified by character, specify integer `area_id` like `c(barnA = 1, barnB = 2, ...)`.
#' @param seed Seed for a simulation.
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_blv_spread()].
prepare_cows <- function(csv, param, data = NULL, output_file = NULL,
                         today = Sys.Date(),
                         create_calf_data = F, modify_prevalence = NULL,
                         area_name = NULL, seed = NULL) {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cols_in_input <- intersect(cow_table_cols, colnames(input))
  n_cows <- nrow(input)
  if (n_cows == 0) {
    stop("No data is contained in the cow input.")
  }
  cows <- a_new_calf[rep(1, n_cows), ]
  cows[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

  warn_invalid_col(input, cow_table_cols)

  # First, calculate values in columns which users can specify.
  cows$cow_id <- tryCatch(as.integer(as.character(cows$cow_id)),
    warning = function(e) {
      stop("`cow_id` in the cow input contains non-integer value(s).")
    })
  is_na <- is.na(cows$cow_id)
  if (any(is_na)) {
    n_na <- sum(is_na)
    cows$cow_id[is_na] <- setdiff(1:n_cows, cows$cow_id)[1:n_na]
    # 1:n is used because it is much faster than seq_len(n).
  }

  # Convert date variables from character to Date
  if (is.character(today)) {
    today <- ymd(today)
  }
  date_vars <- grep("^date_", cow_table_cols, value = T)
  is_date_as_chr <- cows[, vapply(.SD, is.character, T), .SDcols = date_vars]
  date_as_chr <- date_vars[is_date_as_chr]
  # if statement here is to prevent warning due to 0-length RHS of :=
  if (length(date_as_chr) != 0) {
    cows[, (date_as_chr) := cows[, lapply(.SD, ymd), .SDcols = date_as_chr]]
  }

  # Convert is_xxx variables from numeric or character to logical
  lgl_vars <- c(grep("^is_", cow_table_cols, value = T),
                "susceptibility_ial_to_ipl", "susceptibility_ipl_to_ebl")
  cows_w_lgl_vars <- cows[, ..lgl_vars]
  cows_lgl_vars_converted <- lapply(cows_w_lgl_vars, function(x)
    as.logical(factor(x, levels = c("1", "TRUE", "0", "FALSE"),
                      labels = c("TRUE", "TRUE", "FALSE", "FALSE")))
    )
  cows[, (lgl_vars) := cows_lgl_vars_converted]

  # Convert classes of other types of variables
  int_vars <- c("parity", "n_ai", "chamber_id")
  num_vars <- c(int_vars, "age", "months_in_area")

  cows_w_num_vars <- cows[, ..num_vars]
  coersed_to_num <- lapply(cows_w_num_vars,
    function(x) tryCatch(as.numeric(as.character(x)),
                         # Do as.character to prepare when x is factor.
                         warning = function(e) "error")
    )
  non_num_vars <- num_vars[!vapply(coersed_to_num, is.numeric, T)]
  if (length(non_num_vars) != 0) {
    stop(glue(
      "Following column(s) in the cow data must contain numbers only:
       {paste0('`', non_num_vars, '`', collapse = ', ')}"
    ))
  }
  non_int_vars <-
    int_vars[!vapply(coersed_to_num, is.wholenumbers, na.rm = T, T)]
  if (length(non_int_vars) != 0) {
    stop(glue(
      "Following column(s) in the cow data must contain integers only:
       {paste0('`', non_int_vars, '`', collapse = ', ')}"
    ))
  }
  cows[, (num_vars) := coersed_to_num]
  cows[, (int_vars) := lapply(.SD, floor), .SDcols = int_vars]
  cows$chamber_id <- as.integer(cows$chamber_id)

  chr_vars <- c("sex", "stage", "infection_status", "area_id")
  cows[, (chr_vars) := cows[, lapply(.SD, as.character), .SDcols = chr_vars]]

  is_na_age <- is.na(cows$age)
  is_na_date_birth <- is.na(cows$date_birth)
  if (any(!is_na_age & !is_na_date_birth) & !is.numeric(cows$date_birth)) {
    today <- mean(cows$date_birth + months(cows$age), na.rm = T)
  }
  if (any(is_na_age)) {
    if (is.numeric(cows$date_birth)) {
      cows$age[is_na_age] <- (cows$date_birth * -1)[is_na_age]
    } else {
      cows$age[is_na_age] <-
        (interval(cows$date_birth, today) %/% months(1))[is_na_age]
    }
  }
  if (any(is_na_date_birth)) {
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

  is_na <- is.na(cows$sex)
  if (any(is_na)) {
    cows$sex[is_na] <- "female"
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
    cows_add_age <- sample.int(age_make_calf, n_cows_add, replace = T) * 1
    cows_add <- a_new_calf[rep(1, n_cows_add), ]
    cows_add[, `:=`(cow_id = max(cows$cow_id) + 1:n_cows_add,
                    age = cows_add_age,
                    date_birth = cows_add_age * -1,
                    sex = "female")]
    cows <- rbindlist(list(cows, cows_add))
    n_cows <- n_cows + n_cows_add
  }


  param_calculated <- calc_param_pre(param)
  delivery_age_table <-
    integerize(param_calculated$age_first_delivery +
               param_calculated$calving_interval * 0:9)
  is_na <- is.na(cows$parity)
  if (any(is_na)) {
    cows[is_na & (stage == "heifer" | stage == "calf"), `:=`(parity = 0)]
    cows[is_na & (is.na(stage) | stage == "dry" | stage == "milking"),
      `:=`(parity = vapply(age, function(x) sum(x >= delivery_age_table), 1))]
    cows[(stage == "milking" | stage == "dry" | !is.na(date_last_delivery)) &
         age < delivery_age_table[1],
         `:=`(parity = 1)]
  }
  is_na_date_delivered <- is.na(cows$date_last_delivery) & cows$parity != 0
  if (any(is_na_date_delivered)) {
    cows[is_na_date_delivered,
         `:=`(date_last_delivery = fifelse(age <= delivery_age_table[parity], 0,
              trunc(runif(.N, 0, param_calculated$calving_interval - 1)) * -1))]
  }

  is_na <- is.na(cows$is_replacement)
  if (any(is_na)) {
    prob_rep <- set_prob_rep(sum(cows$parity != 0), param)
    cows[is_na, `:=`(is_replacement =
                     (sex == "female" & (age > 0 | runif(.N) < prob_rep)))]
  }

  is_na_date_got_pregnant <- is.na(cows$date_got_pregnant) & cows$parity != 0
  if (any(is_na_date_got_pregnant)) {
    months_open <- integerize(param_calculated$months_open)
    cows[is_na_date_got_pregnant,
         `:=`(date_got_pregnant =
              fifelse(date_last_delivery * -1 > months_open,
                      date_last_delivery + months_open, NA_real_))]
  }
  cows[date_got_pregnant < -10 |
       ((date_got_pregnant == -10 | date_got_pregnant == -9) &
         stage == "milking"),
       `:=`(date_got_pregnant = NA_real_)]
  is_na_date_dried <-
    is.na(cows$date_dried) & (is.na(cows$stage) | cows$stage != "milking")
  months_milking <- integerize(param_calculated$months_milking)
  if (any(is_na_date_dried)) {
    cows[is_na_date_dried,
         `:=`(date_dried =
                fifelse(date_last_delivery * -1 > months_milking,
                        date_last_delivery + months_milking, NA_real_))]
  }
  cows$date_dried[cows$stage == "milking"] <- NA_real_
  is_na <- is.na(cows$stage)
  if (any(is_na)) {
    cows[is_na & parity == 0, `:=`(stage = fifelse(age < 4, "calf", "heifer"))]
    cows[is_na & parity != 0,
         `:=`(stage = fifelse(is.na(date_dried), "milking", "dry"))]
  }

  cows$is_to_test_pregnancy[is.na(cows$is_to_test_pregnancy)] <- F
  is_na <- is.na(cows$n_ai)
  if (any(is_na)) {
    cows$n_ai[is_na & cows$stage == "calf"] <- 0
    cows[
      is_na & is.na(date_got_pregnant) &
      (
       (stage == "heifer" & is_ai_started_heifer(age, param_calculated)) |
       ((stage == "milking" | stage == "dry") &
        is_ai_started_milking(date_last_delivery * -1, param_calculated))
      ),
      `:=`(n_ai = sample(0:10, .N, replace = T,
                         prob = param_calculated$prob_n_ai) + 0)
      ]
  }

  is_ial <- cows[,
    grepl("^i?al$", infection_status, ignore.case = T) |
    (!is.na(date_ial) & is.na(date_ipl) & is.na(date_ebl))
    ]
  is_ipl <- cows[,
    grepl("^i?pl$", infection_status, ignore.case = T) |
    (!is.na(date_ipl) & is.na(date_ebl))
    ]
  is_ebl <- cows[,
    grepl("^e?bl$", infection_status, ignore.case = T) |
    !is.na(date_ebl)
    ]
  is_s <- !(is.na(cows$infection_status) | is_ial | is_ipl | is_ebl)
  items_coerced_to_s <- unique(cows$infection_status[is_s])
  cows$infection_status[is_ial] <- "ial"
  cows$infection_status[is_ipl] <- "ipl"
  cows$infection_status[is_ebl] <- "ebl"
  cows$infection_status[is_s] <- "s"
  items_coerced_to_s <- unique(cows$infection_status[is_s])
  items_coerced_to_s <- items_coerced_to_s[items_coerced_to_s != "s"]
  if (length(items_coerced_to_s) != 0) {
    message(glue(
      "Following item(s) in the `infection_status` column is treated as \\
       non-infected:
       {paste(items_coerced_to_s, collapse = ',')}"
    ))
  }

  is_na <- is.na(cows$infection_status)
  if (!is.null(modify_prevalence)) {
    fct_infection_status <-
      factor(cows$infection_status, levels = c("s", "ial", "ipl", "ebl"))
    n_modify_prevalence <- length(modify_prevalence)
    stopifnot(n_modify_prevalence <= 2, sum(modify_prevalence) <= 1,
              all(modify_prevalence >= 0), all(modify_prevalence <= 1))

    if (n_modify_prevalence == 2) {
      appropreate_n_highrisk <- round(n_cows * modify_prevalence[2])
      inf_table <- table(fct_infection_status, useNA = "always")
      n_highrisk <- inf_table["ipl"] + inf_table["ebl"]
      max_n_highrisk <- n_highrisk + inf_table[5]  # inf_table[5] = NA
      if (appropreate_n_highrisk < n_highrisk) {
        n_extra <- n_highrisk - appropreate_n_highrisk
        if (n_extra < inf_table["ipl"]) {
          fct_infection_status[
            resample(which(fct_infection_status == "ipl"), n_extra)
            ] <- "ial"
        } else {
          fct_infection_status[fct_infection_status == "ipl"] <- "ial"
          fct_infection_status[
            resample(which(fct_infection_status == "ebl"),
                     n_extra - inf_table["ipl"])
            ] <- "ial"
        }
      } else if (appropreate_n_highrisk <= max_n_highrisk) {
        fct_infection_status[
          resample(which(is_na), appropreate_n_highrisk - n_highrisk)
          ] <- "ipl"
      } else {  # max_n_highrisk < appropreate_n_highrisk
        fct_infection_status[is_na] <- "ipl"
        n_extra <- appropreate_n_highrisk - max_n_highrisk
        if (n_extra < inf_table["ial"]) {
          fct_infection_status[
            resample(which(fct_infection_status == "ial"), n_extra)
            ] <- "ipl"
        } else {
          fct_infection_status[fct_infection_status == "ial"] <- "ipl"
          fct_infection_status[
            resample(which(fct_infection_status == "s"),
                     n_extra - inf_table["ial"])
            ] <- "ipl"
        }
      }

      appropreate_n_ial <- round(n_cows * modify_prevalence[1])
      is_na <- is.na(fct_infection_status)
      inf_table <- table(fct_infection_status, useNA = "always")
      max_n_ial <- inf_table["ial"] + inf_table[5]  # inf_table[5] = NA
      if (appropreate_n_ial < inf_table["ial"]) {
        fct_infection_status[
          resample(which(fct_infection_status == "ial"),
                   inf_table["ial"] - appropreate_n_ial)
        ] <- "s"
        fct_infection_status[is_na] <- "s"
      } else if (appropreate_n_ial <= max_n_ial) {
        fct_infection_status[
          resample(which(is_na), appropreate_n_ial - inf_table["ial"])
        ] <- "ial"
        fct_infection_status[is.na(fct_infection_status)] <- "s"
      } else {  # max_n_ial < appropreate_n_ial
        fct_infection_status[is_na] <- "ial"
        fct_infection_status[
          resample(which(fct_infection_status == "s"),
                   appropreate_n_ial - max_n_ial)
        ] <- "ial"
      }
    } else {  # length(modify_prevalence == 1)
      appropreate_n_inf <- round(n_cows * modify_prevalence)
      inf_table <- table(fct_infection_status, useNA = "always")
      n_inf <- inf_table["ial"] + inf_table["ipl"] + inf_table["ebl"]
      max_n_inf <- n_inf + inf_table[5]  # inf_table[5] = NA
      if (appropreate_n_inf < n_inf) {
        n_extra <- n_inf - appropreate_n_inf
        if (n_extra < inf_table["ial"]) {
          fct_infection_status[
            resample(which(fct_infection_status == "ial"), n_extra)
            ] <- "s"
        } else if (n_extra < inf_table["ial"] + inf_table["ipl"]) {
          fct_infection_status[fct_infection_status == "ial"] <- "s"
          fct_infection_status[
            resample(which(fct_infection_status == "ipl"),
                     n_extra - inf_table["ial"])
            ] <- "s"
        } else {
          fct_infection_status[
            fct_infection_status == "ial" | fct_infection_status == "ipl"
            ] <- "s"
          fct_infection_status[
            resample(which(fct_infection_status == "ebl"),
                     n_extra - inf_table["ial"] - inf_table["ipl"])
            ] <- "s"
        }
        fct_infection_status[is_na] <- "s"
      } else if (appropreate_n_inf <= max_n_inf) {
        fct_infection_status[
          resample(which(is_na), appropreate_n_inf - n_inf)
          ] <- "ial"
        fct_infection_status[is.na(fct_infection_status)] <- "s"
      } else {  # max_n_inf < appropreate_n_inf
        fct_infection_status[is_na] <- "ial"
        fct_infection_status[
          resample(which(fct_infection_status == "s"),
                   appropreate_n_inf - max_n_inf)
          ] <- "ial"
      }
    }
    cows$infection_status <- fct_infection_status
  } else if (any(is_na)) {
    cows$infection_status[is_na] <- "s"
  }

  cows$date_ial[cows$infection_status != "s" & is.na(cows$date_ial)] <- 0
  cows[
    (infection_status == "ipl" | infection_status == "ebl") & is.na(date_ipl),
    `:=`(date_ipl = 0)]
  cows$date_ebl[cows$infection_status == "ebl" & is.na(cows$date_ebl)] <- 0
  cows$cause_infection[cows$infection_status != "s"] <- "initial"

  if (!is.null(area_name)) {
    chr_area_name <- names(area_name)
    if (any(!cows$area_id %in% c(chr_area_name, NA_character_))) {
      stop(glue("`area_name` in the cow data contains area names \\
                 which are not contained in the area data."))
    }
    cows$area_id <- factor(cows$area_id, levels = chr_area_name)
  }
  cows$area_id <- as.integer(cows$area_id)
  if (anyNA(cows$area_id)) {
    cow_stage <- c("calf", "heifer", "milking", "dry")
    join_on <- c("stage", "parity")
    area_by_stage_and_parity <- cows[,
      list(freq_area =
             as.integer(names(sort(table(area_id), decreasing = T))[1])),
      by = join_on]
    area_by_stage_and_parity <- area_by_stage_and_parity[
      CJ(stage = cow_stage, parity = parity, unique = T),
      on = join_on]
    cows <- area_by_stage_and_parity[cows, on = join_on]
    cows$area_id <- fcoalesce(cows$area_id, cows$freq_area)
    cows$freq_area <- NULL
    area_id_in_input <- unique(cows$area_id[!is.na(cows$area_id)])
    empty_area_id <-
      setdiff(seq_len(length(area_id_in_input) + 4L), area_id_in_input)[1:4]
    join_on <- "stage"
    area_by_stage <- cows[,
      list(freq_area =
             as.integer(names(sort(table(area_id), decreasing = T))[1])),
           by = join_on]
    area_by_stage <-
      area_by_stage[CJ(stage = cow_stage, sorted = F), on = join_on]
    area_by_stage$freq_area <- fcoalesce(area_by_stage$freq_area, empty_area_id)
    cows <- area_by_stage[cows, on = join_on]
    cows$area_id <- fcoalesce(cows$area_id, cows$freq_area)
    cows$freq_area <- NULL
  }

  cows$months_in_area[is.na(cows$months_in_area)] <- 0
  cows$is_isolated[is.na(cows$is_isolated)] <- F

  # Next, calculate values in columns users should not specify.
  rows_to_calc_longevity <- 1:n_cows
  # 1:n is used because it is much faster than seq_len(n).
  n_rows_to_calc_longevity <- n_cows
  while (n_rows_to_calc_longevity > 0) {
    calculated_longevity <-
      longevity(n_rows_to_calc_longevity, param_calculated)
    cows[rows_to_calc_longevity,
         `:=`(date_removal_expected = date_birth + calculated_longevity$age,
              cause_removal = calculated_longevity$cause)]
    rows_to_calc_longevity <- which(cows$date_removal_expected <= 0)
    n_rows_to_calc_longevity <- length(rows_to_calc_longevity)
  }

  cows$is_owned <- T
  cows[is.na(date_got_pregnant),
       `:=`(day_heat = sample.int(30, .N, replace = T) * 1)]
  cows[is.na(day_last_detected_heat),
       `:=`(day_last_detected_heat = sample.int(30, .N, replace = T))]  # TODO: Improve this

  cows$is_detected[cows$infection_status != "s"] <- T
  susceptibility <- runif(n_cows)
  cows$susceptibility_ial_to_ipl <-
    susceptibility < param_calculated$prob_develop_ipl
  cows$susceptibility_ial_to_ipl[
    cows$infection_status == "ipl" | cows$infection_status == "ebl"] <- T
  cows$susceptibility_ipl_to_ebl <-
    susceptibility < param_calculated$prob_develop_ebl
  cows$susceptibility_ipl_to_ebl[cows$infection_status == "ebl"] <- T

  cows$i_month <- 0

  cows <- cows[, ..cow_table_cols]

  if (!is.null(output_file)) {
    fwrite(cows, output_file)
  }

  return(cows)
}


#' Process raw area data to suitable form
#'
#' Transform an input csv into a suitable form, which is in a form of [area_table].
#'
#' An input csv file can have following columns. The csv file must contain `area_type` column.
#'
#' - `area_id`: If not set or non-numerical value is set, sequencial integers are allocated (from 1 to the number of input rows). More than two rows can have the same `area_id` only when these rows have `area_type`s as "tie". *e.g.* `data.frame(area_id = c(1, 1), area_type = c("tie", "tie"), capacity = c(10, 20))` is identical to `data.frame(area_id = 1, area_type = "tie", capacity = list(c(10, 20)))`. If `NA`, the previous non-`NA` value is set.
#' - `area_type`
#' - `capacity`: If `NA`, `Inf` is set. If `area_type` is "tie" or "hatch", `capacity` must be set. A character like `"10,20,30x2"` will be converted to a numeric vector `c(10, 20, 30, 30)`. Separator (`,`) can be specifed by `sep` argument.
#'
#' For further detail of each variable, see [area_table].
#'
#' @param csv File path of an input csv file. See the Detail section to know about form of input csv.
#' @param data data.frame as a input instead of `csv`. See the Detail section to know about form of input data.
#' @param output_file The name of an output file (must be a csv file). If `NULL`, no output file is created.
#' @param sep Separatator used in `capacity` column. See explanation of `capacity` in Detail section.
#' @param seed Seed for a simulation.
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_blv_spread()].
prepare_area <- function(csv, data = NULL, output_file = NULL,
                         sep = "[,\t\r\n |;:]+", seed = NULL) {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cols_in_input <- intersect(area_table_cols, colnames(input))
  n_rows <- nrow(input)
  if (n_rows == 0) {
    stop("No data is contained in the area input.")
  }
  area_table <- a_area[rep(1, n_rows), ]
  area_table[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

  warn_invalid_col(input, area_table_cols)

  if (all(is.na(area_table$area_id))) {
    area_table$area_id <- 1:n_rows
    # 1:n is used because it is much faster than seq_len(n).
  } else if (!is.numeric(input$area_id)) {  # Here, input, not area_table, is correct.
    area_table$area_id <-
      factor(area_table$area_id, levels = unique(area_table$area_id))
  }
  area_table$area_id <- as.integer(area_table$area_id)
  if (is.na(area_table$area_id[1])) {
    stop(glue("If any value is set to `area_id` column in area data, \\
               the first value in `area_id` column must be set."))
  }
  area_table$area_id <- nafill(area_table$area_id, type = "locf")

  area_type_list <- split(area_table$area_type, area_table$area_id)
  n_type_in_each_area <-
    vapply(area_type_list, function(x) n_distinct(x, na.rm = T), 1)
  has_invalid_area_type <- n_type_in_each_area != 1
  if (any(has_invalid_area_type)) {
    area_with_invalid_type <- names(area_type_list)[has_invalid_area_type]
    stop(glue("`area_type` must contain exactly one value. \\
               Check `area_type` of following `area_id` in the area data:
               {paste(area_with_invalid_type, collapse = ', ')}"))
  }

  is_na <- is.na(area_table$capacity)
  if (any(is_na)) {
    if (any(area_table$area_type[is_na] == "tie" |
            area_table$area_type[is_na] == "hatch")) {
      stop("`capacity` column in area data must be set for tie or hatch areas.")
    }
    area_table$capacity[is_na] <- "Inf"
  }
  area_table$capacity <- as.character(area_table$capacity)
  # as.character() to when capacity is an integer/numeric vector.
  area_table$capacity <- warn_double_sep(area_table$capacity)
  area_table$capacity <-
    paste0("c(", gsub(sep, ", ", area_table$capacity), ")")
  area_table$capacity <-
    lapply(area_table$capacity,
           function(x) gsub("(\\d+) ?x ?(\\d+)", "rep(\\1, \\2)", x, perl = T))
  area_table$capacity <-
    lapply(area_table$capacity, function(x) eval(parse(text = x)))
  area_table$capacity <- lapply(area_table$capacity, as.numeric)

  if (any(duplicated(area_table$area_id))) {
    area_table <- area_table[,
      list(area_type = unique(area_type[!is.na(area_type)])[1],
           capacity = list(unlist(capacity))),
      by = area_id]
  }

  if (!is.null(output_file)) {
    fwrite(area_table, output_file)
  }

  return(area_table)
}


#' Process raw movement data to suitable form
#'
#' Transform an input csv into a suitable form, which is in a form of [movement_table].
#'
#' An input csv file can have following columns. The csv file must contain `current_area`, `condition` and `next_area` column.
#'
#' - `current_area`
#' - `condition`
#' - `next_area`: A character like `"1|2|3"` will be converted to a numeric vector `c(1, 2, 3)`. Separator (`|`) can be specifed by `sep` argument. (This transformation from character to numeric is necessary if you want to read data from a csv file.)
#' - `priority`: If `NA`, `rep(1, length(next_area))` is set (which means all `next_area`s have the same priority and cows are randomly allocated among all `next_area`s). Values must be integer/numeric vectors of the same length with `next_area` or `NA`. Multiple values can be specified like `next_area`.
#' For further detail of each variable, see [movement_table].
#'
#' @param csv File path of an input csv file. See the Detail section to know about form of input csv.
#' @param data data.frame as a input instead of `csv`. See the Detail section to know about form of input data.
#' @param output_file The name of an output file (must be a csv file). If `NULL`, no output file is created.
#' @param area_name If `current_area` and `next_area` are specified by character, specify integer `area_id` like `c(barnA = 1, barnB = 2, ...)`.
#' @param sep Separatator used in `priority` column. See explanation of `priority` in Detail section.
#' @param seed Seed for a simulation.
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_blv_spread()].
prepare_movement <- function(csv, data = NULL, output_file = NULL,
                             area_name = NULL, sep = "[,\t\r\n |;:]+",
                             seed = NULL) {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cols_in_input <- intersect(movement_table_cols, colnames(input))
  n_rows <- nrow(input)
  if (n_rows == 0) {
    stop("No data is contained in the movement input.")
  }
  movement_table <- a_movement[rep(1, n_rows), ]
  movement_table[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

  warn_invalid_col(input, movement_table_cols)

  necessary_cols <- c("current_area", "condition", "next_area")
  necessary_data <- movement_table[, ..necessary_cols]
  if (anyNA(necessary_data)) {
    missing_cols <- necessary_cols[vapply(necessary_data, anyNA, F)]
    stop(glue(
      "Following column(s) in the movement data must not contain missing value:
       {paste0('`', missing_cols, '`', collapse = ', ')}"
    ))
  }

  movement_table$next_area <- warn_double_sep(movement_table$next_area)
  movement_table$next_area <-
    strsplit(as.character(movement_table$next_area), sep)
  # as.character() to when capacity is an integer/numeric vector.
  if (!is.null(area_name)) {
    chr_area_name <- names(area_name)
    if (any(!movement_table$current_area %in% chr_area_name)) {
      stop(glue("`current_area` in the movement data contains \\
                 an area name which is not contained in the area data."))
    }
    if (any(!unlist(movement_table$next_area) %in% chr_area_name)) {
      stop(glue("`next_area` in the movement data contains \\
                 an area name which is not contained in the area data."))
    }

    movement_table$current_area <-
      factor(movement_table$current_area,
             levels = chr_area_name, labels = area_name)
    movement_table$next_area <- lapply(movement_table$next_area,
      function(x) factor(x, levels = chr_area_name, labels = area_name)
      )
  }
  movement_table$current_area <- as.integer(movement_table$current_area)
  movement_table$next_area <- lapply(movement_table$next_area, as.integer)

  movement_table$priority <- warn_double_sep(movement_table$priority)
  movement_table$priority <-
    strsplit(as.character(movement_table$priority), sep)
  # as.character() to when capacity is an integer/numeric vector.
  movement_table$priority <- lapply(movement_table$priority, as.numeric)
  n_priority <- vapply(movement_table$priority, length, 1)
  if (anyNA(movement_table$priority) | any(n_priority == 0)) {
    n_next_area <-
      # FIXME: use 0.5 to set is_priprity_specified_by_integer as F
      # vapply(movement_table$next_area, function(x) length(x[!is.na(x)]), 1)
      vapply(movement_table$next_area, function(x) length(x[!is.na(x)]), 0.5)
    list1 <- lapply(n_next_area, function(x) rep(1, x))
    is_priority_missing <- is.na(movement_table$priority) | n_priority == 0
    movement_table$priority[is_priority_missing] <- list1[is_priority_missing]
    n_priority[is_priority_missing] <- n_next_area[is_priority_missing]
  }
  if (any(vapply(movement_table$next_area, length, 1) != n_priority)) {
    stop(glue("`priority` in the movement data must be consisted of NA or \\
               integer/numeric vectors of the same number of items with \\
               `next_area`."))
  }

  # Sort next_area along with priority
  movement_table$next_area <-
    mapply(function(next_area, priority) {next_area[order(priority)]},
           movement_table$next_area, movement_table$priority, SIMPLIFY = FALSE)

  # translate condition from a form that users can easily understand
  # to a form that functions can easily understand
  cond <- movement_table$condition
  convert_day_to_month <- function(day) {
    day <- as.numeric(day)
    month <- round(day / days_per_month, 3)  # rounded for readability
    return(month)
  }
  cond <- str_replace_all(cond, "(?<=dim[^|&]{1,20}?)\\d+",
                          convert_day_to_month)
  # max range is 20 because it seems enough
  cond <- str_replace_all(cond, "(?<!parity[^|&]{1,20}?)(\\d*\\.\\d+)",
                          "integerize(\\1)")

  # fixed = T because it's about 2-3x faster
  cond <- gsub("months_from_delivery", "i_month - date_last_delivery",
               cond, fixed = T)
  cond <- gsub("months_from_pregnancy", "i_month - date_got_pregnant",
               cond, fixed = T)
  cond <- gsub("months_from_dry", "i_month - date_dried", cond, fixed = T)

  # (?:^|[^_]) is about 3x faster than (?<!_)
  cond <- gsub("(?:^|[^_])delivery", "\\1i_month == date_last_delivery", cond)
  cond <- gsub("(?:^|[^_])pregnancy", "\\1i_month == date_got_pregnant", cond)
  cond <- gsub("(?:^|[^_])dry", "\\1i_month == date_dried", cond)
  cond <- gsub("dim", "i_month - date_last_delivery", cond, fixed = T)
  cond <- gsub("stay", "months_in_area", cond, fixed = T)

  cond <- paste0(cond, " & area_id == ", movement_table$current_area,
                 " & is_owned == T")
  movement_table$condition <- cond

  if (!is.null(output_file)) {
    fwrite(movement_table, output_file)
  }

  return(movement_table)
}


#' Check whether input contains doublewidth separator
#'
#' @param data Data to check like `movement_table$next_area`.
warn_double_sep <- function(data) {
  data_name <- as.character(substitute(data))
  data_type <- switch(data_name[2],
                      cows = "cow",
                      area_table = "area",
                      movement_table = "movement")
  if (any(grepl(double_sep, data))) {
    data <- gsub(double_sep, ", ", data)
    warning(glue(
      "'{double_sep}' was found in `{data_name[3]}` in the {data_type} data. \\
       Replaced it with ', '."
    ))
  }
  return(data)
}


#' Check whether input contains invalid column(s)
#'
#' @param input A data.table
#' @param default_col A character vector indicating valid column names
warn_invalid_col <- function(input, default_col) {
  invalid_col <- setdiff(colnames(input), default_col)
  if (length(invalid_col) != 0) {
    warning(glue(
      "Following column(s) in the cow data is ignored: \\
       {paste0('`', invalid_col, '`', collapse = ', ')}
       Did you change the column name from the original one?"
    ))
  }
}


#' Process raw data to suitable forms
#'
#' Process raw data to suitable forms to use in simulation.
#'
#' @param excel Set this or `cow_data`, `area_data` and `movement_data`.
#' @param param See [param].
#' @param output When `TRUE`, create output csv files with names of "cow.csv", "area.csv" and "movement.csv" into a working directory. Shorthand form of setting "xxx.csv" to `xxx_output_file`.
#' @param cow_data See [prepare_cows()] for detail.
#' @param area_data See [prepare_area()] for detail.
#' @param movement_data See [prepare_movement()] for detail.
#' @param cow_output_file,area_output_file,movement_output_file If not `NULL`, created data is exported to the files with these names (must be csv files).
#' @param sep Separatator used in `capacity` column of area data. See [prepare_area()] for detail.
#' @param seed Seed for a simulation.
#' @param ... Other arguments passed to [prepare_cows()].
#'
#' @export
#' @return csv files which can be used as an input for [simulate_blv_spread()].
prepare_data <- function(excel, param, output = F,
                         cow_data = NULL, area_data = NULL,
                         movement_data = NULL,
                         cow_output_file = NULL, area_output_file = NULL,
                         movement_output_file = NULL,
                         sep = "[,\t\r\n |;:]+", seed = NULL, ...) {
  if (!missing(excel)) {
    cow_input <- read_excel(excel, sheet = "cow", skip = 3)
    area_input <- read_excel(excel, sheet = "area", skip = 3,
      range = cell_limits(c(4, 1), c(NA, 3)))  # cells from A4:C4
    movement_input <- read_excel(excel, sheet = "movement", skip = 3)
  } else {
    cow_input <- cow_data
    area_input <- area_data
    movement_input <- movement_data
  }

  if (is.numeric(area_input$area_id)) {
    area_name <- NULL
  } else {
    unique_areas <- unique(area_input$area_id)
    area_name <- seq_along(unique_areas)
    names(area_name) <- unique_areas
  }

  if (output) {
    cow_output_file <- "cow.csv"
    area_output_file <- "area.csv"
    movement_output_file <- "movement.csv"
  }

  cows <- prepare_cows(data = cow_input, param = param,
                       output_file = cow_output_file, area_name = area_name,
                       seed = seed, ...)
  areas <- prepare_area(data = area_input,
                        output_file = area_output_file, sep = sep, seed = seed)
  movement <- prepare_movement(data = movement_input,
                               output_file = movement_output_file,
                               area_name = area_name, sep = sep, seed = seed)

  return(list(cows = cows, areas = areas, movement = movement))
}

