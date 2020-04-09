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
#' - `infection_status`: At least one of this variable or `modify_prevalence` argument must be set. Valid categories are follows: "al", "pl" and "ebl" (case insensitive). Other values or `NA` will be coerced to "s" (= non-infected). When `modify_prevalence` is set, prevalence is modified to make prevalence equal to the value of `modify_prevalence`.
#' - `date_ial`, `date_ipl`, `date_ebl`: Specify the date when infection status was confirmed. If `NULL`, `0` is set.
#' - `area_id`: If not set, cows are divided to four areas based on `stage` ("calf" = 1, "heifer" = 2, "milking" = 3, "dry" = 4). If `NA`s are included, cows are allocated to areas in which cows with the same stage and parity are kept. If `area_id` is written in character, argument `area_name` must be set. If a cow is in a communal pasture, specify `0`.
#' - `month_in_area`: If not set, it is assumed to be 0. This parameter has no effect when a farm does not use `month_in_area` as a criteria for area movement. See [area_table] for detail of area movement.
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
#' @param modify_prevalence double (0-1). If not `NULL`, modify `infection_status` column to make prevalence to the specified value.
#' @param param See [param].
#' @param area_name If `area_id` is specified by character, specify integer `area_id` like `c(barnA = 1, barnB = 2, ...)`.
#' @param n_chambers Set if a farm owns tie-stall barns. Specify the number of chambers in each tie-stall barn like `c(area_id = the_number_of_chambers_in_the_area, ...)`. Note if both of `area_name` and `n_chambers` are set, `area_id` in `n_chambers` option must be integer.
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_BLV_spread()].
prepare_cow <- function(csv, param, data = NULL, output_file = NULL,
                        today = Sys.Date(),
                        create_calf_data = F, modify_prevalence = NULL,
                        area_name = NULL, n_chambers = NULL) {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  cols_in_input <- intersect(cow_table_cols, colnames(input))
  n_cows <- nrow(input)
  if (n_cows == 0) {
    stop("No data is contained in the cow input.")
  }
  cows <- a_new_calf[rep(1, n_cows), ]
  cows[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

  # First, calculate values in columns which users can specify.
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
  suppressMessages(interval(today, today) %/% months(1))
  # To suppress a message "Note: method with signature ..." by lubridate

  # Convert is_xxx variables from numeric or character to logical
  lgl_vars <- grep("^is_", cow_table_cols, value = T)
  cows_w_lgl_vars <- cows[, .SD, .SDcols = lgl_vars]
  cows_lgl_vars_converted <- lapply(cows_w_lgl_vars, function(x)
    as.logical(factor(x, levels = c("1", "TRUE", "0", "FALSE"),
                      labels = c("TRUE", "TRUE", "FALSE", "FALSE")))
    )
  cows[, (lgl_vars) := cows_lgl_vars_converted]

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
    cows_add_age <- sample.int(age_make_calf, n_cows_add, replace = T)
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
         age < delivery_age_table[1], `:=`(parity = 1)]
  }
  if (any(is.na(cows$date_last_delivery) & cows$parity != 0)) {
    cows[is.na(date_last_delivery) & parity != 0,
         `:=`(date_last_delivery = fifelse(age <= delivery_age_table[parity], 0,
              trunc(runif(.N, 0, param_calculated$calving_interval - 1)) * -1))]
  }

  if (anyNA(cows$is_replacement)) {
    is_na <- is.na(cows$is_replacement)
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
  is_na_date_dried <- is.na(cows$date_dried) &
                        (is.na(cows$stage) | cows$stage != "milking")
  months_milking <- integerize(param_calculated$months_milking)
  if (any(is_na_date_dried)) {
    cows[is_na_date_dried,
         `:=`(date_dried =
                fifelse(date_last_delivery * -1 > months_milking,
                        date_last_delivery + months_milking, NA_real_))]
  }
  cows[stage == "milking", `:=`(date_dried = NA)]
  if (anyNA(cows$stage)) {
    is_na <- is.na(cows$stage)
    cows[is_na & parity == 0, `:=`(stage = fifelse(age < 4, "calf", "heifer"))]
    cows[is_na & parity != 0,
         `:=`(stage = fifelse(is.na(date_dried), "milking", "dry"))]
  }

  cows$is_to_test_pregnancy[is.na(cows$is_to_test_pregnancy)] <- F
  cows$n_ai[is.na(cows$n_ai)] <- 0

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
  if (any(is_s)) {
    cows$infection_status[is_s] <- "s"
    items_coerced_to_s <- unique(cows$infection_status[is_s])
    message(glue(
      "Following item(s) in the `infection_status` column is treated as \\
       non-infected:
       {paste(items_coerced_to_s, collapse = ',')}"
          ))
  }
  if (!is.null(modify_prevalence)) {
    appropreate_n_inf <- round(n_cows * modify_prevalence)
    inf_count <- table(cows$infection_status != "s", useNA = "always")
    max_n_inf <- inf_count["TRUE"] + inf_count["<NA>"]
    is_na <- is.na(cows$infection_status)
    if (appropreate_n_inf < inf_count["TRUE"]) {
      cows[resample(which(infection_status != "s"),
                    inf_count["TRUE"] - appropreate_n_inf),
           `:=`(infection_status = "s")]
      cows$infection_status[is_na] <- "s"
    } else if (appropreate_n_inf <= max_n_inf) {
      cows[resample(which(is_na), max_n_inf - appropreate_n_inf),
           `:=`(infection_status = "ial")]
      cows$infection_status[is.na(cows$infection_status)] <- "s"
    } else {
      cows[resample(which(infection_status == "s"),
                    appropreate_n_inf - max_n_inf),
           `:=`(infection_status = "ial")]
      cows$infection_status[is_na] <- "ial"
    }
  } else if (anyNA(cows$infection_status)) {
    cows$infection_status[is.na(cows$infection_status)] <- "s"
  }

  cows[infection_status != "s" & is.na(date_ial), `:=`(date_ial = 0)]
  cows[(infection_status == "ipl" | infection_status == "ebl") &
        is.na(date_ipl),
       `:=`(date_ipl = 0)]
  cows[infection_status == "ebl" & is.na(date_ebl), `:=`(date_ebl = 0)]
  cows$cause_infection[cows$infection_status != "s"] <- "initial"

  if (!is.null(area_name)) {
    if (any(!cows$area_id %in% c(names(area_name), "0", NA_character_))) {
      stop(glue("`area_name` in the cow data contains area names \\
                 which are not contained in the area data."))
    }
    cows$area_id <-
      factor(cows$area_id, levels = c(names(area_name), "0"),
             labels = c(area_name, "0"))
  }
  cows$area_id <- as.integer(cows$area_id)
  if (anyNA(cows$area_id)) {
    cow_stage <- c("calf", "heifer", "milking", "dry")
    join_on <- c("stage", "parity")
    area_by_stage_and_parity <- cows[,
      list(freq_area =
             as.integer(names(sort(table(area_id), decreasing = T))[1])),
      by = join_on]
    area_by_stage_and_parity <-
      area_by_stage_and_parity[CJ(stage = cow_stage,
                                  parity = parity, unique = T),
                               on = join_on]
    cows <- area_by_stage_and_parity[cows, on = join_on]
    cows[, `:=`(area_id = fcoalesce(area_id, freq_area),
                freq_area = NULL)]
    area_id_in_input <- unique(na.omit(cows$area_id))
    empty_area_id <- setdiff(seq_len(length(area_id_in_input) + 4L),
                             area_id_in_input)[1:4]
    area_by_stage <- cows[,
      list(freq_area =
             as.integer(names(sort(table(area_id), decreasing = T))[1])),
           by = "stage"]
    area_by_stage <-
      area_by_stage[CJ(stage = cow_stage, sorted = F), on = "stage"]
    area_by_stage[, `:=`(freq_area = fcoalesce(freq_area, empty_area_id))]
    cows <- area_by_stage[cows, on = "stage"]
    cows[, `:=`(area_id = fcoalesce(area_id, freq_area),
                freq_area = NULL)]
  }

  cows[is.na(months_in_area), `:=`(months_in_area = 0)]
  cows[is.na(is_isolated), `:=`(is_isolated = F)]


  # Next, calculate values in columns users should not specify.
  rows_to_calc_longevity <- 1:n_cows
  # 1:n is used because it is much faster than seq_len(n).
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
  cows[is.na(date_got_pregnant),
       `:=`(day_heat = sample.int(30, .N, replace = T))]
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

  if (!is.null(n_chambers)) {
    for (i_area in names(n_chambers)) {
      empty_chambers <- setdiff(1:n_chambers[i_area],
                                cows$chamber_id[cows$area_id == i_area])
      # 1:n is used because it is much faster than seq_len(n).
      cows[area_id == i_area & is.na(chamber_id),
           `:=`(chamber_id = resample(empty_chambers, .N))]
    }
  }

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
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_BLV_spread()].
prepare_area <- function(csv, data = NULL, output_file = NULL,
                         sep = "[,\t\r\n |;:]") {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  cols_in_input <- intersect(area_table_cols, colnames(input))
  n_rows <- nrow(input)
  if (n_rows == 0) {
    stop("No data is contained in the area input.")
  }
  area_table <- a_area[rep(1, n_rows), ]
  area_table[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

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
  area_table$area_id <- na.locf(area_table$area_id)

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
    area_table$capacity[is_na] <- Inf
  }
  area_table$capacity <- as.character(area_table$capacity)
  # as.character() to when capacity is an integer/numeric vector.
  area_table$capacity <-
    paste0("c(", gsub(paste0(sep, "+"), ", ", area_table$capacity), ")")
  area_table$capacity <-
    lapply(area_table$capacity,
           function(x) gsub("(\\d+) ?x ?(\\d+)", "rep(\\1, \\2)", x, perl = T))
  area_table$capacity <-
    lapply(area_table$capacity, function(x) eval(parse(text = x)))
  area_table$capacity <- lapply(area_table$capacity, as.numeric)

  if (any(duplicated(area_table$area_id))) {
    area_table <- area_table[,
                             list(area_type = unique(na.omit(area_type))[1],
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
#'
#' @export
#' @return A csv file which can be used as an input for [simulate_BLV_spread()].
prepare_movement <- function(csv, data = NULL, output_file = NULL,
                             area_name = NULL, sep = "[,\t\r\n |;:]") {
  if (!missing(csv)) {
    input <- fread(csv)
  } else {
    input <- as.data.table(data)
  }

  cols_in_input <- intersect(movement_table_cols, colnames(input))
  n_rows <- nrow(input)
  if (n_rows == 0) {
    stop("No data is contained in the movement input.")
  }
  movement_table <- a_movement[rep(1, n_rows), ]
  movement_table[, (cols_in_input) := input[, .SD, .SDcols = cols_in_input]]

  necessary_cols <- c("current_area", "condition", "next_area")
  necessary_data <- movement_table[, .SD, .SDcols = necessary_cols]
  if (anyNA(necessary_data)) {
    missing_cols <- necessary_cols[vapply(necessary_data, anyNA, F)]
    stop(glue(
      "Following column(s) in the movement data must not contain missing value:
       {paste0('`', missing_cols, '`', collapse = ', ')}"
     ))
  }

  movement_table$next_area <-
    strsplit(as.character(movement_table$next_area), paste0(sep, "+"))
  # as.character() to when capacity is an integer/numeric vector.
  if (!is.null(area_name)) {
    if (any(!unique(movement_table$current_area) %in% c(names(area_name), "0"))) {
      stop(glue("`current_area` in the movement data contains \\
                 an area name which is not contained in the area data."))
    }
    if (any(
        !unique(unlist(movement_table$next_area)) %in% c(names(area_name), "0")
        )) {
      stop(glue("`next_area` in the movement data contains \\
                 an area name which is not contained in the area data."))
    }

    movement_table$current_area <- factor(movement_table$current_area,
      levels = c(names(area_name), "0"), labels = c(area_name, "0"))
    movement_table$next_area <- lapply(movement_table$next_area,
      function(x) factor(x, levels = c(names(area_name), "0"),
                         labels = c(area_name, "0"))
      )
  }
  movement_table$current_area <- as.integer(movement_table$current_area)
  movement_table$next_area <- lapply(movement_table$next_area, as.integer)

  movement_table$priority <-
    strsplit(as.character(movement_table$priority), paste0(sep, "+"))
  # as.character() to when capacity is an integer/numeric vector.
  movement_table$priority <- lapply(movement_table$priority, as.numeric)
  n_priority <- vapply(movement_table$priority, length, 1)
  if (anyNA(movement_table$priority) | any(n_priority == 0)) {
    n_next_area <-
      vapply(movement_table$next_area, function(x) length(na.omit(x)), 1)
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

  if (!is.null(output_file)) {
    fwrite(movement_table, output_file)
  }

  return(movement_table)
}


#' Process raw data to suitable forms
#'
#' Process raw data to suitable forms to use in simulation.
#'
#' @param excel Set this or `cow_data`, `area_data` and `movement_data`.
#' @param param See [param].
#' @param output When `TRUE`, create output csv files with names of "cow.csv", "area.csv" and "movement.csv" into a working directory. Shorthand form of setting "xxx.csv" to `xxx_output_file`.
#' @param cow_data See [prepare_cow()] for detail.
#' @param area_data See [prepare_area()] for detail.
#' @param movement_data See [prepare_movement()] for detail.
#' @param cow_output_file,area_output_file,movement_output_file If not `NULL`, created data is exported to the files with these names (must be csv files).
#' @param sep Separatator used in `capacity` column of area data. See [prepare_area()] for detail.
#' @param ... Other arguments passed to [prepare_cow()].
#'
#' @export
#' @return csv files which can be used as an input for [simulate_BLV_spread()].
prepare_data <- function(excel, param, output = F,
                         cow_data = NULL, area_data = NULL,
                         movement_data = NULL,
                         cow_output_file = NULL, area_output_file = NULL,
                         movement_output_file = NULL,
                         sep = "[,\t\r\n |;:]", ...) {
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

  cows <- prepare_cow(data = cow_input, param = param,
                      output_file = cow_output_file, area_name = area_name, ...)
  areas <- prepare_area(data = area_input,
                        output_file = area_output_file, sep = sep)
  movement <- prepare_movement(data = movement_input,
                               output_file = movement_output_file,
                               area_name = area_name, sep = sep)

  return(list(cows = cows, areas = areas, movement = movement))
}

