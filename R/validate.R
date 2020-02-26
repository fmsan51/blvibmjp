#' Validate cow_table
#'
#' Validate `cow_table`.
#'
#' @param cows A [cow_table].
#' @param date_format One of "ymd", "ydm", "myd", "mdy", "dym", "dmy".
#' @param today A `date` class object indicates today.
#'
#' @return A [cow_table] whose columns are fomatted.
validate_cow_table <- function(cows,
                               date_format = "ymd",
                               today = lubridate::today(tzone = "Asia/Tokyo")) {
  cows <- copy(cows)

  # Check missing parameters
  for (i in c("stage", "parity")) {
    # TODO: add a function to automatically set parameters which are not specified by user input with warning.
    if (anyNA(cows[[i]])) {
      stop(glue("there must not be missing value in column `{i}`."), call. = F)
    }
  }

  for (i in c("date_death", "date_death_expected", "is_owned", "cause_removal",
              "day_heat", "day_last_heat", "n_heat_from_ai",
              "date_ipl_expected", "date_ebl_expected", "cause_infection",
              "susceptibility_ial_to_ipl", "susceptibility_ipl_to_ebl",
              "area_id", "i_month")) {
    if (!all(is.na(cows[[i]]))) {
      stop(glue("column `{i}` must not specified by users."))
    }
  }
  validate_category(cows, "stage", c("calf", "heifer", "milking", "dry"))
  validate_category(cows, "sex",
                    c("female", "male", "freemartin", "f1-female", "f1-male",
                      "black-female", "black-male"))
  validate_category(cows, "infection_status", c("s", "ial", "ipl", "ebl"))

  # Check duplicated ids
  if (any(duplicated(cows$cow_id))) {
    duplicated_ids <- unique(cows$cow_id[duplicated(cows$cow_id)])
    stop(glue("following `cow_id` is duplicated: \\
              {paste(duplicated_ids, collapse = ', ')}
              `cow_id` must not be duplicated."), call. = F)
  }

  # Check types of numeric variables
  for (i in "age") {
    if (any(cows[[i]] %% 1 != 0 | cows[[i]] < 0, na.rm = T)) {
      stop(glue("column `{i}` must contains positive integer only."), call. = F)
    }
  }

  # age and dates
  if (!all(is.na(cows$age) | is.na(cows$date_birth)) |
      any(is.na(cows$age) & is.na(cows$date_birth))) {
    stop("input either one of `age` or `date_birth`.", call. = F)
  }
  cows <- format_date(cows, date_format, today)

  # sex and race
  if (any(cows[age >= 6, sex] != "female")) {
    ans <- menu(c("continue", "stop and fix"),
                title = glue("there is a cow older than 5 months old and \\
                             is free-martin, f1 or Japanese-black in the herd.
                             Will you continue or fix it?"))
    if (ans == 0 | ans == 2) {
      stop("stopped.", call. = F)
    }
  }

  # stage
  if (any(cows[stage %in% c("calf", "heifer"), parity] != 0)) {
    stop("`parity` for calves and heifers must be 0.", call. = F)
  }
  if (any(cows[stage %in% c("milking", "dry"), parity] == 0)) {
    stop("`parity` for milking cows and dry cows must not be 0.", call. = F)
  }
  if (any(!is.na(cows[stage %in% c("calf", "heifer"), date_last_delivery]))) {
    stop("`date_last_delivery` for calves and heifers must be NA.", call. = F)
  }
  if (any(!is.na(cows[stage == "calf", date_got_pregnant]))) {
    stop("`date_got_pregnant` for calves must be NA.", call. = F)
  }

  if (any(cows[stage == "calf", age] >= 10)) {
    ans <- menu(c("continue", "stop and fix"),
                title = glue("There are calves who are older than 9 months old.
                             Will you continue or fix it?"))
    if (ans == 0 | ans == 2) {
      stop("stopped.", call. = F)
    }
  }
  if (cows[stage == "heifer" & (age < 2 | 30 <= age), .N] != 0) {
    ans <- menu(c("stop", "stop and fix"),
                title = glue("There are heifers who are younger than 2 months \\
                             old or older than 30 months old.
                             Will you continue for fix it?"))
    if (ans == 0 | ans == 2) {
      stop("stopped.", call. = F)
    }
  }
  if (any(cows[stage %in% c("milking", "dry"), age] < 18)) {
    ans <- menu(c("continue", "stop and fix"),
                title = glue("There is milking cow or dry cow who is younger \\
                             than 18 months old.
                             Will you continue for fix it?"))
    if (ans == 0 | ans == 2) {
      stop("stopped.", call. = F)
    }
  }

  # Delivery and milking
  if (cows[!is.na(date_got_pregnant) & date_got_pregnant <= -12, .N] != 0) {
    stop(glue("`date_got_pregnant` must not more than 12 months ago.
              Set NA if a cow is open."), call. = F)
  }
  if (cows[!is.na(date_got_pregnant) & !is.na(n_ai), .N] != 0) {
    stop(glue("`n_ai` of a pregnant cow \\
              (a cow whose `date_got_pregnant` is NA) must be 0."), call. = F)
  }

  return(cows)

  # TODO: chamber_idとis_isolatedの設定をしたいなー（Free-stall以外で設定してはいけない）
}


#' Validate that categorical columns contain valid categories only
#'
#' @param cows A [cow_table].
#' @param col Name of a column to validate.
#' @param category Possible categories.
#'
#' @return Stop if the `col` contains invalid category.
validate_category <- function(cows, col, category) {
  values <- cows[[col]]
  if (any(!(values %in% category))) {
    invalid_value <- unique(values[!(values %in% category)])
    stop(glue("column `{col}` contains invalid value(s): \\
              {paste(invalid_value, collapse = ', ')}
              column `{col}` must contain only following values: \\
              {paste(category, collapste = ', ')}"), call. = F)
  }
}


#' Format and validate date columns
#'
#' @param cows A [cow_table].
#' @param date_format One of "ymd", "ydm", "myd", "mdy", "dym", "dmy".
#' @param today A `date` class object indicates today.
#'
#' @return A [cow_table] whose date columns are formatted.
format_date <- function(cows, date_format = "ymd",
                        today = lubridate::today(tzone = "Asia/Tokyo")) {
  cows <- copy(cows)

  if (all(date_format != c("ymd", "ydm", "myd", "mdy", "dym", "dmy"))) {
    stop("date_format must be one of ymd, ydm, myd, mdy, dym, dmy", call. = F)
  }
  date_formatter <- function(date) {
    do.call(date_format, list(date, tz = tz(today), locale = "C"))
  }

  # Validation of columns
  date_cols <- grep("^date_", colnames(cows), value = T)
  for (date_col in c(date_cols, "day_last_heat_detected")) {
    date_type <- classify_date_column(cows[[date_col]], date_format)
    if (date_type == "numeric" &
        any(cows[[date_col]] %% 1 != 0 | cows[[date_col]] > 0, na.rm = T)) {
      stop(glue("when you specify `{date_col}` by number (not Date), \\
                it must be an integer equal to or lower than 0."),
           call. = F)
    } else if (date_type == "date") {
      if (date_col == "day_last_heat_detected") {
        cows[[date_col]] <- day(date_formatter(cows[[date_col]]))
        cows[[date_col]][cows[[date_col]] == 31] <- 1
      } else {
        cows[[date_col]] <- floor(
          interval(today, date_formatter(cows[[date_col]])) / months(1, F)
          )
        if (any(cows[[date_col]] < 0, na.rm = T)) {
          stop("date in `{date_col}` must be preceded by `today`.")
        }
      }
    } else if (date_type == "invalid") {
      stop(glue("date format of column `{date_col}` is invalid."))
    }
  }

  return(cows)
}


#' Check date_xxx columns whether they are numeric, date or invaild value.
#'
#' @param date_col date_xxx column
#' @param date_format See [validate_cow_table].
#'
#' @return A character one of "numeric", "date" or "invalid".
classify_date_column <- function(date_col, date_format) {
  is_numeric <- tryCatch({as.numeric(date_col); T},
                          warning = function(e) {return(F)})
  is_date <- tryCatch({do.call(date_format, list(date_col)); T},
                      warning = function(e) {return(F)})
  if (is_numeric & !is_date) {
    col_class <- "numeric"
  } else if (!is_numeric & is_date) {
    col_class <- "date"
  } else {
    col_class <- "invalid"
  }
  return(col_class)
}

