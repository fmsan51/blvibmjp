.onUnload <- function(libpath) {
  # library.dynam.unload("blvibmjp", libpath)  # Enable this when using Rcpp
}


.onLoad <- function(libname, pkgname) {
  suppressMessages(interval(today, today) %/% months(1))
  # To suppress a message "Note: method with signature ..." by lubridate
  # when runnig prepare_cows()
}


# Run and delete the following when using Rcpp
# usethis::use_package(c("Rcpp", "BH"), type = "LinkingTo")

# To prevent a warning "undefined gloval variable xxx" while devtools::check()
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(
    ".", "..cow_table_cols", "..lgl_vars", "..necessary_cols",
    "adjoint_next_chamber", "adjoint_previous_chamber", "age", "area_id", "area_table", "area_type",
    "capacity", "capacity_as_ratio", "cause_infection", "cause_removal", "chamber_id", "cond", "cow_id", "cow_status",
    "date_birth", "date_dried", "date_ebl", "date_got_pregnant", "date_ial", "date_ipl", "date_ipl_expected", "date_ebl_expected", "date_last_delivery", "date_removal_expected", "day_heat", "day_heat1", "day_heat2", "day_last_detected_heat", "day_next_heat",
    "id_calf", "infection_status", "is_after_inf", "is_ai1_successed", "is_ai2_successed", "is_detected", "is_freemartin", "is_heat1_detected", "is_heat2_detected", "is_infected", "is_isolated", "is_owned", "is_pregnant", "is_to_test_pregnancy", "i_heat", "i_month", "i_rp", "i_simulation",
    "N", "n_ai", "n_heat", "n_heat_from_ai", "n_litter",
    "parity", "prevalence", "p_inf",
    "sex", "se_upr", "se_est", "se_lwr", "sp_est", "sp_lwr", "sp_upr", "stage", "susceptibility_ial_to_ipl", "susceptibility_ipl_to_ebl",
    "total", "total_inf", "type", "type_prev",
    "uninfected"
  ))
}

