.onUnload <- function(libpath) {
  # library.dynam.unload("blvibmjp", libpath)  # Enable this when using Rcpp
}

# Run and delete the following when using Rcpp
# usethis::use_package(c("Rcpp", "BH"), type = "LinkingTo")

# To prevent a warning "undefined gloval variable xxx" while devtools::check()
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(
    ".",
    "age",
    "cause_infection", "cause_removal", "chamber_id", "cow_id", "cow_status",
    "date_death_expected", "date_got_pregnant", "date_last_delivery", "day_heat", "day_heat1", "day_heat2", "day_last_heat_detected", "day_next_heat",
    "group_id",
    "infection_status", "is_after_inf", "is_ai1_successed", "is_ai2_successed", "is_edge1", "is_edge2", "is_exposed", "is_freemartin", "is_heat1_detected", "is_heat2_detected", "is_infected", "is_isolated", "is_pregnant", "is_owned", "i_heat", "i_month", "i_rp", "i_simulation",
    "N", "neighbor1_infectivity", "neighbor1_isolated", "neighbor1_status", "neighbor2_infectivity", "neighbor2_isolated", "neighbor2_status", "n_ai", "n_heat", "n_heat_from_ai",
    "parity", "prevalence", "p_inf",
    "sex", "stage", "susceptibility_ial_to_ipl", "susceptibility_ipl_to_ebl",
    "total", "total_inf", "type",
    "uninfected"
  ))
}

