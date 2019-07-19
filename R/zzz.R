.onUnload <- function(libpath) {
  library.dynam.unload("blvibmjp", libpath)
}


# To prevent a warning "undefined gloval variable xxx" while devtools::check()
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(
    ".",
    "cause_infection",
    "infection_status", "is_owned", "i_month", "i_simulation",
    "N",
    "prevalence", "p_inf",
    "total",
    "uninfected"
  ))
}

