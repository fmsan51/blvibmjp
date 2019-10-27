test_that("simulation runs", {
  param_simulation$n_simulation <- 1
  param_simulation$simulation_length <- 5
  param_simulation$input_csv <- system.file("testdata", "input", "test_cow.csv",
                                            package = "blvibmjp")
  param_simulation$output_dir <- system.file("testdata", "output",
                                             package = "blvibmjp")
  param_farm$months_grazing <-  6:10
  param_farm$hours_grazing <- 0:23
  param_farm$change_gloves <- T
  param_area$n_area <- 4
  param_area$xy_chamber <- list(NA, NA, c(25, 8), NA)
  param_area$is_calf_separated <- F
  param_area$is_milking_dry_separated <- T

  expect_warning(
    simulate_blv_spread(param_simulation, param_farm, param_area,
                        list_param_modification = NULL,
                        save_cows = F, save_param = F,
                        i_simulation_start = 1),
    NA)
})
# TODO: 全てのoptionをtest
