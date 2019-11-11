# test_that("simulation runs", {
  param_simulation$n_simulation <- 1
  param_simulation$simulation_length <- 5
  param_simulation$input_csv <- system.file("testdata", "input", "test_cow.csv",
                                            package = "blvibmjp")
  param_simulation$output_dir <-
    file.path(dirname(dirname(param_simulation$input_csv)), "output")
  param_farm$months_grazing <-  6:10
  param_farm$hours_grazing <- 0:23
  param_farm$change_gloves <- T

  area_table <- a_area[rep(1, 5), ]
  area_table[, `:=`(area_id = 1:5,
                    area_type = c("free", "free", "free", "outside", "tie"),
                    capacity = list(NA, NA, NA, NA, c(20, 20, 30)))]

  movement_table <- a_movement[rep(1, 5), ]
  movement_table[, `:=`(current_area = 1:5,
                        condition = c("age > 3", "age > 12", "delivery", "dry", "delivery | dry"),
                        next_area = list(2L, 3L, 4L, 5L, c(4L, 5L)),
                        priority = list(NA, NA, NA, NA, c(2, 1)))]

  # if (is_sensitivity_analysis) {
  #   param_sensitivity <- fread(system.file("testdata", "input",
  #                              "param_sensitivity.csv", package = "blvibmjp"))
  #   fwrite(param_sensitivity,
  #          system.file("testdata", "output", "sensitivity.csv"))
  #   row_param <- seq_len(nrow(param_sensitivity) - 3) + 3
  #   param_modification <- param_sensitivity[row_param, 4:103]
  #   name_param <- param_sensitivity[[1]][row_param]
  #   for (i in seq_len(100)) {
  #     param_modification[[i]] <- as.list(param_modification[[i]])
  #     names(param_modification[[i]]) <- name_param
  #   }
  # }

  expect_warning(
    simulate_blv_spread(param_simulation, param_farm, param_area,
                        area_table, movement_table,
                        communal_pasture_table = NULL,
                        list_param_modification = NULL,
                        save_cows = T, save_param = T,
                        i_simulation_start = 1),
    NA)

  # simulation_csv <- system.file("testdata", "output", "simulation01.csv",
  #                               package = "blvibmjp")
  # calculate_prevalences(simulation_csv)
  # plot_prevalences(simulation_csv)
  # plot_infection_route(simulation_csv)
# })
# TODO: 全てのoptionをtest
