context("change_area")
cows <- a_new_calf()

test_that("condition works", {
  # Condition works
  area <- a_area[rep(1, 8), ]
  area$condition <- c("age > 20",
                      "parity > 1",
                      "delivery",
                      "pregnancty",
                      "dry",
                      "dim > 100",
                      NA,
                      "age > 20 & parity == 3")
  area[, `:=`(area_id = 1:8,
              area_type = "free",
              capacity = Inf,
              next_area = 2:9,
              priority = 1)]
  cows <- a_new_calf[rep(1, 8 * 2), ]
  cows <- areaarea_id


})

