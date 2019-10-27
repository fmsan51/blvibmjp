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
                      "age > 20 & parity == 3",
                      NA)
  area[, `:=`(area_id = 1:8,
              area_type = "free",
              capacity = Inf,
              next_area = 2:9,
              priority = 1)]
  area <- setup_area_table(area)
  cows <- a_new_calf[rep(1, 8 * 2), ]
  cows$area_id <- rep(1:8, each = 2)
  cows[c(1:2), age := c(21, 20)]
  cows[c(3:4), parity := c(2, 1)]
  day <- 10
  cows[c(5:6), `:=`(date_last_delivery = c(day, day - 1),
                    i_month = day)]
  cows[c(7:8), `:=`(date_got_pregnant = c(day, day - 1),
                    i_month = day)]
  cows[c(9:10), `:=`(date_dried = c(day, day - 1),
                     i_month = day)]
  cows[c(11:12), `:=`(date_delivered = c(day - 4, day),
                      i_month = day)]
  cows[c(13:14), `:=`(age = c(21, 20),
                      parity = c(3, 3))]
  cows <- change_area(cows, area)




})

