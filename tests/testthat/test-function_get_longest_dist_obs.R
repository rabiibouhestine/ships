library(readr)

test_that("get_longest_dist_obs works", {

  sample <- read_csv("../../data/ships_sample.csv", n_max = 100000)

  result <- get_longest_dist_obs(data = sample, ship_name = "KAROLI")

  expect_equal(nrow(result), 2)

  result <- get_longest_dist_obs(data = sample, ship_name = "KERLI")

  expect_equal(nrow(result), 2)

  result <- get_longest_dist_obs(data = sample, ship_name = "FINNKRAFT")

  expect_equal(nrow(result), 2)

  result <- get_longest_dist_obs(data = sample, ship_name = "IDUNA")

  expect_equal(nrow(result), 2)
})
