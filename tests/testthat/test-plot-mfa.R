context("Plotting Method")


test_that("check_sides fails with invalid lengths", {
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
  tables <- data_tables(wines, sets, TRUE, scaling_vec)
  mymfa <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)
  
  expect_error(plot(mymfa, type = 1, d1 = 1, d2 = 3))
  expect_error(plot(mymfa, type = 2, X = 1, d1 = 1, d2 = 3))
  expect_error(plot(mymfa, type = 3, X = 1, d1 = 1, d2 = 3))
  expect_error(plot(mymfa, type = 4, X = 1, d1 = 1, d2 = 3))
  expect_error(plot(mymfa, type = 5, X = 1, d1 = 1, d2 = 3))
  
})