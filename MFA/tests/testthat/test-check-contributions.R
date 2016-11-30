context("contributions functions arguments")

test_that("check_mfa with ok mfa",{
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
  mymfa <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)

  expect_true(check_mfa(mymfa))
}
)

test_that("check_mfa with invalid mfa",{
  expect_error(check_mfa(c(4,7,9)))
}
)

test_that("check_contrib with ok arguments",{
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
  tables <- data_tables(wines, sets, TRUE, scaling_vec)
  mymfa <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)

  sets <- lapply(sets, FUN=function(vec){vec-1})
  expect_true(check_contrib(mymfa, sets, tables))

}
)

test_that("check_contrib with invalid arguments",{
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
  tables <- data_tables(wines, sets, TRUE, scaling_vec)
  mymfa <- mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)

  # With 'sets' not a list
  sets <- c(1,4,6)
  expect_error(check_contrib(mymfa, sets, tables))

  # With 'sets' going beyond the number of variables in the mfa
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  expect_error(check_contrib(mymfa, sets, tables))

  # With 'tables' not consistent with the mapping 'sets'
  sets <- lapply(sets, FUN=function(vec){vec-1})
  tables <- list(tables[[1]], tables[[2]])
  expect_error(check_contrib(mymfa, sets, tables))
}
)
