context("Data input")

test_that("test_data with acceptable data format", {

  expect_true(test_data(matrix(1:100, 10)))
  expect_true(test_data(data.frame(1:10, 11:20, 21:30)))

})

test_that("test_data fails if data is not a matrix or a dataframe", {

  expect_error(test_data(list(1:10, 11:20, 21:30)))

})



context("Sets input (character or numeric)")

test_that("test_sets passes with a list of vectors", {

  expect_true(test_sets_all(matrix(1:100, 5, 20), list(1:5, 6:10, 11:15, 16:20)))

})

test_that("test_sets fails if 'sets' is not a list or is empty, has a mix of numeric and character values, contains more columns than the data set or contains elements that are not vectors", {

  expect_error(test_sets_all(matrix(1:100, 5, 20), matrix(1:9, 3)))
  expect_error(test_sets_all(matrix(1:100, 5, 20), list()))
  expect_error(test_sets_all(matrix(1:100, 5, 20), list(1, 2, "a", "b", 3, 4)))
  expect_error(test_sets_all(matrix(1:100, 5, 20), as.list(1:25)))
  expect_error(test_sets_all(matrix(1:100, 5, 20), list(1:3, 4:6, list(7, 8, 9))))

})



context("Sets of numeric vectors")

test_that("test_sets passes with a list of numeric vectors", {

  expect_true(test_sets_numeric(matrix(1:100, 5, 20), list(1:5, 6:10, 11:15, 16:20)))

})

test_that("test_sets fails if 'sets' contains negative or non-integer values, or contains a column number that does not exist in the data", {

  expect_error(test_sets_numeric(matrix(1:100, 5, 20), list(-1, 0, 1)))
  expect_error(test_sets_numeric(matrix(1:100, 5, 20), list(1, 2, 3.5)))
  expect_error(test_sets_numeric(matrix(1:100, 5, 20), list(5, 10, 15, 20, 25)))

})



context("Center and scale input")

test_that("test_center_scale passes with a logical value or a numeric vector whose length is the number of active variables", {

  expect_true(test_center_scale(TRUE, list(1:3, 4:6, 7:10)))
  expect_true(test_center_scale(11:20, list(1:3, 4:6, 7:10)))

})

test_that("test_center_scale fails with a value that is not a logical and is not a numeric vector with proper length", {

  expect_error(test_center_scale("yes", list(1:3, 4:6, 7:10)))
  expect_error(test_center_scale(11:15, list(1:3, 4:6, 7:10)))

})



context("Output of data_tables function")

test_that("test_tables passes with a list of matrices with the same number of rows", {

  expect_true(test_tables(list(matrix(1:9, 3), matrix(10:18, 3), matrix(19:27, 3))))

})

test_that("test_tables fails if any element of the list is not a matrix or if they have different numbers of rows", {

  expect_error(test_tables(list(1:10, matrix(1:9, 3), matrix(10:18, 3))))
  expect_error(test_tables(list(matrix(1:9, 3), matrix(10:18, 3), matrix(19:26, 2))))

})

test_that("data_tables gives correct output (first table)", {
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))

  table <- round(data_tables(wines, sets, TRUE, scaling_vec)[[1]], 2)
  attr(table,"dimnames") <- NULL
  attr(table,"scaled:center") <- NULL
  attr(table,"scaled:scale") <- NULL
  expect_identical(table, matrix(c(0.30, 0.16, 0.02, 0.43, -0.52, -0.39, -0.11, -0.11, 0.30, -0.25, 0.30, -0.11, 0.32, 0.13, 0.13, 0.32, -0.45, -0.06, -0.26, -0.45, 0.32, 0.32, -0.06, -0.26, 0.18, 0.31, 0.04, 0.31, -0.49, -0.22, -0.09, -0.22, 0.31, -0.49, 0.31, 0.04, -0.09, -0.45, 0.03, -0.09, 0.39, 0.51, -0.09, 0.39, -0.09, 0.03, -0.45, -0.09, -0.44, -0.31, -0.17, -0.17, 0.37, 0.24, 0.51, 0.37, -0.03, -0.17, -0.17, -0.03, 0.27, 0.57, -0.02, 0.12, -0.17, -0.47, -0.17, -0.02, 0.42, -0.02, -0.17, -0.32), 12))

})



context("Number of components")

test_that("test_ncomp passes with a valid number of components or NULL", {

  expect_true(test_ncomp(2, list(matrix(1:10, 2), matrix(11:20, 2), matrix(21:30, 2))))
  expect_true(test_ncomp(NULL, list(matrix(1:10, 2), matrix(11:20, 2), matrix(21:30, 2))))

})

test_that("test_ncomp fails if number of components is negative or 0, or exceeds the maximum rank of the data", {

  expect_error(test_ncomp(-5, list(matrix(1:10, 2), matrix(11:20, 2), matrix(21:30, 2))))
  expect_error(test_ncomp(10, list(matrix(1:10, 2), matrix(11:20, 2), matrix(21:30, 2))))

})


context("Output of mfa function: eigenvalues")

test_that("mfa function yields correct eigenvalues", {

  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))

  expect_identical(round(mfa(wines, sets, ncomps = NULL, center = TRUE, scale = scaling_vec)$eigen_values, 3), c(0.770, 0.123, 0.091, 0.076, 0.060, 0.039, 0.031, 0.025, 0.019, 0.013, 0.011, 0.000))

})



context("Output of mfa function: common factor scores")

test_that("mfa function yields correct common factor scores", {
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))


  expect_identical(round(mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)$common_factor_scores, 3), matrix(c(-0.980, -0.809, -0.761, -1.115, 1.373, 1.264, 0.808, 0.925, -0.669, 0.073, -0.476, 0.367, 0.163, 0.033, -0.454, -0.166, -0.128, -0.108, 0.205, 0.408, 0.369, -0.757, 0.513, -0.076), 12))

})



context("Output of mfa function: partial factor scores")

test_that("mfa function yields correct first partial factor score", {
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))

  partial <- round(mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)$partial_factor_scores[[1]], 3)
  attr(partial,"dimnames") <- NULL

  expect_identical(partial, matrix(c(-1.037, -1.179, -0.213, -0.946, 1.546, 1.176, 0.698, 1.006, -0.922, 0.189, -0.643, 0.323, 0.155, 0.596, -0.104, 0.446, -0.676, -0.747, 0.166, -0.063, 0.486, -0.936, 0.640, 0.036), 12))

})


context("Output of mfa function: loadings")

test_that("mfa function yields correct loadings", {
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))


  expect_identical(round(mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)$loadings[,1], 3), c(-0.294, -0.267, -0.260, 0.241, 0.286, -0.233, -0.297, -0.296, -0.267, 0.256, -0.238, -0.222, -0.305, -0.136, -0.258, 0.203, -0.277, 0.267, -0.313, -0.261, -0.303, 0.230, -0.205, -0.296, -0.213, -0.268, 0.124, -0.259, 0.177, -0.302, -0.277, -0.265, 0.231, -0.205, -0.275, -0.246, -0.277, 0.180, -0.276, -0.247, -0.235, 0.138, -0.286, 0.239, -0.303, -0.235, -0.287, 0.251, -0.296, -0.323, -0.274, -0.286, 0.282))
  expect_identical(round(mfa(wines, sets, ncomps = 2, center = TRUE, scale = scaling_vec)$loadings[,2], 3), c(0.318, -0.248, 0.396, -0.184, 0.161, 0.129, 0.183, -0.178, 0.200, -0.240, -0.113, -0.333, 0.234, -0.228, 0.379, -0.365, -0.297, 0.283, 0.082, -0.353, -0.169, 0.066, -0.117, 0.201, -0.249, 0.258, 0.132, -0.144, 0.019, 0.215, -0.274, 0.328, 0.031, -0.340, 0.380, -0.410, 0.290, 0.376, 0.309, -0.376, 0.231, -0.219, -0.261, 0.293, 0.241, -0.221, 0.226, -0.083, -0.188, 0.080, -0.262, 0.187, 0.272))

})


context("Output of weights function")

test_that("weight function yields correct weights", {
  wines <- read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv", stringsAsFactors = FALSE)
  sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
  scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))


  expect_identical(round(weights(data_tables(wines, sets, TRUE, scaling_vec)), 3), c(0.241, 0.239, 0.275, 0.273, 0.307, 0.302, 0.417, 0.272, 0.264, 0.309))

})
