context("rv arguments")

test_that("check_table with ok tables",{
  expect_true(
    check_table(
      matrix(1, nrow = 4, ncol = 3),matrix(c(4,7), nrow = 4, ncol = 4)))
}
)

test_that("check_table with invalid tables",{
  expect_error(
    check_table(
      matrix(1, nrow = 5, ncol = 3),matrix(c(4,7), nrow = 4, ncol = 4)))
  expect_error(
    check_table(
      matrix("a", nrow = 4, ncol = 3),matrix(c(4,7), nrow = 4, ncol = 4)))
}
)

test_that("check_set with ok dataset and sets",{
  expect_true(
    check_sets(
      data.frame(V1 = 1, V2 = 1:10, V3 = c(-1,1), V4 = runif(5), V5 = runif(5)),
      list(1:2, 3:4)))
}
)

test_that("check_set with invalid dataset and sets",{
  expect_error(
    check_sets(
      data.frame(V1 = "a", V2 = 1:10, V3 = c(-1,1), V4 = runif(5), V5 = runif(5)),
      list(1:2, 3:4)))
  expect_error(
    check_sets(
      data.frame(V1 = 1, V2 = 1:10, V3 = c(-1,1), V4 = runif(5), V5 = runif(5)),
      list(1:2, 3:6)))
  expect_error(
    check_sets(
      data.frame(V1 = "a", V2 = 1:10, V3 = c(-1,1), V4 = runif(5), V5 = runif(5)),
      c(1,2,3)))
}
)