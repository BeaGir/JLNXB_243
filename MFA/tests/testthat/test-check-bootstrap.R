context("bootstrap arguments")

test_that("check_value with ok numbers",{
  expect_true(check_value(100))
})

test_that("check_value fails with invalid inputs",{
  expect_error(check_value(-200))
  expect_error(check_value(0))
  expect_error(check_value(5.5))
  expect_error(check_value(c(10,20)))
})
