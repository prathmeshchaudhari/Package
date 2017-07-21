library(testthat)
library(Package)
test_check('Package')
test_that("Return",{
  data(dt)
  expect_that(make_filename(2015), matches('accident_2015.csv.bz2'))
})
