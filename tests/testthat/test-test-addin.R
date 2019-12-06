context("add-in tests")

library(shinytest)

test_that("labels", {
  skip_on_cran()
  expect_pass(testApp("apps/make_labels/", compareImages = FALSE))
})

