test_that("isuppercase() works", {
  skip_on_cran()
  library(mixequiv)

  expect_true(is.uppercase("HELLO"))
  expect_false(is.uppercase("hello"))
  expect_false(is.uppercase("Hello"))
  expect_false(is.uppercase("12345"))
  expect_false(is.uppercase("HELLO!"))
})