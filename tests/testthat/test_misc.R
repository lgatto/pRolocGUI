context("Functions work as expected")

test_that("colours are valid", {
  cols <- appStockcol()
  expect_true(inherits(cols, "character"))
})

test_that("remap fails if only one msnset is passed", {
  dat1 <- data(dunkley2006)
  expect_error(remap(dat1))
})

