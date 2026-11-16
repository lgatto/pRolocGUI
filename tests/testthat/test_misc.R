context("Functions work as expected")

test_that("colours are valid", {
  cols <- appStockcol()
  expect_true(inherits(cols, "character"))
})

test_that("remap fails if only one msnset is passed", {
  dat1 <- data(dunkley2006)
  expect_error(remap(dat1))
})


test_that("remap projects all datasets onto the reference PCs", {
  data(hyperLOPIT2015ms3r1)
  data(hyperLOPIT2015ms3r2)
  xx <- commonFeatureNames(hyperLOPIT2015ms3r1, hyperLOPIT2015ms3r2)
  sampleNames(xx@x[[2]]) <- sampleNames(xx[[1]])
  res <- remap(xx)
  expect_identical(sampleNames(res[[1]]), sampleNames(res[[2]]))
  expect_false(isTRUE(all.equal(exprs(res[[2]]), exprs(xx[[2]]),
                                check.attributes = FALSE)))
})
