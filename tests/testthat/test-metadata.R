test_that("imd_meta rainfall dims", {
  m <- imdR:::imd_meta
  expect_equal(m$rain$ncols, 135); expect_equal(m$rain$nrows, 129); expect_equal(m$rain$res, 0.25)
})
test_that("imd_meta temp dims", {
  m <- imdR:::imd_meta
  expect_equal(m$tmax$ncols, 31); expect_equal(m$tmax$res, 1); expect_equal(m$tmin$res, 1)
})
test_that("imd_endpoints has all vars", {
  e <- imdR:::imd_endpoints
  expect_true(all(c("rain","tmax","tmin") %in% names(e)))
  expect_true(grepl("imdpune", e$rain$url))
})
