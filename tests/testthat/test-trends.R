test_that("trend detects increase", {
  df <- data.frame(year=2000:2020, total=as.numeric(2000:2020))
  res <- trend_analysis(df, index_col="total", file_dir=tempdir(), plot=FALSE)
  expect_gt(res$sens_slope, 0); expect_lt(res$pvalue, 0.05)
  expect_equal(res$trend_direction, "Increasing")
})
test_that("trend detects decrease", {
  df <- data.frame(year=2000:2020, total=as.numeric(2020:2000))
  res <- trend_analysis(df, index_col="total", file_dir=tempdir(), plot=FALSE)
  expect_lt(res$sens_slope, 0); expect_equal(res$trend_direction, "Decreasing")
})
test_that("trend errors on missing col", {
  df <- data.frame(year=2000:2010, total=1:11)
  expect_error(trend_analysis(df, index_col="nonexistent", file_dir=tempdir()))
})
test_that("trend needs >=3 years", {
  df <- data.frame(year=2000:2001, total=c(5,6))
  expect_error(trend_analysis(df, index_col="total", file_dir=tempdir()))
})
