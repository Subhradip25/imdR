test_that("to_geotiff writes file", {
  r <- make_synthetic_rast(matrix(c(1,2,3), nrow=1))
  f <- file.path(tempdir(), "t.tif"); to_geotiff(r, f)
  expect_true(file.exists(f))
})
test_that("to_netcdf writes file", {
  r <- make_synthetic_rast(matrix(c(1,2,3), nrow=1))
  f <- file.path(tempdir(), "t.nc"); to_netcdf(r, f, "rain")
  expect_true(file.exists(f))
})
test_that("to_csv returns df", {
  r <- make_synthetic_rast(matrix(c(5,6,7), nrow=1))
  df <- to_csv(r, lat=29.5, lon=76.5)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("date","value") %in% names(df)))
})
