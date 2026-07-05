test_that("array_to_raster correct dims", {
  m <- imdR:::imd_meta$rain
  arr <- array(0, dim=c(m$ncols, m$nrows, 365))  # 2021 non-leap
  r <- imdR:::array_to_raster(arr, "rain", 2021)
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::ncol(r), m$ncols)
  expect_equal(terra::nrow(r), m$nrows)
  expect_equal(terra::nlyr(r), 365)
})
test_that("array_to_raster date names", {
  m <- imdR:::imd_meta$tmax
  arr <- array(30, dim=c(m$ncols, m$nrows, 366))  # 2020 leap
  r <- imdR:::array_to_raster(arr, "tmax", 2020)
  expect_equal(names(r)[1], "2020-01-01")
  expect_equal(terra::nlyr(r), 366)
})
