test_that("get_data downloads one year", {
  skip_on_cran(); skip_if_offline()
  r <- get_data("rain", 2020, 2020, tempdir())
  expect_s4_class(r, "SpatRaster"); expect_equal(terra::nlyr(r), 366)
})
test_that("get_realtime one layer", {
  skip_on_cran(); skip_if_offline()
  d <- as.character(Sys.Date() - 3)
  r <- get_realtime("rain", d, file_dir=tempdir())
  expect_s4_class(r, "SpatRaster"); expect_gte(terra::nlyr(r), 1)
})
