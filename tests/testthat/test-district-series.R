test_that("spatial mean over raster", {
  r <- make_synthetic_rast(matrix(c(10,10,10, 20,20,20), nrow=2, byrow=TRUE))
  m <- as.numeric(terra::global(r, "mean", na.rm=TRUE)[,1])
  expect_equal(unique(round(m,3)), 15)
})
