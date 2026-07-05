test_that("rainfall indices columns", {
  r <- make_synthetic_rast(matrix(c(0,3,5,1,10), nrow=1))
  idx <- compute_rainfall_indices(r, file_dir=tempdir())
  expect_true(all(c("year","cell","dr","total","cdd","cwd","rx1day") %in% names(idx)))
})
test_that("rainy days dr counts >=2.5mm", {
  r <- make_synthetic_rast(matrix(c(0,3,5,1,10), nrow=1))
  idx <- compute_rainfall_indices(r, file_dir=tempdir())
  expect_equal(idx$dr[1], 3)
})
test_that("total = sum of daily", {
  r <- make_synthetic_rast(matrix(c(0,3,5,1,10), nrow=1))
  idx <- compute_rainfall_indices(r, file_dir=tempdir())
  expect_equal(idx$total[1], 19)
})
test_that("rx1day = max day", {
  r <- make_synthetic_rast(matrix(c(0,3,5,1,10), nrow=1))
  idx <- compute_rainfall_indices(r, file_dir=tempdir())
  expect_equal(idx$rx1day[1], 10)
})
