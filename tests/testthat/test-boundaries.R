test_that("list_states returns 36 states", {
  s <- list_states(); expect_type(s, "character"); expect_equal(length(s), 36)
})
test_that("list_districts returns known Tripura districts", {
  d <- list_districts("Tripura")
  expect_true("Dhalai" %in% d); expect_true("North Tripura" %in% d)
})
test_that("get_boundary returns sf for valid state", {
  expect_s3_class(get_boundary("state","Goa"), "sf")
})
test_that("get_boundary errors on bad name", {
  expect_error(get_boundary("state","Atlantis"))
})
test_that("get_boundary works at district level", {
  expect_s3_class(get_boundary("district","Dhalai"), "sf")
})
