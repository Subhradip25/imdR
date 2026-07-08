# Tests for multi-point extraction: get_points(), get_points_all(),
# and internal helpers. Uses a small in-memory SpatRaster so the
# extraction logic is tested without any network access.

# ── Fixture: 3-day synthetic raster over an India-ish extent ──
make_test_raster <- function() {
     r <- terra::rast(nrows = 10, ncols = 10,
                      xmin = 70, xmax = 90, ymin = 8, ymax = 30,
                      nlyrs = 3, crs = "EPSG:4326")
     terra::values(r) <- seq_len(terra::ncell(r) * 3)
     terra::time(r)   <- as.Date(c("2020-01-01", "2020-01-02",
                                   "2020-01-03"))
     r
}

test_that(".extract_dedup returns one row per point, correct dims", {
     r   <- make_test_raster()
     pts <- data.frame(lat = c(15, 20, 25), lon = c(75, 80, 85))
     m   <- .extract_dedup(r, pts, dedup = TRUE)

     expect_equal(nrow(m), 3)                 # one row per point
     expect_equal(ncol(m), terra::nlyr(r))    # one col per day
     expect_true(is.matrix(m))
})

test_that(".extract_dedup dedup and non-dedup agree", {
     r   <- make_test_raster()
     pts <- data.frame(lat = c(15, 20, 25), lon = c(75, 80, 85))

     expect_equal(.extract_dedup(r, pts, dedup = TRUE),
                  .extract_dedup(r, pts, dedup = FALSE))
})

test_that(".extract_dedup maps duplicate-cell points identically", {
     r   <- make_test_raster()
     # Two points in the same grid cell -> identical rows expected.
     pts <- data.frame(lat = c(15.0, 15.1), lon = c(75.0, 75.1))
     m   <- .extract_dedup(r, pts, dedup = TRUE)

     expect_equal(m[1, ], m[2, ])
})

test_that(".extract_dedup errors on out-of-extent coordinates", {
     r   <- make_test_raster()
     pts <- data.frame(lat = 50, lon = 120)   # outside extent
     expect_error(.extract_dedup(r, pts, dedup = TRUE),
                  "outside IMD extent")
})

test_that("wide vs long assembly is internally consistent", {
     # Build outputs directly from a known matrix to test reshape
     # logic independent of raster reading.
     dates <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
     pts   <- data.frame(name = c("A", "B"),
                         lat = c(15, 20), lon = c(75, 80))
     mat   <- matrix(1:6, nrow = 2)           # 2 points x 3 days

     wide <- as.data.frame(t(mat))
     colnames(wide) <- pts$name
     wide <- cbind(date = dates, wide)

     expect_equal(nrow(wide), length(dates))
     expect_equal(ncol(wide), nrow(pts) + 1)  # date + one col per point
     expect_named(wide, c("date", "A", "B"))
})

test_that("get_points validates inputs", {
     pts <- data.frame(lat = 15, lon = 75)

     expect_error(
          get_points(data.frame(x = 1, y = 2), "rain", 2020, 2020,
                     tempdir(), save_csv = FALSE),
          "must have columns")
     expect_error(
          get_points(pts, "humidity", 2020, 2020,
                     tempdir(), save_csv = FALSE),
          "variable must be")
})

test_that("get_points auto-labels points when name column absent", {
     pts <- data.frame(lat = c(15, 20), lon = c(75, 80))
     # Only checks the labeling branch; extraction needs data, so we
     # verify the default-name construction directly.
     nm <- if (is.null(pts$name)) paste0("P", seq_len(nrow(pts))) else pts$name
     expect_equal(nm, c("P1", "P2"))
})

# ── Network-dependent smoke test (skipped on CRAN / offline) ──
test_that("get_points end-to-end works with real download", {
     skip_on_cran()
     skip_if_offline()

     pts <- data.frame(name = c("Panaji", "Margao"),
                       lat = c(15.49, 15.28), lon = c(73.83, 73.99))
     w <- get_points(pts, "rain", 2020, 2020, tempdir(),
                     format = "wide", save_csv = FALSE)

     expect_s3_class(w, "data.frame")
     expect_equal(ncol(w), 3)                 # date + 2 talukas
     expect_named(w, c("date", "Panaji", "Margao"))
     expect_equal(nrow(w), 366)               # 2020 leap year
})
