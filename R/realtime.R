#' Download IMD real-time (provisional) daily gridded data
#'
#' Downloads provisional daily grids from IMD's real-time endpoints,
#' which are updated with roughly a one-day lag. Unlike get_data()
#' (quality-controlled yearly archive), real-time data is PROVISIONAL
#' and values may be revised later by IMD. Use get_data() for research
#' requiring the final quality-controlled dataset.
#'
#' Note: real-time rainfall is 0.25 degree (135 x 129), but real-time
#' temperature is 0.5 degree (61 x 61) -- different from the 1.0 degree
#' archive temperature returned by get_data().
#'
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_date Start date "YYYY-MM-DD".
#' @param end_date End date "YYYY-MM-DD". Defaults to start_date.
#' @param file_dir Directory to save downloaded .grd files.
#' @param overwrite Re-download existing files? Default FALSE.
#' @return Invisible SpatRaster with one layer per day.
#' @examples
#' \donttest{
#' # Yesterday's rainfall
#' y <- Sys.Date() - 1
#' r <- get_realtime("rain", as.character(y), file_dir = tempdir())
#'
#' # This month to date
#' first <- format(Sys.Date(), "%Y-%m-01")
#' r <- get_realtime("rain", first, as.character(Sys.Date() - 1),
#'                   file_dir = tempdir())
#' }
#' @export
get_realtime <- function(variable, start_date, end_date = NULL,
                         file_dir, overwrite = FALSE) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (is.null(end_date)) end_date <- start_date

     start_date <- as.Date(start_date)
     end_date   <- as.Date(end_date)
     if (end_date < start_date) stop("end_date must be >= start_date")
     if (end_date >= Sys.Date())
          warning("Real-time data has ~1 day lag; today/future may be unavailable.")

     cfg <- switch(variable,
                   rain = list(
                        url   = "https://imdpune.gov.in/cmpg/Realtimedata/Rainfall/rain.php",
                        field = "rain", prefix = "rain_ind0.25_",
                        ncols = 135, nrows = 129, na_val = -999,
                        xmin = 66.5, ymin = 6.5, res = 0.25, units = "mm/day"),
                   tmax = list(
                        url   = "https://imdpune.gov.in/cmpg/Realtimedata/max/max.php",
                        field = "max", prefix = "max",
                        ncols = 61, nrows = 61, na_val = 99.9,
                        xmin = 67.5, ymin = 7.5, res = 0.5, units = "deg_C"),
                   tmin = list(
                        url   = "https://imdpune.gov.in/cmpg/Realtimedata/min/min.php",
                        field = "min", prefix = "min",
                        ncols = 61, nrows = 61, na_val = 99.9,
                        xmin = 67.5, ymin = 7.5, res = 0.5, units = "deg_C")
     )

     out_dir <- file.path(path.expand(file_dir), paste0(variable, "_realtime"))
     dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

     days  <- seq(start_date, end_date, by = "day")
     rlist <- list()

     message(paste("Real-time", variable, ":",
                   length(days), "day(s).",
                   "NOTE: provisional data, may be revised."))

     for (day in days) {
          day <- as.Date(day, origin = "1970-01-01")

          fdate <- if (variable == "rain")
               format(day, "%y_%m_%d") else format(day, "%d%m%Y")
          dest  <- file.path(out_dir, paste0(cfg$prefix, fdate, ".grd"))

          if (file.exists(dest) && file.size(dest) >= 1024 && !overwrite) {
               message(paste("Exists, skipping:", basename(dest)))
          } else {
               message(paste("Downloading:", variable,
                             format(day, "%Y-%m-%d")))
               post_val <- format(day, "%d%m%Y")
               form     <- list()
               form[[cfg$field]] <- post_val

               resp <- tryCatch(
                    httr2::request(cfg$url) |>
                         httr2::req_method("POST") |>
                         httr2::req_headers(
                              "User-Agent" = "Mozilla/5.0",
                              "Origin"     = "https://imdpune.gov.in") |>
                         httr2::req_body_form(!!!form) |>
                         httr2::req_timeout(120) |>
                         httr2::req_perform(),
                    error = function(e) {
                         message(paste("FAILED:", conditionMessage(e)))
                         NULL
                    })

               if (is.null(resp)) next
               body <- httr2::resp_body_raw(resp)
               if (length(body) < 1) {
                    message(paste("No data for", format(day, "%Y-%m-%d")))
                    next
               }
               writeBin(body, dest)
          }

          r <- tryCatch(
               .read_realtime_grd(dest, cfg, day),
               error = function(e) {
                    message(paste("Read failed:", conditionMessage(e)))
                    NULL
               })
          if (!is.null(r)) rlist[[format(day, "%Y-%m-%d")]] <- r
     }

     if (length(rlist) == 0)
          stop("No real-time data downloaded. Dates may be unavailable.")

     if (length(rlist) == 1) {
          out <- rlist[[1]]
     } else {
          out <- terra::rast(rlist)
     }

     message(paste("Done!", terra::nlyr(out), "day(s) loaded."))
     return(invisible(out))
}

# ── Internal: read one real-time daily .grd ──────────────────
.read_realtime_grd <- function(filepath, cfg, day) {

     n_vals <- cfg$ncols * cfg$nrows
     con    <- file(filepath, "rb")
     vals   <- readBin(con, "numeric", n = n_vals + 5,
                       size = 4, endian = "little")
     close(con)

     if (length(vals) == n_vals + 1) vals <- vals[-1]
     if (length(vals) != n_vals)
          stop("size mismatch: got ", length(vals),
               " expected ", n_vals)

     vals[abs(vals - cfg$na_val) < 0.01] <- NA

     r <- terra::rast(
          nrows = cfg$nrows, ncols = cfg$ncols,
          xmin  = cfg$xmin - cfg$res / 2,
          xmax  = cfg$xmin + cfg$ncols * cfg$res - cfg$res / 2,
          ymin  = cfg$ymin - cfg$res / 2,
          ymax  = cfg$ymin + cfg$nrows * cfg$res - cfg$res / 2,
          crs   = "EPSG:4326")

     mat <- matrix(vals, nrow = cfg$nrows, ncol = cfg$ncols, byrow = TRUE)
     mat <- mat[cfg$nrows:1, ]
     terra::values(r) <- as.vector(t(mat))
     names(r)       <- format(day, "%Y-%m-%d")
     terra::time(r) <- day
     r
}
