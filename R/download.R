#' Internal: download one year's .grd (no rasterizing).
#' Returns dest path on success, NULL on failure.
#' @keywords internal
#' @noRd
.download_year <- function(variable, yr, out_dir, overwrite = FALSE) {
     ep   <- imd_endpoints[[variable]]
     m    <- imd_meta[[variable]]
     dest <- file.path(out_dir, paste0(yr, ".grd"))

     if (file.exists(dest) && !overwrite) return(dest)

     is_leap  <- (yr %% 4 == 0 & yr %% 100 != 0) | (yr %% 400 == 0)
     n_days   <- ifelse(is_leap, 366, 365)
     expected <- m$ncols * m$nrows * n_days * 4L

     form_data             <- list()
     form_data[[ep$field]] <- as.character(yr)

     resp <- tryCatch(
          httr2::request(ep$url) |>
               httr2::req_method("POST") |>
               httr2::req_headers(
                    "User-Agent" = "Mozilla/5.0",
                    "Referer"    = ep$referer,
                    "Origin"     = "https://imdpune.gov.in") |>
               httr2::req_body_form(!!!form_data) |>
               httr2::req_timeout(300) |>
               httr2::req_retry(max_tries = 3) |>
               httr2::req_perform(),
          error = function(e) NULL)
     if (is.null(resp)) return(NULL)

     body <- httr2::resp_body_raw(resp)
     if (length(body) != expected) return(NULL)

     writeBin(body, dest)
     dest
}

#' Download and read IMD gridded data
#'
#' Downloads binary .grd files from IMD Pune and converts them to
#' terra SpatRaster objects. Single year returns a SpatRaster directly.
#' Multi-year returns a named list of SpatRasters (one per year) because
#' leap and non-leap years have different layer counts.
#'
#' Set \code{parallel = TRUE} to download multiple years concurrently
#' (requires the \pkg{future} and \pkg{future.apply} packages). Only the
#' network download is parallelized; reading/rasterizing stays sequential
#' because terra pointers are not safe across processes.
#'
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year (rain: 1901+, temp: 1951+).
#' @param end_yr End year.
#' @param file_dir Directory to save downloaded .grd files.
#' @param overwrite Re-download even if file exists? Default FALSE.
#' @param parallel Download years in parallel? Default FALSE.
#' @param workers Number of parallel workers when parallel = TRUE. Default 4.
#' @return A SpatRaster (single year) or named list of SpatRasters (multi-year).
#' @examples
#' \donttest{
#' # Download single year rainfall
#' rain2020 <- get_data("rain", 2020, 2020, tempdir())
#'
#' # Download multiple years (returns named list)
#' rain_3yr <- get_data("rain", 2018, 2020, tempdir())
#'
#' # Parallel download of a long range
#' rain_hist <- get_data("rain", 2000, 2020, tempdir(),
#'                       parallel = TRUE, workers = 4)
#' }
#' @export
get_data <- function(variable, start_yr, end_yr, file_dir,
                     overwrite = FALSE, parallel = FALSE, workers = 4) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (end_yr < start_yr)
          stop("end_yr must be >= start_yr")

     years   <- seq(start_yr, end_yr)
     out_dir <- file.path(path.expand(file_dir), variable)
     dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

     # --- download phase ---
     use_par <- parallel && length(years) > 1 &&
          requireNamespace("future",        quietly = TRUE) &&
          requireNamespace("future.apply",  quietly = TRUE)

     if (parallel && !use_par && length(years) > 1)
          message("Packages 'future'/'future.apply' not available; ",
                  "falling back to sequential download.")

     if (use_par) {
          oplan <- future::plan(future::multisession, workers = workers)
          on.exit(future::plan(oplan), add = TRUE)
          message(paste("Parallel download:", length(years),
                        "years on", workers, "workers ..."))
          paths <- future.apply::future_lapply(
               years,
               function(y) .download_year(variable, y, out_dir, overwrite),
               future.seed = TRUE)
     } else {
          paths <- lapply(years, function(y) {
               message(paste("Downloading:", variable, y, "..."))
               .download_year(variable, y, out_dir, overwrite)
          })
     }

     # --- read phase (sequential) ---
     rlist <- list()
     for (i in seq_along(years)) {
          if (is.null(paths[[i]])) {
               message(paste("FAILED/skipped:", years[i]))
               next
          }
          message(paste("Reading", variable, "for year", years[i], "..."))
          rlist[[as.character(years[i])]] <-
               array_to_raster(
                    read_imd_binary(paths[[i]], variable, years[i]),
                    variable, years[i])
     }

     if (length(rlist) == 0) stop("No data downloaded or found.")

     if (length(rlist) == 1) {
          message("Done!")
          return(invisible(rlist[[1]]))
     } else {
          message(paste("Multi-year: returning named list of",
                        length(rlist), "SpatRasters"))
          return(invisible(rlist))
     }
}

#' Read cached IMD .grd files from disk
#'
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory containing the variable sub-folder.
#' @return A SpatRaster (single year) or named list (multi-year).
#' @examples
#' \donttest{
#' rain2020 <- open_data("rain", 2020, 2020, tempdir())
#' rain_3yr <- open_data("rain", 2018, 2020, tempdir())
#' }
#' @export
open_data <- function(variable, start_yr, end_yr, file_dir) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (end_yr < start_yr)
          stop("end_yr must be >= start_yr")

     rlist <- list()

     for (yr in seq(start_yr, end_yr)) {
          fpath <- file.path(path.expand(file_dir), variable,
                             paste0(yr, ".grd"))
          if (!file.exists(fpath)) {
               warning("File not found, skipping: ", fpath)
               next
          }
          message(paste("Processing year", yr, "..."))
          rlist[[as.character(yr)]] <-
               array_to_raster(read_imd_binary(fpath, variable, yr),
                               variable, yr)
     }

     if (length(rlist) == 0)
          stop("No files found. Check file_dir and filenames.")

     if (length(rlist) == 1) return(invisible(rlist[[1]]))

     message(paste("Multi-year: returning named list of",
                   length(rlist), "SpatRasters"))
     return(invisible(rlist))
}
