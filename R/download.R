#' Download and read IMD gridded data
#'
#' Downloads binary .grd files from IMD Pune and converts them to
#' terra SpatRaster objects. Single year returns a SpatRaster directly.
#' Multi-year returns a named list of SpatRasters (one per year) because
#' leap and non-leap years have different layer counts.
#'
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year (rain: 1901+, temp: 1951+).
#' @param end_yr End year.
#' @param file_dir Directory to save downloaded .grd files.
#' @param overwrite Re-download even if file exists? Default FALSE.
#' @return A SpatRaster (single year) or named list of SpatRasters (multi-year).
#' @export
get_data <- function(variable, start_yr, end_yr,
                     file_dir = ".", overwrite = FALSE) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (end_yr < start_yr)
          stop("end_yr must be >= start_yr")

     ep      <- imd_endpoints[[variable]]
     m       <- imd_meta[[variable]]
     years   <- seq(start_yr, end_yr)
     out_dir <- file.path(path.expand(file_dir), variable)
     dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

     rlist <- list()

     for (yr in years) {

          dest <- file.path(out_dir, paste0(yr, ".grd"))

          if (file.exists(dest) && !overwrite) {
               cat("Already exists, skipping:", dest, "\n")
          } else {

               cat("Downloading:", variable, yr, "... ")

               is_leap  <- (yr %% 4 == 0 & yr %% 100 != 0) | (yr %% 400 == 0)
               n_days   <- ifelse(is_leap, 366, 365)
               expected <- m$ncols * m$nrows * n_days * 4L

               form_data             <- list()
               form_data[[ep$field]] <- as.character(yr)

               resp <- tryCatch({
                    httr2::request(ep$url) |>
                         httr2::req_method("POST") |>
                         httr2::req_headers(
                              "User-Agent" = "Mozilla/5.0",
                              "Referer"    = ep$referer,
                              "Origin"     = "https://imdpune.gov.in"
                         ) |>
                         httr2::req_body_form(!!!form_data) |>
                         httr2::req_timeout(300) |>
                         httr2::req_perform()
               }, error = function(e) {
                    cat("FAILED -", conditionMessage(e), "\n"); NULL
               })

               if (is.null(resp)) next

               body <- httr2::resp_body_raw(resp)

               if (length(body) != expected) {
                    cat("FAILED - size mismatch:", length(body),
                        "vs", expected, "\n")
                    next
               }

               writeBin(body, dest)
               cat("(", round(length(body) / 1024^2, 1), "MB )\n")
          }

          arr   <- read_imd_binary(dest, variable, yr)
          r     <- array_to_raster(arr, variable, yr)
          rlist[[as.character(yr)]] <- r
     }

     if (length(rlist) == 0) stop("No data downloaded or found.")

     if (length(rlist) == 1) {
          cat("\nDone!\n")
          return(invisible(rlist[[1]]))
     } else {
          cat("Multi-year: returning named list of", length(rlist),
              "SpatRasters\n")
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
#' @export
open_data <- function(variable, start_yr, end_yr, file_dir = ".") {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (end_yr < start_yr)
          stop("end_yr must be >= start_yr")

     rlist <- list()

     for (yr in seq(start_yr, end_yr)) {
          fpath <- file.path(path.expand(file_dir), variable,
                             paste0(yr, ".grd"))
          if (!file.exists(fpath)) {
               warning("File not found, skipping: ", fpath); next
          }
          cat("Processing year", yr, "...\n")
          rlist[[as.character(yr)]] <-
               array_to_raster(read_imd_binary(fpath, variable, yr),
                               variable, yr)
     }

     if (length(rlist) == 0)
          stop("No files found. Check file_dir and filenames.")

     if (length(rlist) == 1) return(invisible(rlist[[1]]))

     cat("Multi-year: returning named list of", length(rlist),
         "SpatRasters\n")
     return(invisible(rlist))
}
