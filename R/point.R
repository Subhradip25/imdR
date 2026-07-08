#' Extract daily time series for a single variable at a point
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param save_csv Save output as CSV? Default TRUE.
#' @return Invisible data frame with columns date, lat, lon, variable.
#' @examples
#' \donttest{
#' # Extract daily rainfall at Panaji, Goa
#' df <- get_point(lat = 15.5, lon = 73.8,
#'                 variable = "rain",
#'                 start_yr = 2020, end_yr = 2020,
#'                 file_dir = tempdir())
#' head(df)
#'
#' # Extract temperature
#' df_tmax <- get_point(lat = 15.5, lon = 73.8,
#'                      variable = "tmax",
#'                      start_yr = 2020, end_yr = 2020,
#'                      file_dir = tempdir())
#' }
#' @export
get_point <- function(lat, lon, variable, start_yr, end_yr,
                      file_dir, save_csv = TRUE) {

     imd_raster <- get_data(variable, start_yr, end_yr, file_dir)

     # Extract year by year to avoid stacking large rasters into memory.
     # Stacking 10+ years at once causes memory errors on Windows.
     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          message("Extracting point data year by year...")
          df_list <- lapply(names(imd_raster), function(yr) {
               to_csv(imd_raster[[yr]], lat, lon)
          })
          df <- do.call(rbind, df_list)
     } else {
          df <- to_csv(imd_raster, lat, lon)
     }

     colnames(df)[2] <- variable
     df$lat          <- lat
     df$lon          <- lon
     df              <- df[, c("date", "lat", "lon", variable)]
     df              <- df[order(df$date), ]
     rownames(df)    <- NULL

     message(paste("Total rows:", nrow(df),
                   "| Date range:",
                   as.character(min(df$date)),
                   "to",
                   as.character(max(df$date))))

     if (save_csv) {
          fname <- file.path(
               path.expand(file_dir),
               paste0(variable, "_", lat, "N_", lon, "E_",
                      start_yr, "_", end_yr, ".csv"))
          write.csv(df, fname, row.names = FALSE)
          message(paste("Saved:", fname))
     }

     return(invisible(df))
}

#' Extract daily time series for all variables at a point
#'
#' Downloads or reads rain, tmax, and tmin at a location and merges
#' them into a single data frame that also includes diurnal temperature
#' range (DTR). Extraction is done year by year to avoid memory issues
#' with long time series on Windows.
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param save_csv Save merged output as CSV? Default TRUE.
#' @return Invisible data frame with columns date, lat, lon,
#'   rain, tmax, tmin, dtr.
#' @examples
#' \donttest{
#' # Extract rain, tmax, tmin and DTR at Panaji, Goa
#' df <- get_point_all(lat = 15.5, lon = 73.8,
#'                     start_yr = 2020, end_yr = 2020,
#'                     file_dir = tempdir())
#' head(df)
#'
#' # Long time series -- works on Windows without memory errors
#' df <- get_point_all(lat = 15.5, lon = 73.8,
#'                     start_yr = 1985, end_yr = 2020,
#'                     file_dir = tempdir())
#' nrow(df)
#' }
#' @export
get_point_all <- function(lat, lon, start_yr, end_yr,
                          file_dir, save_csv = TRUE) {

     message(paste("=== Extracting all variables at lat =",
                   lat, ", lon =", lon, "==="))

     message("--- Rainfall ---")
     df_rain <- get_point(lat, lon, "rain", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     message("--- Max Temperature ---")
     df_tmax <- get_point(lat, lon, "tmax", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     message("--- Min Temperature ---")
     df_tmin <- get_point(lat, lon, "tmin", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     df <- merge(df_rain,
                 df_tmax[, c("date", "tmax")],
                 by = "date", all = TRUE)
     df <- merge(df,
                 df_tmin[, c("date", "tmin")],
                 by = "date", all = TRUE)
     df <- df[order(df$date),
              c("date", "lat", "lon", "rain", "tmax", "tmin")]
     df$dtr       <- round(df$tmax - df$tmin, 3)
     rownames(df) <- NULL

     message("--- Merged summary ---")
     message(paste("Rows:", nrow(df)))
     message(paste("Date range:",
                   as.character(min(df$date, na.rm = TRUE)),
                   "to",
                   as.character(max(df$date, na.rm = TRUE))))
     message(paste("Columns:", paste(names(df), collapse = ", ")))
     message(paste("Non-NA rain days:", sum(!is.na(df$rain))))
     message(paste("Non-NA tmax days:", sum(!is.na(df$tmax))))
     message(paste("Non-NA tmin days:", sum(!is.na(df$tmin))))

     if (save_csv) {
          fname <- file.path(
               path.expand(file_dir),
               paste0("imd_all_", lat, "N_", lon, "E_",
                      start_yr, "_", end_yr, ".csv"))
          write.csv(df, fname, row.names = FALSE)
          message(paste("Saved:", fname))
     }

     return(invisible(df))
}

#' Read all years once; return stacked SpatRaster + dates
#' @keywords internal
#' @noRd
.read_stack <- function(variable, start_yr, end_yr, file_dir,
                        download = TRUE) {
     lst <- if (download) {
          get_data(variable, start_yr, end_yr, file_dir)
     } else {
          open_data(variable, start_yr, end_yr, file_dir)
     }
     if (is.list(lst) && !inherits(lst, "SpatRaster")) {
          list(r     = terra::rast(unname(lst)),
               dates = do.call("c", lapply(lst, terra::time)))
     } else {
          list(r = lst, dates = terra::time(lst))
     }
}

#' Extract many points in one pass with cell deduplication
#' @keywords internal
#' @noRd
.extract_dedup <- function(r, points, dedup = TRUE) {
     xy <- as.matrix(points[, c("lon", "lat")])
     if (dedup) {
          cells <- terra::cellFromXY(r, xy)
          if (anyNA(cells))
               stop("One or more coordinates fall outside IMD extent.")
          uidx <- !duplicated(cells)
          vals <- as.matrix(r[cells[uidx]])
          vals[match(cells, cells[uidx]), , drop = FALSE]
     } else {
          v <- terra::vect(as.data.frame(xy),
                           geom = c("lon", "lat"), crs = "EPSG:4326")
          as.matrix(terra::extract(r, v, ID = FALSE))
     }
}

#' Extract daily time series for one variable at many points
#'
#' Reads the raster once and extracts all locations in a single pass
#' with grid-cell deduplication. Far faster than looping get_point()
#' for many coordinates.
#'
#' @param points Data frame with columns lat, lon, optionally name.
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param format "long" or "wide". Default "long".
#' @param download Use get_data() if TRUE, open_data() if FALSE. Default TRUE.
#' @param dedup Collapse points sharing a grid cell? Default TRUE.
#' @param save_csv Save output as CSV? Default TRUE.
#' @return Invisible data frame in the requested format.
#' @examples
#' \donttest{
#' pts <- data.frame(name = c("Panaji", "Margao"),
#'                   lat = c(15.49, 15.28), lon = c(73.83, 73.99))
#' df  <- get_points(pts, "rain", 2020, 2020,
#'                   file_dir = tempdir(), format = "wide")
#' }
#' @export
get_points <- function(points, variable, start_yr, end_yr, file_dir,
                       format = "long", download = TRUE,
                       dedup = TRUE, save_csv = TRUE) {

     if (!all(c("lat", "lon") %in% names(points)))
          stop("'points' must have columns 'lat' and 'lon'.")
     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'.")
     format <- match.arg(format, c("long", "wide"))
     if (is.null(points$name))
          points$name <- paste0("P", seq_len(nrow(points)))

     st    <- .read_stack(variable, start_yr, end_yr, file_dir, download)
     mat   <- .extract_dedup(st$r, points, dedup)
     dates <- st$dates

     if (nrow(mat) != nrow(points) || ncol(mat) != length(dates))
          stop("Extraction dimension mismatch; check coordinates/dates.")

     message(paste("Extracted", nrow(points), "locations x",
                   ncol(mat), "days |",
                   as.character(min(dates)), "to",
                   as.character(max(dates))))

     if (format == "wide") {
          out <- as.data.frame(t(mat))
          colnames(out) <- points$name
          out <- cbind(date = dates, out)
     } else {
          out <- data.frame(
               date = rep(dates,       each  = nrow(points)),
               name = rep(points$name, times = ncol(mat)),
               lat  = rep(points$lat,  times = ncol(mat)),
               lon  = rep(points$lon,  times = ncol(mat)),
               value = as.vector(mat))
          colnames(out)[5] <- variable
          out <- out[order(out$name, out$date), ]
          rownames(out) <- NULL
     }

     if (save_csv) {
          fname <- file.path(path.expand(file_dir),
                             paste0(variable, "_", nrow(points),
                                    "pts_", start_yr, "_", end_yr,
                                    "_", format, ".csv"))
          write.csv(out, fname, row.names = FALSE)
          message(paste("Saved:", fname))
     }
     return(invisible(out))
}

#' Extract all variables (rain, tmax, tmin, DTR) at many points
#'
#' Reads each variable's raster once, extracts all points, merges to a
#' single long data frame with diurnal temperature range.
#'
#' @param points Data frame with columns lat, lon, optionally name.
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param download Passed to get_points(). Default TRUE.
#' @param dedup Passed to get_points(). Default TRUE.
#' @param save_csv Save merged CSV? Default TRUE.
#' @return Invisible data frame: date, name, lat, lon, rain, tmax, tmin, dtr.
#' @examples
#' \donttest{
#' pts <- data.frame(name = c("Panaji", "Margao"),
#'                   lat = c(15.49, 15.28), lon = c(73.83, 73.99))
#' df  <- get_points_all(pts, 2020, 2020, file_dir = tempdir())
#' }
#' @export
get_points_all <- function(points, start_yr, end_yr, file_dir,
                           download = TRUE, dedup = TRUE,
                           save_csv = TRUE) {

     r <- get_points(points, "rain", start_yr, end_yr, file_dir,
                     format = "long", download = download,
                     dedup = dedup, save_csv = FALSE)
     x <- get_points(points, "tmax", start_yr, end_yr, file_dir,
                     format = "long", download = download,
                     dedup = dedup, save_csv = FALSE)
     n <- get_points(points, "tmin", start_yr, end_yr, file_dir,
                     format = "long", download = download,
                     dedup = dedup, save_csv = FALSE)

     df <- merge(r, x[, c("date", "name", "tmax")],
                 by = c("date", "name"), all = TRUE)
     df <- merge(df, n[, c("date", "name", "tmin")],
                 by = c("date", "name"), all = TRUE)
     df$dtr <- round(df$tmax - df$tmin, 3)
     df <- df[order(df$name, df$date), ]
     rownames(df) <- NULL

     if (save_csv) {
          fname <- file.path(path.expand(file_dir),
                             paste0("imd_all_", nrow(points),
                                    "pts_", start_yr, "_", end_yr,
                                    ".csv"))
          write.csv(df, fname, row.names = FALSE)
          message(paste("Saved:", fname))
     }
     return(invisible(df))
}
