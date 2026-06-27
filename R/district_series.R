#' Extract a district or state spatial-mean daily time series
#'
#' Returns a tidy daily time series of the spatial mean across all grid
#' cells within a named state or district. This is the correct way to
#' summarise gridded data over an area: values are averaged across space
#' (cells sample the same region), giving one value per day. To obtain a
#' seasonal or annual total for rainfall, sum the returned daily values.
#'
#' Works with both the quality-controlled archive (default) and the
#' provisional real-time daily data (realtime = TRUE).
#'
#' Note on resolution: archive temperature is 1.0 degree (~111 km cells),
#' so small states or districts may contain no grid-cell centre. In that
#' case the result is all NA; set touches = TRUE to capture every cell the
#' boundary overlaps.
#'
#' @param variable One of "rain", "tmax", "tmin".
#' @param name State or district name (partial match allowed).
#' @param start Start of period. For archive data, a year (e.g. 2018) or
#'   date string. For real-time data, a date string "YYYY-MM-DD".
#' @param end End of period. Same format as start.
#' @param file_dir Directory for downloaded data and output.
#' @param level "district" (default) or "state".
#' @param touches Logical. If TRUE, include every grid cell the boundary
#'   overlaps, not only those whose centre falls inside. Useful for small
#'   regions and for the coarse 1.0 degree temperature grid. Default FALSE.
#' @param save_csv Save the series as CSV? Default TRUE.
#' @param plot Produce a publication-quality line plot? Default FALSE.
#' @param realtime Use provisional real-time daily data instead of the
#'   archive? Default FALSE. When TRUE, start and end must be date strings
#'   "YYYY-MM-DD".
#' @return Invisible tidy data frame with columns date, region, and the
#'   variable (daily spatial mean).
#' @examples
#' \donttest{
#' # Archive: Karnal district rainfall, multi-year daily series
#' df <- get_district_series("rain", "Karnal",
#'                           start = 2018, end = 2020,
#'                           file_dir = tempdir())
#' head(df)
#'
#' # Seasonal total for rainfall = sum of daily means
#' sum(df$rain, na.rm = TRUE)
#'
#' # State-level temperature series with a plot.
#' # Goa is smaller than a 1.0 degree temperature cell, so touches = TRUE
#' # is needed to capture overlapping cells.
#' get_district_series("tmax", "Goa",
#'                     start = 2020, end = 2020,
#'                     level = "state",
#'                     file_dir = tempdir(),
#'                     touches = TRUE,
#'                     plot = TRUE)
#'
#' # Real-time: North Tripura rainfall, this month to date
#' get_district_series("rain", "North Tripura",
#'                     start = "2026-06-01", end = "2026-06-25",
#'                     file_dir = tempdir(),
#'                     realtime = TRUE, touches = TRUE)
#' }
#' @export
get_district_series <- function(variable, name, start, end,
                                file_dir,
                                level    = "district",
                                touches  = FALSE,
                                save_csv = TRUE,
                                plot     = FALSE,
                                realtime = FALSE) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (!level %in% c("state", "district"))
          stop("level must be 'state' or 'district'")

     # Boundary (resolve once)
     boundary <- get_boundary(level, name)
     bv       <- .boundary_to_vect(boundary)

     # -- Acquire data --------------------------------------------
     if (realtime) {
          imd_raster <- get_realtime(variable,
                                     start_date = as.character(start),
                                     end_date   = as.character(end),
                                     file_dir   = file_dir)
          rast_list <- list(imd_raster)  # single SpatRaster
     } else {
          imd_raster <- get_data(variable, as.integer(start),
                                 as.integer(end), file_dir)
          # Normalise to a list of yearly SpatRasters
          if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
               rast_list <- imd_raster
          } else {
               rast_list <- list(imd_raster)
          }
     }

     message(paste("Computing daily spatial mean for", name,
                   "(", level, ")..."))

     # -- Spatial-mean per day, processed year by year -----------
     daily_list <- lapply(rast_list, function(r) {
          masked <- terra::mask(
               terra::crop(r, bv), bv, touches = touches)
          data.frame(
               date  = as.Date(names(masked)),
               value = round(as.numeric(
                    terra::global(masked, "mean", na.rm = TRUE)[, 1]), 3),
               stringsAsFactors = FALSE
          )
     })

     df <- do.call(rbind, daily_list)
     df <- df[order(df$date), ]
     rownames(df) <- NULL

     # Trim to requested range when dates were given
     if (realtime || grepl("-", as.character(start))) {
          df <- df[df$date >= as.Date(as.character(start)) &
                        df$date <= as.Date(as.character(end)), ]
     }

     # Tidy columns
     names(df)[2] <- variable
     df$region    <- name
     df <- df[, c("date", "region", variable)]
     rownames(df) <- NULL

     n_na <- sum(is.na(df[[variable]]))
     message(paste("Done!", nrow(df), "days |",
                   "non-NA:", nrow(df) - n_na))

     # Warn when nothing was captured (region smaller than grid cell)
     if (n_na == nrow(df)) {
          warning("All values are NA. The region may be smaller than the ",
                  "grid cell size (temperature is 1.0 degree, ~111 km). ",
                  "Try touches = TRUE to capture overlapping cells.")
     }

     # -- Save CSV ------------------------------------------------
     if (save_csv) {
          clean <- gsub("[^A-Za-z0-9]", "_", name)
          yr1   <- format(min(df$date), "%Y")
          yr2   <- format(max(df$date), "%Y")
          tag   <- if (yr1 == yr2) yr1 else paste0(yr1, "_", yr2)
          fname <- file.path(
               path.expand(file_dir),
               paste0(variable, "_", clean, "_series_", tag, ".csv"))
          write.csv(df, fname, row.names = FALSE)
          message(paste("Saved:", fname))
     }

     # -- Optional publication-quality plot -----------------------
     if (plot) {
          p <- plot_timeseries(
               df, variable = variable,
               title = paste(toupper(variable), "-", name,
                             "(", level, ")"))
          print(p)
     }

     return(invisible(df))
}
