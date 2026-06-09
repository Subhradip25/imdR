#' Read an IMD binary .grd file
#'
#' @param filepath Path to the .grd file.
#' @param variable One of "rain", "tmax", "tmin".
#' @param year Integer year.
#' @return A numeric 3-dimensional array of dimensions ncols x nrows x ndays.
#' @keywords internal
read_imd_binary <- function(filepath, variable, year) {

     m <- imd_meta[[variable]]
     if (is.null(m)) stop("variable must be 'rain', 'tmax', or 'tmin'")

     is_leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
     n_days  <- ifelse(is_leap, 366, 365)
     n_vals  <- m$ncols * m$nrows * n_days

     message(paste("Reading", variable, "for year", year,
         "| Grid:", m$ncols, "x", m$nrows, "| Days:", n_days, "\n"))

     con      <- file(filepath, "rb")
     raw_vals <- readBin(con, what = "numeric", n = n_vals,
                         size = 4, endian = "little")
     close(con)

     arr <- array(raw_vals, dim = c(m$ncols, m$nrows, n_days))
     arr[abs(arr - m$na_val) < 0.01] <- NA

     message("Value range (non-NA):",
         round(min(arr, na.rm = TRUE), 2), "to",
         round(max(arr, na.rm = TRUE), 2), m$units, "\n")

     return(arr)
}

#' Convert a 3D IMD array to a terra SpatRaster
#'
#' @param arr Numeric array from read_imd_binary().
#' @param variable One of "rain", "tmax", "tmin".
#' @param year Integer year.
#' @return A terra SpatRaster with named layers and time dimension.
#' @keywords internal
array_to_raster <- function(arr, variable, year) {

     m <- imd_meta[[variable]]

     is_leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
     n_days  <- ifelse(is_leap, 366, 365)

     dates <- seq.Date(as.Date(paste0(year, "-01-01")),
                       by = "day", length.out = n_days)

     r <- terra::rast(
          nrows = m$nrows, ncols = m$ncols, nlyr = n_days,
          xmin  = m$xmin - m$res / 2,
          xmax  = m$xmin + m$ncols * m$res - m$res / 2,
          ymin  = m$ymin - m$res / 2,
          ymax  = m$ymin + m$nrows * m$res - m$res / 2,
          crs   = "EPSG:4326"
     )

     mat <- matrix(NA_real_, nrow = m$nrows * m$ncols, ncol = n_days)

     for (i in seq_len(n_days)) {
          slice      <- arr[, , i]
          slice_t    <- t(slice)
          slice_flip <- slice_t[m$nrows:1, ]
          mat[, i]   <- as.vector(t(slice_flip))
     }

     terra::values(r) <- mat
     names(r)         <- as.character(dates)
     terra::time(r)   <- dates

     return(r)
}
