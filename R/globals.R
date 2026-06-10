#' @importFrom utils write.csv combn txtProgressBar setTxtProgressBar
#' @importFrom stats quantile median aggregate
#' @importFrom rlang .data
#' @importFrom sf st_as_sf
NULL

utils::globalVariables(c(
     "india_states",
     "india_districts",
     "year",
     "value",
     "trend",
     "smooth"
))

# ── Internal: convert sf boundary to SpatVector safely ────────
# Explicitly uses sf::st_as_sf() to ensure sf geometry is valid
# before terra::vect() attempts the conversion.
.boundary_to_vect <- function(boundary) {
     terra::vect(sf::st_as_sf(boundary))
}

# ── Internal: SpatRaster to long-format data frame ────────────
# Converts a masked SpatRaster to a long-format data frame with
# columns: date, lat, lon, variable. Used by extract_by_boundary()
# and get_bbox() for CSV output.
.raster_to_long <- function(r_masked, variable) {

     dates  <- as.Date(names(r_masked))
     n_cell <- terra::ncell(r_masked)

     coords        <- as.data.frame(
          terra::xyFromCell(r_masked, 1:n_cell)
     )
     names(coords) <- c("lon", "lat")

     vals  <- as.matrix(r_masked, wide = FALSE)
     valid <- which(rowSums(!is.na(vals)) > 0)

     message(paste("Valid land cells :", length(valid)))
     message(paste("Total rows in CSV:", length(valid) * length(dates)))
     message("Building long-format table...")

     df_list <- lapply(valid, function(i) {
          data.frame(
               date  = dates,
               lat   = round(coords$lat[i], 4),
               lon   = round(coords$lon[i], 4),
               value = round(vals[i, ], 4),
               stringsAsFactors = FALSE
          )
     })

     df           <- do.call(rbind, df_list)
     names(df)[4] <- variable
     df           <- df[order(df$date, df$lat, df$lon), ]
     rownames(df) <- NULL
     df
}
