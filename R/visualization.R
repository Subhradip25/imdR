#' Plot a single day of IMD gridded data
#'
#' Publication-quality map with SOI boundaries. Supports full-India,
#' state-level, and district-level zoom.
#'
#' @param imd_raster A SpatRaster or named list from get_data().
#' @param date Date to plot (must match a layer name).
#' @param variable One of "rain", "tmax", "tmin".
#' @param level NULL, "state", or "district" for zoom.
#' @param name State or district name for zoom.
#' @param title Custom title. Auto-generated if NULL.
#' @param save_path File path to save PNG/PDF. NULL = no save.
#' @param width Plot width in inches. Default 7.
#' @param height Plot height in inches. Default 8.
#' @return Invisible ggplot2 object.
#' @examples
#' \donttest{
#' r <- get_data("rain", 2020, 2020, tempdir())
#'
#' # Full India map
#' plot_imd(r, "2020-06-28", "rain")
#'
#' # Zoom to Kerala
#' plot_imd(r, "2020-06-28", "rain",
#'          level = "state", name = "Kerala")
#'
#' # Zoom to North Goa district
#' plot_imd(r, "2020-06-28", "rain",
#'          level = "district", name = "North Goa")
#'
#' # Save to file
#' plot_imd(r, "2020-06-28", "rain",
#'          save_path = file.path(tempdir(), "rain_20200628.png"))
#' }
#' @export
plot_imd <- function(imd_raster, date,
                     variable  = "rain",
                     level     = NULL,
                     name      = NULL,
                     title     = NULL,
                     save_path = NULL,
                     width     = 7,
                     height    = 8) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")

     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          yr <- format(as.Date(as.character(date)), "%Y")
          if (!yr %in% names(imd_raster))
               stop("Year '", yr, "' not found in multi-year list.")
          imd_raster <- imd_raster[[yr]]
     }

     date <- as.character(date)
     if (!date %in% names(imd_raster))
          stop("Date '", date, "' not found in raster layers.")

     r_day   <- imd_raster[[date]]
     zoom_sf <- NULL

     if (!is.null(level) && !is.null(name)) {
          boundary <- get_boundary(level, name)
          bv       <- .boundary_to_vect(boundary)
          r_day    <- terra::mask(terra::crop(r_day, bv), bv)
          zoom_sf  <- boundary
     }

     if (is.null(title)) {
          var_label <- switch(variable,
                              rain = "Daily Rainfall",
                              tmax = "Daily Maximum Temperature",
                              tmin = "Daily Minimum Temperature")
          region_label <- if (!is.null(name)) paste("-", name) else ""
          title <- paste("IMD", var_label, region_label, "-", date)
     }

     pal <- imd_palette[[variable]]

     if (!is.null(zoom_sf)) {
          bb   <- sf::st_bbox(zoom_sf)
          pad  <- 0.5
          xlim <- c(bb["xmin"] - pad, bb["xmax"] + pad)
          ylim <- c(bb["ymin"] - pad, bb["ymax"] + pad)
     } else {
          xlim <- c(66, 100)
          ylim <- c(6,  38)
     }

     p <- ggplot2::ggplot() +
          tidyterra::geom_spatraster(data = r_day) +
          ggplot2::scale_fill_gradientn(
               colors   = pal$colors,
               name     = pal$name,
               na.value = "white",
               guide    = ggplot2::guide_colorbar(
                    barwidth = 1.2, barheight = 15,
                    title.position = "top", title.hjust = 0.5
               )
          ) +
          ggplot2::geom_sf(data = india_districts, fill = NA,
                           color = "grey60", linewidth = 0.15) +
          ggplot2::geom_sf(data = india_states, fill = NA,
                           color = "grey30", linewidth = 0.3) +
          {if (!is.null(zoom_sf))
               ggplot2::geom_sf(data = zoom_sf, fill = NA,
                                color = "black", linewidth = 0.8)} +
          ggplot2::labs(
               title    = title,
               subtitle = "Source: IMD Pune  |  Boundary: Survey of India  |  imdR",
               x = "Longitude (E)", y = "Latitude (N)"
          ) +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(
               plot.title      = ggplot2::element_text(face = "bold",
                                                       size = 13, hjust = 0.5),
               plot.subtitle   = ggplot2::element_text(size = 8, hjust = 0.5,
                                                       color = "grey50"),
               legend.position = "right",
               legend.title    = ggplot2::element_text(size = 10, face = "bold"),
               axis.title      = ggplot2::element_text(size = 10),
               panel.grid      = ggplot2::element_line(color = "grey92",
                                                       linewidth = 0.3)
          ) +
          ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

     if (!is.null(save_path)) {
          ggplot2::ggsave(save_path, plot = p, width = width,
                          height = height, dpi = 300, bg = "white")
          message(paste("Saved:", save_path))
     }

     print(p)
     return(invisible(p))
}

#' Plot a daily time series with 30-day rolling mean
#'
#' @param df Data frame with columns date and the variable.
#' @param variable Column name to plot.
#' @param title Plot title. Auto-generated if NULL.
#' @param save_path File path to save PNG. NULL = no save.
#' @param width Width in inches. Default 10.
#' @param height Height in inches. Default 5.
#' @return Invisible ggplot2 object.
#' @examples
#' \donttest{
#' # Extract point data and plot
#' df <- get_point(lat = 15.5, lon = 73.8,
#'                 variable = "rain",
#'                 start_yr = 2020, end_yr = 2020,
#'                 file_dir = tempdir(),
#'                 save_csv = FALSE)
#' plot_timeseries(df, variable = "rain")
#'
#' # Plot temperature with custom title
#' df_tmax <- get_point(lat = 15.5, lon = 73.8,
#'                      variable = "tmax",
#'                      start_yr = 2020, end_yr = 2020,
#'                      file_dir = tempdir(),
#'                      save_csv = FALSE)
#' plot_timeseries(df_tmax, variable = "tmax",
#'                 title = "Goa Maximum Temperature 2020")
#' }
#' @export
plot_timeseries <- function(df,
                            variable  = "rain",
                            title     = NULL,
                            save_path = NULL,
                            width     = 10,
                            height    = 5) {

     if (!variable %in% names(df))
          stop("Column '", variable, "' not found.")
     if (!"date" %in% names(df))
          stop("Data frame must have a 'date' column.")

     df$date <- as.Date(df$date)

     var_meta <- list(
          rain = list(label = "Daily Rainfall (mm/day)",
                      color = "#2E86C1", fill = "#AED6F1"),
          tmax = list(label = "Daily Max Temperature (C)",
                      color = "#E74C3C", fill = "#F1948A"),
          tmin = list(label = "Daily Min Temperature (C)",
                      color = "#1A5276", fill = "#7FB3D3"),
          dtr  = list(label = "Diurnal Temperature Range (C)",
                      color = "#6C3483", fill = "#C39BD3")
     )

     meta <- if (!is.null(var_meta[[variable]])) var_meta[[variable]] else
          list(label = variable, color = "steelblue", fill = "lightblue")

     if (is.null(title)) {
          loc   <- if (all(c("lat", "lon") %in% names(df)))
               paste0("(", df$lat[1], "N, ", df$lon[1], "E)") else ""
          title <- paste("IMD Daily", toupper(variable), loc)
     }

     df$smooth <- zoo::rollmean(df[[variable]], k = 30,
                                fill = NA, align = "center")

     p <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
          ggplot2::geom_col(ggplot2::aes(y = .data[[variable]]),
                            fill = meta$fill, color = NA,
                            width = 1, alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = smooth),
                             color = meta$color, linewidth = 1,
                             na.rm = TRUE) +
          ggplot2::scale_x_date(date_breaks = "1 month",
                                date_labels = "%b\n%Y") +
          ggplot2::labs(
               title    = title,
               subtitle = "30-day rolling mean (solid line)  |  imdR",
               x = NULL, y = meta$label
          ) +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(
               plot.title       = ggplot2::element_text(face = "bold",
                                                        hjust = 0.5, size = 13),
               plot.subtitle    = ggplot2::element_text(size = 8, hjust = 0.5,
                                                        color = "grey50"),
               axis.text.x      = ggplot2::element_text(size = 8),
               panel.grid.minor = ggplot2::element_blank()
          )

     if (!is.null(save_path)) {
          ggplot2::ggsave(save_path, plot = p, width = width,
                          height = height, dpi = 300, bg = "white")
          message(paste("Saved:", save_path))
     }

     print(p)
     return(invisible(p))
}
