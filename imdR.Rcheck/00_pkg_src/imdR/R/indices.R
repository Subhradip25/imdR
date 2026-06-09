#' Compute rainfall climate indices
#'
#' Computes 11 indices per grid cell per year. Handles single-year
#' SpatRasters and multi-year named lists from get_data().
#'
#' @param rain_raster A SpatRaster or named list from get_data("rain",...).
#' @param level NULL, "state", or "district".
#' @param name State or district name.
#' @param file_dir Output directory for CSV.
#' @param save_csv Save results as CSV? Default TRUE.
#' @return Invisible data frame with columns year, cell, dr, d64, d115,
#'   rx1day, rx5day, rtwd, sdii, total, cwd, cdd, pci.
#' @examples
#' \donttest{
#' # Full India rainfall indices for 2020
#' r   <- get_data("rain", 2020, 2020, tempdir())
#' idx <- compute_rainfall_indices(r, file_dir = tempdir())
#'
#' # State level indices
#' goa_idx <- compute_rainfall_indices(r,
#'   level    = "state",
#'   name     = "Goa",
#'   file_dir = tempdir())
#'
#' # Multi-year indices for trend analysis
#' r_3yr   <- get_data("rain", 2018, 2020, tempdir())
#' idx_3yr <- compute_rainfall_indices(r_3yr, file_dir = tempdir())
#' }
#' @export
compute_rainfall_indices <- function(rain_raster,
                                     level    = NULL,
                                     name     = NULL,
                                     file_dir = ".",
                                     save_csv = TRUE) {

     if (is.list(rain_raster) && !inherits(rain_raster, "SpatRaster")) {
          message("Multi-year list -- processing year by year...\n")
          all_results <- lapply(names(rain_raster), function(yr_name) {
               compute_rainfall_indices(
                    rain_raster = rain_raster[[yr_name]],
                    level = level, name = name,
                    file_dir = file_dir, save_csv = FALSE
               )
          })
          out           <- do.call(rbind, all_results)
          rownames(out) <- NULL
          valid_rows    <- !is.na(out$total)
          if (save_csv) {
               region_tag <- if (!is.null(name))
                    paste0("_", gsub(" ", "_", name)) else ""
               fname <- file.path(path.expand(file_dir),
                                  paste0("rainfall_indices", region_tag, ".csv"))
               write.csv(out[valid_rows, ], fname, row.names = FALSE)
               message(paste("Saved:", fname, "\n"))
          }
          return(invisible(out))
     }

     if (!is.null(level) && !is.null(name)) {
          boundary    <- get_boundary(level, name)
          rain_raster <- terra::mask(
               terra::crop(rain_raster, .boundary_to_vect(boundary)),
               .boundary_to_vect(boundary))
          message(paste("Computing indices for:", name, "\n\n"))
     }

     dates  <- as.Date(names(rain_raster))
     years  <- unique(format(dates, "%Y"))
     n_cell <- terra::ncell(rain_raster)
     message(paste(paste("Computing rainfall indices for", length(years)), "year(s)...\n"))
     results <- list()

     for (yr in years) {
          message(paste("  Year:", yr, "\n"))
          idx   <- which(format(dates, "%Y") == yr)
          vals  <- as.matrix(rain_raster[[idx]], wide = FALSE)
          valid <- rowSums(!is.na(vals)) > 0

          dr        <- rep(NA_real_, n_cell)
          dr[valid] <- rowSums(vals[valid,,drop=FALSE] >= 2.5, na.rm=TRUE)

          d64        <- rep(NA_real_, n_cell)
          d64[valid] <- rowSums(vals[valid,,drop=FALSE] >= 64.5, na.rm=TRUE)

          d115        <- rep(NA_real_, n_cell)
          d115[valid] <- rowSums(vals[valid,,drop=FALSE] >= 115.6, na.rm=TRUE)

          rx1day        <- rep(NA_real_, n_cell)
          rx1day[valid] <- suppressWarnings(
               apply(vals[valid,,drop=FALSE], 1, max, na.rm=TRUE))
          rx1day[is.infinite(rx1day)] <- NA

          rx5day        <- rep(NA_real_, n_cell)
          rx5day[valid] <- suppressWarnings(
               apply(vals[valid,,drop=FALSE], 1, function(x) {
                    x[is.na(x)] <- 0
                    max(zoo::rollsum(x, k=5, fill=NA, align="right"), na.rm=TRUE)
               }))
          rx5day[is.infinite(rx5day)] <- NA

          rtwd        <- rep(NA_real_, n_cell)
          rtwd[valid] <- rowSums(
               ifelse(vals[valid,,drop=FALSE] >= 2.5,
                      vals[valid,,drop=FALSE], 0), na.rm=TRUE)

          sdii        <- rep(NA_real_, n_cell)
          sdii[valid] <- ifelse(dr[valid] > 0, rtwd[valid]/dr[valid], NA)

          total        <- rep(NA_real_, n_cell)
          total[valid] <- rowSums(vals[valid,,drop=FALSE], na.rm=TRUE)

          cwd        <- rep(NA_real_, n_cell)
          cwd[valid] <- apply(vals[valid,,drop=FALSE], 1, function(x) {
               wet <- x >= 2.5 & !is.na(x)
               if (!any(wet)) return(0)
               r <- rle(wet); max(r$lengths[r$values])
          })

          cdd        <- rep(NA_real_, n_cell)
          cdd[valid] <- apply(vals[valid,,drop=FALSE], 1, function(x) {
               dry <- x < 2.5 | is.na(x)
               r <- rle(dry); max(r$lengths[r$values])
          })

          months   <- format(dates[idx], "%m")
          pci_vals <- rep(NA_real_, n_cell)
          pci_vals[valid] <- apply(vals[valid,,drop=FALSE], 1, function(x) {
               if (all(is.na(x))) return(NA)
               mt <- tapply(x, months, sum, na.rm=TRUE)
               at <- sum(mt, na.rm=TRUE)
               if (at == 0) return(NA)
               sum((mt/at)^2) * 100
          })

          results[[yr]] <- data.frame(
               year=as.integer(yr), cell=seq_len(n_cell),
               dr=round(dr,1), d64=round(d64,1), d115=round(d115,1),
               rx1day=round(rx1day,2), rx5day=round(rx5day,2),
               rtwd=round(rtwd,2), sdii=round(sdii,3), total=round(total,2),
               cwd=round(cwd,1), cdd=round(cdd,1), pci=round(pci_vals,3)
          )
     }

     out           <- do.call(rbind, results)
     rownames(out) <- NULL
     valid_rows    <- !is.na(out$total)
     message(paste(paste("\nDone! Valid land cells:", sum(valid_rows)), "\n"))

     if (save_csv) {
          region_tag <- if (!is.null(name))
               paste0("_", gsub(" ", "_", name)) else ""
          fname <- file.path(path.expand(file_dir),
                             paste0("rainfall_indices", region_tag, ".csv"))
          write.csv(out[valid_rows, ], fname, row.names = FALSE)
          message(paste("Saved:", fname, "\n"))
     }

     return(invisible(out))
}

#' Compute temperature climate indices
#'
#' Computes 13 indices per grid cell per year from daily tmax and tmin.
#'
#' @param tmax_raster A SpatRaster or named list for tmax.
#' @param tmin_raster A SpatRaster or named list for tmin.
#' @param level NULL, "state", or "district".
#' @param name State or district name.
#' @param file_dir Output directory for CSV.
#' @param save_csv Save results as CSV? Default TRUE.
#' @return Invisible data frame with columns year, cell, mean_tmax,
#'   mean_tmin, mean_dtr, txx, txn, tnx, tnn, su35, su40, tr10, tr25,
#'   wsdi, csdi.
#' @examples
#' \donttest{
#' # Full India temperature indices for 2020
#' tx  <- get_data("tmax", 2020, 2020, tempdir())
#' tn  <- get_data("tmin", 2020, 2020, tempdir())
#' idx <- compute_temp_indices(tx, tn, file_dir = tempdir())
#'
#' # State level indices
#' goa_idx <- compute_temp_indices(tx, tn,
#'   level    = "state",
#'   name     = "Goa",
#'   file_dir = tempdir())
#'
#' # Multi-year temperature indices
#' tx_3yr  <- get_data("tmax", 2018, 2020, tempdir())
#' tn_3yr  <- get_data("tmin", 2018, 2020, tempdir())
#' idx_3yr <- compute_temp_indices(tx_3yr, tn_3yr,
#'                                  file_dir = tempdir())
#' }
#' @export
compute_temp_indices <- function(tmax_raster,
                                 tmin_raster,
                                 level    = NULL,
                                 name     = NULL,
                                 file_dir = ".",
                                 save_csv = TRUE) {

     if (is.list(tmax_raster) && !inherits(tmax_raster, "SpatRaster")) {
          message("Multi-year list -- processing year by year...\n")
          all_results <- lapply(names(tmax_raster), function(yr_name) {
               compute_temp_indices(
                    tmax_raster=tmax_raster[[yr_name]],
                    tmin_raster=tmin_raster[[yr_name]],
                    level=level, name=name,
                    file_dir=file_dir, save_csv=FALSE
               )
          })
          out           <- do.call(rbind, all_results)
          rownames(out) <- NULL
          valid_rows    <- !is.na(out$mean_tmax)
          if (save_csv) {
               region_tag <- if (!is.null(name))
                    paste0("_", gsub(" ", "_", name)) else ""
               fname <- file.path(path.expand(file_dir),
                                  paste0("temp_indices", region_tag, ".csv"))
               write.csv(out[valid_rows, ], fname, row.names = FALSE)
               message(paste("Saved:", fname, "\n"))
          }
          return(invisible(out))
     }

     if (!is.null(level) && !is.null(name)) {
          boundary    <- get_boundary(level, name)
          tmax_raster <- terra::mask(terra::crop(tmax_raster, .boundary_to_vect(boundary)),
                                     .boundary_to_vect(boundary))
          tmin_raster <- terra::mask(terra::crop(tmin_raster, .boundary_to_vect(boundary)),
                                     .boundary_to_vect(boundary))
          message(paste("Computing temperature indices for:", name, "\n\n"))
     }

     dates  <- as.Date(names(tmax_raster))
     years  <- unique(format(dates, "%Y"))
     n_cell <- terra::ncell(tmax_raster)
     message(paste(paste("Computing temperature indices for", length(years)), "year(s)...\n"))
     results <- list()

     for (yr in years) {
          message(paste("  Year:", yr, "\n"))
          idx <- which(format(dates, "%Y") == yr)
          tx  <- as.matrix(tmax_raster[[idx]], wide=FALSE)
          tn  <- as.matrix(tmin_raster[[idx]], wide=FALSE)
          if (is.null(dim(tx))) tx <- matrix(tx, nrow=1)
          if (is.null(dim(tn))) tn <- matrix(tn, nrow=1)
          valid <- rowSums(!is.na(tx)) > 0

          mean_tmax        <- rep(NA_real_, n_cell)
          mean_tmax[valid] <- rowMeans(tx[valid,,drop=FALSE], na.rm=TRUE)
          mean_tmin        <- rep(NA_real_, n_cell)
          mean_tmin[valid] <- rowMeans(tn[valid,,drop=FALSE], na.rm=TRUE)
          dtr_daily        <- tx - tn
          mean_dtr         <- rep(NA_real_, n_cell)
          mean_dtr[valid]  <- rowMeans(dtr_daily[valid,,drop=FALSE], na.rm=TRUE)

          txx        <- rep(NA_real_, n_cell)
          txx[valid] <- suppressWarnings(
               apply(tx[valid,,drop=FALSE], 1, max, na.rm=TRUE))
          txx[is.infinite(txx)] <- NA

          txn        <- rep(NA_real_, n_cell)
          txn[valid] <- suppressWarnings(
               apply(tx[valid,,drop=FALSE], 1, min, na.rm=TRUE))
          txn[is.infinite(txn)] <- NA

          tnx        <- rep(NA_real_, n_cell)
          tnx[valid] <- suppressWarnings(
               apply(tn[valid,,drop=FALSE], 1, max, na.rm=TRUE))
          tnx[is.infinite(tnx)] <- NA

          tnn        <- rep(NA_real_, n_cell)
          tnn[valid] <- suppressWarnings(
               apply(tn[valid,,drop=FALSE], 1, min, na.rm=TRUE))
          tnn[is.infinite(tnn)] <- NA

          su35        <- rep(NA_real_, n_cell)
          su35[valid] <- rowSums(tx[valid,,drop=FALSE] >= 35, na.rm=TRUE)
          su40        <- rep(NA_real_, n_cell)
          su40[valid] <- rowSums(tx[valid,,drop=FALSE] >= 40, na.rm=TRUE)
          tr10        <- rep(NA_real_, n_cell)
          tr10[valid] <- rowSums(tn[valid,,drop=FALSE] <= 10, na.rm=TRUE)
          tr25        <- rep(NA_real_, n_cell)
          tr25[valid] <- rowSums(tn[valid,,drop=FALSE] >= 25, na.rm=TRUE)

          p90_tx      <- apply(tx, 1, quantile, probs=0.90, na.rm=TRUE)
          wsdi        <- rep(NA_real_, n_cell)
          wsdi[valid] <- sapply(seq_len(sum(valid)), function(ci) {
               x <- tx[valid,,drop=FALSE][ci,]
               hot <- x > p90_tx[valid][ci] & !is.na(x)
               r <- rle(hot)
               if (!any(r$values)) return(0)
               sum(r$lengths[r$values & r$lengths >= 6])
          })

          p10_tn      <- apply(tn, 1, quantile, probs=0.10, na.rm=TRUE)
          csdi        <- rep(NA_real_, n_cell)
          csdi[valid] <- sapply(seq_len(sum(valid)), function(ci) {
               x <- tn[valid,,drop=FALSE][ci,]
               cold <- x < p10_tn[valid][ci] & !is.na(x)
               r <- rle(cold)
               if (!any(r$values)) return(0)
               sum(r$lengths[r$values & r$lengths >= 6])
          })

          results[[yr]] <- data.frame(
               year=as.integer(yr), cell=seq_len(n_cell),
               mean_tmax=round(mean_tmax,2), mean_tmin=round(mean_tmin,2),
               mean_dtr=round(mean_dtr,2), txx=round(txx,2),
               txn=round(txn,2), tnx=round(tnx,2), tnn=round(tnn,2),
               su35=round(su35,0), su40=round(su40,0),
               tr10=round(tr10,0), tr25=round(tr25,0),
               wsdi=round(wsdi,0), csdi=round(csdi,0)
          )
     }

     out           <- do.call(rbind, results)
     rownames(out) <- NULL
     valid_rows    <- !is.na(out$mean_tmax)
     message(paste(paste("\nDone! Valid land cells:", sum(valid_rows)), "\n"))

     if (save_csv) {
          region_tag <- if (!is.null(name))
               paste0("_", gsub(" ", "_", name)) else ""
          fname <- file.path(path.expand(file_dir),
                             paste0("temp_indices", region_tag, ".csv"))
          write.csv(out[valid_rows, ], fname, row.names = FALSE)
          message(paste("Saved:", fname, "\n"))
     }

     return(invisible(out))
}
