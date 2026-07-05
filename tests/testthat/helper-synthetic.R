make_synthetic_rast <- function(vals_by_cell, start = "2020-01-01", xmin = 76, xmax = 77, ymin = 29, ymax = 30) {
  n_days <- ncol(vals_by_cell); n_cell <- nrow(vals_by_cell)
  ncol_r <- ceiling(sqrt(n_cell)); nrow_r <- ceiling(n_cell / ncol_r)
  r <- terra::rast(nrows=nrow_r, ncols=ncol_r, nlyr=n_days, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, crs="EPSG:4326")
  full <- matrix(NA_real_, nrow=nrow_r*ncol_r, ncol=n_days)
  full[seq_len(n_cell), ] <- vals_by_cell
  terra::values(r) <- full
  names(r) <- as.character(seq.Date(as.Date(start), by="day", length.out=n_days))
  terra::time(r) <- as.Date(names(r)); r
}
