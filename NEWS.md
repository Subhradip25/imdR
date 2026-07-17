# imdR 0.4.0

## New features

* `get_points()` extracts a daily time series for one variable at many
  coordinates in a single raster read, with grid-cell deduplication.
  Returns long or wide format and optionally writes CSV. Approximately
  44x faster than looping `get_point()` over locations (benchmarked on
  50 points, 2015-2020).

* `get_points_all()` extracts rainfall, maximum temperature, minimum
  temperature and diurnal temperature range (DTR) at many coordinates,
  merged into a single long data frame.

* `get_data()` gains `parallel` and `workers` arguments for concurrent
  year downloads. Requires the suggested packages `future` and
  `future.apply`; falls back to sequential download if unavailable.

## Improvements

* Download functions now retry up to three times on transient network
  failures via `httr2::req_retry()`.

## Bug fixes

* Removed the accidental export of the internal test helper
  `make_synthetic_rast()`.

* Resolved duplicate internal definitions of `.boundary_to_vect()` and
  `.raster_to_long()`.

* Fixed adaptive axis breaks in `plot_timeseries()`.

# imdR 0.3.0

* Initial CRAN release.
