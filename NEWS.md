# imdR 0.4.0

## New features

* `get_points()` extracts a daily time series for one variable at many
  coordinates in a single raster read, with grid-cell deduplication.
  Returns long or wide format and writes CSV. ~44x faster than looping
  `get_point()` over locations (50 points, 2015-2020).
* `get_points_all()` extracts rain, tmax, tmin and diurnal temperature
  range (DTR) at many coordinates, merged into a single long data frame.
* `get_data()` gains `parallel` and `workers` arguments for concurrent
  year downloads (requires suggested `future` and `future.apply`;
  falls back to sequential if unavailable).

## Improvements

* Downloads now retry up to 3 times on transient network failures.

# imdR 0.3.0

* Initial CRAN release.
