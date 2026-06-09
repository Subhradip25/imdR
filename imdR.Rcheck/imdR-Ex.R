pkgname <- "imdR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "imdR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('imdR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("compute_rainfall_indices")
### * compute_rainfall_indices

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compute_rainfall_indices
### Title: Compute rainfall climate indices
### Aliases: compute_rainfall_indices

### ** Examples

## No test: 
# Full India rainfall indices for 2020
r   <- get_data("rain", 2020, 2020, tempdir())
idx <- compute_rainfall_indices(r, file_dir = tempdir())

# State level indices
goa_idx <- compute_rainfall_indices(r,
  level    = "state",
  name     = "Goa",
  file_dir = tempdir())

# Multi-year indices for trend analysis
r_3yr   <- get_data("rain", 2018, 2020, tempdir())
idx_3yr <- compute_rainfall_indices(r_3yr, file_dir = tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compute_rainfall_indices", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compute_temp_indices")
### * compute_temp_indices

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compute_temp_indices
### Title: Compute temperature climate indices
### Aliases: compute_temp_indices

### ** Examples

## No test: 
# Full India temperature indices for 2020
tx  <- get_data("tmax", 2020, 2020, tempdir())
tn  <- get_data("tmin", 2020, 2020, tempdir())
idx <- compute_temp_indices(tx, tn, file_dir = tempdir())

# State level indices
goa_idx <- compute_temp_indices(tx, tn,
  level    = "state",
  name     = "Goa",
  file_dir = tempdir())

# Multi-year temperature indices
tx_3yr  <- get_data("tmax", 2018, 2020, tempdir())
tn_3yr  <- get_data("tmin", 2018, 2020, tempdir())
idx_3yr <- compute_temp_indices(tx_3yr, tn_3yr,
                                 file_dir = tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compute_temp_indices", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_by_boundary")
### * extract_by_boundary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_by_boundary
### Title: Extract IMD raster masked to a state or district boundary
### Aliases: extract_by_boundary

### ** Examples

## No test: 
r <- get_data("rain", 2020, 2020, tempdir())

# Return masked raster without saving
nagaland_rain <- extract_by_boundary(r, "state", "Nagaland", "rain")

# State — NetCDF
extract_by_boundary(r, "state", "Nagaland", "rain",
  save = TRUE, format = "netcdf", file_dir = tempdir())

# State — GeoTIFF (QGIS/ArcGIS)
extract_by_boundary(r, "state", "Nagaland", "rain",
  save = TRUE, format = "geotiff", file_dir = tempdir())

# State — CSV (all grid points x all days)
extract_by_boundary(r, "state", "Nagaland", "rain",
  save = TRUE, format = "csv", file_dir = tempdir())

# District — all formats work the same way
extract_by_boundary(r, "district", "North Goa", "rain",
  save = TRUE, format = "csv", file_dir = tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_by_boundary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_bbox")
### * get_bbox

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_bbox
### Title: Extract IMD data within a bounding box
### Aliases: get_bbox

### ** Examples

## No test: 
# Indo-Gangetic Plains — NetCDF
get_bbox(lat_min = 24, lat_max = 30,
         lon_min = 73, lon_max = 88,
         variable = "rain",
         start_yr = 2020, end_yr = 2020,
         file_dir = tempdir(),
         format   = "netcdf")

# Western Ghats — GeoTIFF
get_bbox(lat_min = 8,  lat_max = 21,
         lon_min = 73, lon_max = 78,
         variable = "rain",
         start_yr = 2020, end_yr = 2020,
         file_dir = tempdir(),
         format   = "geotiff")

# Northeast India — CSV (all grid points x all days)
get_bbox(lat_min = 22, lat_max = 29,
         lon_min = 89, lon_max = 97,
         variable = "rain",
         start_yr = 2020, end_yr = 2020,
         file_dir = tempdir(),
         format   = "csv")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_bbox", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_boundary")
### * get_boundary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_boundary
### Title: Get the sf boundary for a named state or district
### Aliases: get_boundary

### ** Examples

goa       <- get_boundary("state", "Goa")
north_goa <- get_boundary("district", "North Goa")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_boundary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_data")
### * get_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_data
### Title: Download and read IMD gridded data
### Aliases: get_data

### ** Examples

## No test: 
# Download single year rainfall
rain2020 <- get_data("rain", 2020, 2020, tempdir())

# Download multiple years (returns named list)
rain_3yr <- get_data("rain", 2018, 2020, tempdir())

# Download temperature data
tmax2020 <- get_data("tmax", 2020, 2020, tempdir())
tmin2020 <- get_data("tmin", 2020, 2020, tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_point")
### * get_point

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_point
### Title: Extract daily time series for a single variable at a point
### Aliases: get_point

### ** Examples

## No test: 
# Extract daily rainfall at Panaji, Goa
df <- get_point(lat = 15.5, lon = 73.8,
                variable = "rain",
                start_yr = 2020, end_yr = 2020,
                file_dir = tempdir())
head(df)

# Extract temperature
df_tmax <- get_point(lat = 15.5, lon = 73.8,
                     variable = "tmax",
                     start_yr = 2020, end_yr = 2020,
                     file_dir = tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_point", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_point_all")
### * get_point_all

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_point_all
### Title: Extract daily time series for all variables at a point
### Aliases: get_point_all

### ** Examples

## No test: 
# Extract rain, tmax, tmin and DTR at Panaji, Goa
df <- get_point_all(lat = 15.5, lon = 73.8,
                    start_yr = 2020, end_yr = 2020,
                    file_dir = tempdir())
head(df)

# Long time series — works on Windows without memory errors
df <- get_point_all(lat = 15.5, lon = 73.8,
                    start_yr = 1985, end_yr = 2020,
                    file_dir = tempdir())
nrow(df)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_point_all", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_districts")
### * list_districts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_districts
### Title: List district names, optionally filtered by state
### Aliases: list_districts

### ** Examples

list_districts()
list_districts("Goa")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_districts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("list_states")
### * list_states

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list_states
### Title: List all state names in the bundled SOI shapefile
### Aliases: list_states

### ** Examples

list_states()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list_states", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("open_data")
### * open_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: open_data
### Title: Read cached IMD .grd files from disk
### Aliases: open_data

### ** Examples

## No test: 
rain2020 <- open_data("rain", 2020, 2020, tempdir())
rain_3yr <- open_data("rain", 2018, 2020, tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("open_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_imd")
### * plot_imd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_imd
### Title: Plot a single day of IMD gridded data
### Aliases: plot_imd

### ** Examples

## No test: 
r <- get_data("rain", 2020, 2020, tempdir())

# Full India map
plot_imd(r, "2020-06-28", "rain")

# Zoom to Kerala
plot_imd(r, "2020-06-28", "rain",
         level = "state", name = "Kerala")

# Zoom to North Goa district
plot_imd(r, "2020-06-28", "rain",
         level = "district", name = "North Goa")

# Save to file
plot_imd(r, "2020-06-28", "rain",
         save_path = file.path(tempdir(), "rain_20200628.png"))
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_imd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_timeseries")
### * plot_timeseries

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_timeseries
### Title: Plot a daily time series with 30-day rolling mean
### Aliases: plot_timeseries

### ** Examples

## No test: 
# Extract point data and plot
df <- get_point(lat = 15.5, lon = 73.8,
                variable = "rain",
                start_yr = 2020, end_yr = 2020,
                file_dir = tempdir(),
                save_csv = FALSE)
plot_timeseries(df, variable = "rain")

# Plot temperature with custom title
df_tmax <- get_point(lat = 15.5, lon = 73.8,
                     variable = "tmax",
                     start_yr = 2020, end_yr = 2020,
                     file_dir = tempdir(),
                     save_csv = FALSE)
plot_timeseries(df_tmax, variable = "tmax",
                title = "Goa Maximum Temperature 2020")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_timeseries", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("to_csv")
### * to_csv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: to_csv
### Title: Extract a daily time series at a point location
### Aliases: to_csv

### ** Examples

## No test: 
r  <- get_data("rain", 2020, 2020, tempdir())
df <- to_csv(r, lat = 15.5, lon = 73.8)
head(df)

# Save directly to file
to_csv(r, lat = 15.5, lon = 73.8,
       file_path = file.path(tempdir(), "panaji_rain_2020.csv"))
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("to_csv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("to_geotiff")
### * to_geotiff

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: to_geotiff
### Title: Save an IMD SpatRaster as a compressed GeoTIFF
### Aliases: to_geotiff

### ** Examples

## No test: 
r <- get_data("rain", 2020, 2020, tempdir())
to_geotiff(r, file.path(tempdir(), "rain_2020.tif"))

# Save a boundary-extracted region
goa <- extract_by_boundary(r, "state", "Goa", "rain")
to_geotiff(goa, file.path(tempdir(), "rain_Goa_2020.tif"))
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("to_geotiff", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("to_netcdf")
### * to_netcdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: to_netcdf
### Title: Save an IMD SpatRaster as a CF-1.7 compliant NetCDF file
### Aliases: to_netcdf

### ** Examples

## No test: 
r <- get_data("rain", 2020, 2020, tempdir())
to_netcdf(r, file.path(tempdir(), "rain_2020.nc"), "rain")

# Save a boundary-extracted region
goa <- extract_by_boundary(r, "state", "Goa", "rain")
to_netcdf(goa, file.path(tempdir(), "rain_Goa_2020.nc"), "rain")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("to_netcdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trend_analysis")
### * trend_analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trend_analysis
### Title: Mann-Kendall trend analysis with Sen's slope
### Aliases: trend_analysis

### ** Examples

## No test: 
# Download 10 years of rainfall
r   <- get_data("rain", 2011, 2020, tempdir())
idx <- compute_rainfall_indices(r, file_dir = tempdir())

# Trend in annual total rainfall
trend_analysis(idx, index_col = "total",
               file_dir = tempdir())

# Trend in rainy days
trend_analysis(idx, index_col = "dr",
               file_dir = tempdir())

# Region-specific trend
goa_idx <- compute_rainfall_indices(r,
  level = "state", name = "Goa",
  file_dir = tempdir())
trend_analysis(goa_idx, index_col = "total",
               name = "Goa", file_dir = tempdir())

# Temperature trend
tx  <- get_data("tmax", 2011, 2020, tempdir())
tn  <- get_data("tmin", 2011, 2020, tempdir())
tidx <- compute_temp_indices(tx, tn, file_dir = tempdir())
trend_analysis(tidx, index_col = "mean_tmax",
               file_dir = tempdir())
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trend_analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
