library(imdR)
library(microbenchmark)

file_dir <- tempdir()
start_yr <- 2015; end_yr <- 2020
set.seed(1)

N   <- 50
pts <- data.frame(name=paste0("P",seq_len(N)),
                  lat=runif(N,8,30), lon=runif(N,70,90))

invisible(get_data("rain", start_yr, end_yr, file_dir))

loop_points <- function()
  do.call(rbind, lapply(seq_len(nrow(pts)), function(i)
    get_point(pts$lat[i], pts$lon[i], "rain",
              start_yr, end_yr, file_dir, save_csv=FALSE)))

fast_points <- function()
  get_points(pts, "rain", start_yr, end_yr, file_dir,
             format="long", download=FALSE, dedup=TRUE, save_csv=FALSE)

bm <- microbenchmark(
  looped = suppressMessages(loop_points()),
  fast   = suppressMessages(fast_points()),
  times  = 5L)
print(bm)

cat("\nMedian speedup:",
    round(median(bm$time[bm$expr=="looped"])/
          median(bm$time[bm$expr=="fast"]),1), "x\n")
