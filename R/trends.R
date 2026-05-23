#' Mann-Kendall trend analysis with Sen's slope
#'
#' Aggregates multi-cell index data to spatial means per year, then
#' performs Mann-Kendall test and Sen's slope estimation.
#'
#' @param index_df Data frame from compute_rainfall_indices() or
#'   compute_temp_indices().
#' @param index_col Column name to analyse (e.g. "total", "dr").
#' @param level Not used in computation; passed to filename.
#' @param name Region name for output filename.
#' @param file_dir Output directory.
#' @param save_csv Save results table as CSV? Default TRUE.
#' @param plot Produce and save a trend plot? Default TRUE.
#' @return Invisible data frame with tau, S, pvalue, significance,
#'   sens_slope, trend_direction, total_change.
#' @examples
#' \dontrun{
#' # Download 10 years of rainfall
#' r   <- get_data("rain", 2011, 2020, "~/imdR_data")
#' idx <- compute_rainfall_indices(r, file_dir = "~/imdR_data")
#'
#' # Trend in annual total rainfall
#' trend_analysis(idx, index_col = "total",
#'                file_dir = "~/imdR_data")
#'
#' # Trend in rainy days
#' trend_analysis(idx, index_col = "dr",
#'                file_dir = "~/imdR_data")
#'
#' # Region-specific trend
#' goa_idx <- compute_rainfall_indices(r,
#'   level = "state", name = "Goa",
#'   file_dir = "~/imdR_data")
#' trend_analysis(goa_idx, index_col = "total",
#'                name = "Goa", file_dir = "~/imdR_data")
#'
#' # Temperature trend
#' tx  <- get_data("tmax", 2011, 2020, "~/imdR_data")
#' tn  <- get_data("tmin", 2011, 2020, "~/imdR_data")
#' tidx <- compute_temp_indices(tx, tn, file_dir = "~/imdR_data")
#' trend_analysis(tidx, index_col = "mean_tmax",
#'                file_dir = "~/imdR_data")
#' }
#' @export
trend_analysis <- function(index_df,
                           index_col,
                           level    = NULL,
                           name     = NULL,
                           file_dir = ".",
                           save_csv = TRUE,
                           plot     = TRUE) {

     if (!index_col %in% names(index_df))
          stop("Column '", index_col, "' not found.\n",
               "Available: ", paste(names(index_df), collapse=", "))

     if ("year" %in% names(index_df) && "cell" %in% names(index_df)) {
          annual <- stats::aggregate(index_df[[index_col]],
                                     by  = list(year = index_df$year),
                                     FUN = mean, na.rm = TRUE)
          names(annual)[2] <- index_col
     } else if ("year" %in% names(index_df)) {
          annual <- index_df[, c("year", index_col)]
     } else {
          stop("index_df must have a 'year' column.")
     }

     annual <- annual[order(annual$year), ]
     y      <- annual[[index_col]]
     x      <- annual$year
     n      <- length(y)

     if (n < 3) stop("Need at least 3 years for trend analysis.")

     cat("=== Trend Analysis:", index_col, "===\n")
     cat("Years:", min(x), "to", max(x), "(n =", n, ")\n\n")

     if (n < 10)
          cat("Note: MK test unreliable for n <", n,
              "years. Results are indicative only.\n\n")

     mk     <- suppressWarnings(Kendall::MannKendall(y))
     tau    <- round(as.numeric(mk$tau), 4)
     pval   <- round(as.numeric(mk$sl),  4)
     s_stat <- round(as.numeric(mk$S),   2)

     pairs  <- utils::combn(seq_len(n), 2)
     slopes <- (y[pairs[2,]] - y[pairs[1,]]) /
          (x[pairs[2,]] - x[pairs[1,]])
     sens_slope     <- round(stats::median(slopes, na.rm=TRUE), 4)
     sens_intercept <- round(stats::median(y, na.rm=TRUE) -
                                  sens_slope * stats::median(x, na.rm=TRUE), 4)

     sig <- if (pval <= 0.001) "***" else if (pval <= 0.01) "**" else
          if (pval <= 0.05)  "*"   else if (pval <= 0.1)  "."  else "ns"

     trend_dir <- if (sens_slope > 0) "Increasing" else
          if (sens_slope < 0) "Decreasing" else "No trend"

     cat("Mann-Kendall: tau =", tau, "| S =", s_stat,
         "| p =", pval, sig, "\n")
     cat("Sen's slope: ", sens_slope, index_col, "/ year |",
         trend_dir, "\n")
     cat("Total change:", round(sens_slope*(max(x)-min(x)),3),
         "over", max(x)-min(x), "years\n\n")
     cat("Significance: *** p<=0.001  ** p<=0.01",
         " * p<=0.05  . p<=0.1  ns p>0.1\n\n")

     result <- data.frame(
          index=index_col, n_years=n,
          year_start=min(x), year_end=max(x),
          tau=tau, S=s_stat, pvalue=pval, significance=sig,
          sens_slope=sens_slope, sens_intercept=sens_intercept,
          trend_direction=trend_dir,
          total_change=round(sens_slope*(max(x)-min(x)), 3)
     )

     if (save_csv) {
          region_tag <- if (!is.null(name))
               paste0("_", gsub(" ", "_", name)) else ""
          fname <- file.path(path.expand(file_dir),
                             paste0("trend_", index_col,
                                    region_tag, ".csv"))
          write.csv(result, fname, row.names = FALSE)
          cat("Saved:", fname, "\n")
     }

     if (plot) {
          trend_line <- sens_slope * x + sens_intercept
          p <- ggplot2::ggplot(
               data.frame(year=x, value=y, trend=trend_line)
          ) +
               ggplot2::geom_col(ggplot2::aes(x=year, y=value),
                                 fill="steelblue", alpha=0.7, width=0.7) +
               ggplot2::geom_line(ggplot2::aes(x=year, y=trend),
                                  color="red", linewidth=1.2) +
               ggplot2::labs(
                    title    = paste("Trend Analysis:", index_col),
                    subtitle = paste0("Sen's slope = ", sens_slope,
                                      " / year  |  p = ", pval, " ", sig,
                                      "  |  ", trend_dir),
                    x="Year", y=index_col,
                    caption="Red line = Sen's slope  |  imdR"
               ) +
               ggplot2::theme_bw(base_size=12) +
               ggplot2::theme(
                    plot.title    = ggplot2::element_text(face="bold", hjust=0.5),
                    plot.subtitle = ggplot2::element_text(size=9, hjust=0.5,
                                                          color="grey40"),
                    plot.caption  = ggplot2::element_text(size=8, color="grey50")
               )
          print(p)

          plot_path <- file.path(
               path.expand(file_dir),
               paste0("trend_", index_col,
                      if (!is.null(name)) paste0("_", gsub(" ","_",name))
                      else "", ".png"))
          ggplot2::ggsave(plot_path, plot=p, width=7, height=5,
                          dpi=300, bg="white")
          cat("Plot saved:", plot_path, "\n")
     }

     return(invisible(result))
}
