#' plot_bench_heatmap
#'
#' @param benchmark_data
#'
#' @return
#' @export
#'
#' @examples
plot_bench_heatmap <- function(benchmark_data) {
  benchmark_all <- benchmark_data$PCal
  benchmark <- unique(benchmark_all[!is.na(peaks.PpP) & isoab == 100, c("molecule", "FileName", "isoab")], by = c("molecule", "FileName"))
  benchmark_files <- benchmark_data$files
  benchmark_targets <- benchmark_data$targets
  benchmark_targets <- unique(benchmark_targets[, "molecule"])



  files.dt <- data.table(FileName = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(benchmark_files)))
  files.dt[,fileIdx:= seq(nrow(files.dt))]
  benchmark_targets$fileIdx <- rep(1, nrow(benchmark_targets))
  benchmark_targets <- benchmark_targets[files.dt, on=.(fileIdx<=fileIdx), allow.cartesian = TRUE]

  plot.dt <- benchmark[benchmark_targets, on = .(molecule, FileName), nomatch = NA]

  plot.dt$Found <- !is.na(plot.dt$isoab)


  plot.dt <- plot.dt[Found == TRUE, .(nr = .N), by = .(molecule, Found)][, !"Found"][plot.dt, on =.(molecule), nomatch = NA]
  plot.dt[is.na(nr)]$nr <- 0

  plot_hm <- ggplot(
    plot.dt,
    aes(
      x = reorder(as.character(molecule), nr),
      y = as.character(FileName),
      fill = as.factor(Found),
      molecule = molecule,
      FileName = FileName
    )
  ) +
    geom_tile() +
    scale_fill_manual(values=c("firebrick", "forestgreen")) +
    ggtitle("Found/not found compounds per sample") +
    labs(x = "molecule", y = "file name", fill = "Found")

  p <- plotly::ggplotly(
                        plot_hm,
                        width = 1000,
                        tooltip = c("molecule", "FileName")
                       )

  no_of_b_peaks <- nrow(benchmark_all)
  no_of_compounds <- length(unique(benchmark_all$molecule))
  median_fwhm <- round(median(benchmark_all$peaks.FW50M, na.rm = TRUE), 1)
  median_no_PpP <- median(benchmark_all$peaks.PpP)
  median_mz_acc <- round(median(benchmark_all$peaks.mz_accuracy_ppm), 1)

  t <- paste0("# of benchmark peaks: ", no_of_b_peaks, "       # of compounds: " , no_of_compounds,
         "       median FWHM [s]: ", median_fwhm, "       median # points per peak: ",median_no_PpP, "       median mz accuracy [ppm]: ",median_mz_acc)
  return(list('p' = p, 't' = t))
}
