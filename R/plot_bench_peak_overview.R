#' plot_bench_peak_overview
#'
#' @param benchmark_data output from \code{\link{find_bench_peaks}}
#' @param molecule molecule
#' @param adduct adduct
#' @param ia isotopic abundance rounded to 2 digits
#'
#' @return plotly object
#' @export
#'
plot_bench_peak_overview <- function (benchmark_data, molecule, adduct, ia){
  benchmark_data <- benchmark_data$PCal
  if(nrow(benchmark_data[molecule == molecule & adduct == adduct & round(isoab, 2) == ia]) > 0){
    p <- suppressWarnings(
          plot_Peak_per_mol(benchmark_data,
                            mol = molecule,
                            add = adduct,
                            ia = ia
                           )
                         )
    return(p)
  } else {return(NULL)}
}
