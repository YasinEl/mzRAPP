#' plot_bench_peak_overview
#'
#' @param benchmark_data
#' @param molecule
#' @param adduct
#' @param ia
#'
#' @return
#' @export
#'
#' @examples
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
