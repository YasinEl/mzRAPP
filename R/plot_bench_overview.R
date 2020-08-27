#' plot_bench_overview
#'
#' @param benchmark_data output from \code{\link{findBenchPeaks}}
#' @param x variable (column name) to be plotted on x axis
#' @param y variable (column name) to be plotted on y axis
#' @param colb variable (column name) to color by
#' @param choice_vector_bench named vector including all variables used as elements
#'
#' @return plotly object
#' @export
#'
plot_bench_overview <- function(benchmark_data, x, y, colb, choice_vector_bench){
  benchmark_data <- benchmark_data$PCal

  suppressWarnings(
    p <- ggplot() +
      geom_point(data = benchmark_data[!is.na(get(x)) & !is.na(get(y))], aes(x = get(x),
                                                                        y = get(y),
                                                                        color = get(colb),
                                                                        molecule = molecule,
                                                                        adduct = adduct,
                                                                        isoab = isoab,
                                                                        sample_name = FileName,
                                                                        key = IDX)) +
      theme_classic() +
      labs(x = names(choice_vector_bench)[choice_vector_bench == x],
           y = names(choice_vector_bench)[choice_vector_bench == y]) +
      labs(color=names(choice_vector_bench)[choice_vector_bench == colb]) +
      ggtitle("Overview - Peaks"))

    p <- plotly::ggplotly(p,
                          tooltip = c("molecule",
                                      "adduct",
                                      "isoab",
                                      "sample_name"),
                          dynamicTicks = TRUE,
                          source = "bench_scatter")#,
                          #width = 1000)
  return(p)
}
