#' plot_bench_overview
#'
#' @param benchmark_data
#' @param x
#' @param y
#' @param colb
#' @param choice_vector_bench
#'
#' @return
#' @export
#'
#' @examples
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
