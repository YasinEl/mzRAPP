#' plot_bench_histo
#'
#' @param benchmark_data
#' @param var
#' @param choice_vector_bench
#'
#' @return
#' @export
#'
#' @examples
plot_bench_histo <- function(benchmark_data, var, choice_vector_bench){
  benchmark_data <- benchmark_data$PCal

  suppressWarnings(
    if(!(var %in% c("molecule", "FileName", "Grp", "adduct"))){
      p <- ggplot() +
        geom_histogram(data = benchmark_data[!is.na(get(var))], aes(get(var)), bins = 30) +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var])

    } else{
      p <- ggplot() +
        geom_bar(data = benchmark_data[!is.na(get(var))], aes(as.character(get(var)))) +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var])

    }
  )
  p <- plotly::ggplotly(p, dynamicTicks = TRUE)
  return(p)
}
