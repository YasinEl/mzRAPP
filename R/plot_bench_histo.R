# observe({
#   benchmark <- isolate(benchmark_data())
#   benchmark <- benchmark$PCal
#
#
#   var = input$bench_plotHisto
#
#
#   suppressWarnings(
#     if(!(var %in% c("molecule", "FileName", "Grp", "adduct"))){
#       p <- ggplot() +
#         geom_histogram(data = benchmark[!is.na(get(var))], aes(get(var)), bins = 30) +
#         ggtitle("Overview - Histogram") +
#         xlab(names(choice_vector_bench)[choice_vector_bench == var])
#
#     } else{
#       p <- ggplot() +
#         geom_bar(data = benchmark[!is.na(get(var))], aes(as.character(get(var)))) +
#         ggtitle("Overview - Histogram") +
#         xlab(names(choice_vector_bench)[choice_vector_bench == var])
#
#     }
#   )
#
#
#   output$bench_plotHisto <- renderPlotly(plotly::ggplotly(p, dynamicTicks = TRUE))
#
#
#   output$graph_area_bench_3 <-
#     renderPlotly(plot_Peak_with_predicted_peak(benchmark,
#                                                IndexNumber = input$index_number))
#
# })

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
