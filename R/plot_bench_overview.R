# observe({
#   benchmark <- isolate(benchmark_data())
#   benchmark <- benchmark$PCal
#   #output$graph_area_bench_1 <-
#   #  renderPlotly(plot_vs_prediction(PC, y = peaks.area))
#   #output$graph_area_bench_2 <-
#   #  renderPlotly(plot_vs_prediction(PC, y = ErrorRel_A))
#
#
#   x = input$bench_plotxy_input_x
#   y = input$bench_plotxy_input_y
#   colb = input$bench_plotxy_input_color
#
#   suppressWarnings(
#     p <- ggplot() +
#       geom_point(data = benchmark[!is.na(get(x)) & !is.na(get(y))], aes(x = get(x),
#                                                                         y = get(y),
#                                                                         color = get(colb),
#                                                                         molecule = molecule,
#                                                                         adduct = adduct,
#                                                                         isoabb = isoabb,
#                                                                         sample_name = FileName)) +
#       labs(x = names(choice_vector_bench)[choice_vector_bench == x],
#            y = names(choice_vector_bench)[choice_vector_bench == y]) +
#       labs(color=names(choice_vector_bench)[choice_vector_bench == colb]) +
#       ggtitle("Overview - Peaks")
#   )
#   output$bench_plotxy <- renderPlotly(plotly::ggplotly(p,
#                                                        tooltip = c("molecule",
#                                                                    "adduct",
#                                                                    "isoabb",
#                                                                    "sample_name"),
#                                                        dynamicTicks = TRUE,
#                                                        width = 1000))
# })


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
                                                                        isoabb = isoabb,
                                                                        sample_name = FileName)) +
      labs(x = names(choice_vector_bench)[choice_vector_bench == x],
           y = names(choice_vector_bench)[choice_vector_bench == y]) +
      labs(color=names(choice_vector_bench)[choice_vector_bench == colb]) +
      ggtitle("Overview - Peaks"))

    p <- plotly::ggplotly(p,
                          tooltip = c("molecule",
                                      "adduct",
                                      "isoabb",
                                      "sample_name"),
                          dynamicTicks = TRUE,
                          width = 1000)
  return(p)
}
