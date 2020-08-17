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
plot_bench_histo <- function(benchmark_data, var, choice_vector_bench, color = "blue", post_comp = FALSE){

  if(post_comp == FALSE){
    benchmark_data <- benchmark_data$PCal
  } else if (post_comp == TRUE){
    benchmark_data <- rbindlist(list(benchmark_data$c_table, benchmark_data$nf_b_table), fill = TRUE, use.names = TRUE)
  }

  suppressWarnings(
    if(!(var %in% c("molecule", "FileName", "Grp", "adduct", "molecule_b", "sample_name_b", "Grp_b", "adduct_b"))){

      if(var == "peak_height_b" | var == "peak_area_b"){
        benchmark_data[, eval(quote(var)) := log10(get(var))]
      }

      p <- ggplot() +
        geom_histogram(data = benchmark_data[!is.na(get(var))],
                       aes(
                         get(var),
                         fill = var,
                         colour = var
                         ),
                       fill = color,
                       bins = 30) +
        theme_classic() +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var]) +
        scale_colour_manual(name="benchmark data",
                            values= color,
                            labels= names(choice_vector_bench)[choice_vector_bench == var]) +
        scale_fill_manual(name="benchmark data",
                          values= color,
                          labels= names(choice_vector_bench)[choice_vector_bench == var]) +
        theme(legend.title = element_blank())


    } else{
      p <- ggplot() +
        geom_bar(data = benchmark_data[!is.na(get(var))], aes(as.character(get(var))), fill = color) +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var])

    }
  )

  p <- plotly::ggplotly(p, dynamicTicks = TRUE)

  p$x$data[[1]]$name <- names(choice_vector_bench)[choice_vector_bench == var]
  return(p)
}
