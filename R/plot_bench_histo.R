
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
plot_bench_histo <- function(benchmark_data, var, choice_vector_bench, color = "blue", post_comp = FALSE, rm_NF_legend = FALSE){

  if(post_comp == FALSE){
    if(is.data.table(benchmark_data) == FALSE){
      benchmark_data <- benchmark_data$PCal
    }
  } else if (post_comp == TRUE){
    benchmark_data <- rbindlist(list(benchmark_data$c_table, benchmark_data$nf_b_table), fill = TRUE, use.names = TRUE)
    benchmark_data <- benchmark_data[main_peak == TRUE |
                                       is.na(peak_area_ug)]
    dtf <- comp_data$feature_table

    benchmark_data <- dtf[!is.na(area_b)][benchmark_data, on = .(molecule_b, adduct_b, isoab_b, sample_name_b), nomatch = NA]

    #benchmark_data[is.na(main_peak), main_peak := FALSE]
    #benchmark_data <- benchmark_data[order(main_peak)]

    benchmark_data[, peak_found := FALSE]
    benchmark_data[!is.na(area_g), peak_found := TRUE]

    #df$Private <- relevel(df$Private, "Yes")
    #benchmark_data[, main_peak := as.logical(main_peak)]
    #benchmark_data <- benchmark_data[relevel(as.factor(main_peak), TRUE)]
    #return(benchmark_data)
  }

  suppressWarnings(
    if(!(var %in% c("molecule", "FileName", "Grp", "adduct", "molecule_b", "sample_name_b", "Grp_b", "adduct_b"))){

      if(var == "peak_height_b" | var == "peak_area_b"){
        benchmark_data[, eval(quote(var)) := log10(get(var))]
      }

      p <- ggplot(data = benchmark_data[!is.na(get(var))], aes(get(var), fill = if(post_comp == TRUE){as.factor(peak_found)}else{color})) +
        geom_histogram(position = "dodge",
                       #fill = color,
                       bins = 30) +
        theme_classic() +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var]) +
        scale_colour_manual(name="benchmark data",
                            values= if(post_comp == TRUE){c("red", color)}else{color},
                            labels= if(post_comp == TRUE){c(names(choice_vector_bench)[choice_vector_bench == var], "2")}else{
                              names(choice_vector_bench)[choice_vector_bench == var]}) +
        scale_fill_manual(name="benchmark data",
                          values= if(post_comp == TRUE){c("red", color)}else{color},
                          labels= if(post_comp == TRUE){c(names(choice_vector_bench)[choice_vector_bench == var], "2")}else{
                            names(choice_vector_bench)[choice_vector_bench == var]}) +
        theme(legend.title = element_blank())


    } else{
      p <- ggplot() +
        geom_bar(data = benchmark_data[!is.na(get(var))], aes(as.character(get(var))), fill = color) +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var])

    }
  )

  p <- plotly::ggplotly(p, dynamicTicks = TRUE)

  if(post_comp == TRUE){
    p$x$data[[2]]$name <- names(choice_vector_bench)[choice_vector_bench == var]
    p$x$data[[1]]$name <- "Peak not Found"
    p$x$data[[2]]$legendgroup <- var
    p$x$data[[1]]$legendgroup <- "Peak not Found"
  } else {
    p$x$data[[1]]$name <- names(choice_vector_bench)[choice_vector_bench == var]
  }

  if(rm_NF_legend == TRUE){
    p$x$data[[1]]$showlegend <- F
  }



  return(p)
}
