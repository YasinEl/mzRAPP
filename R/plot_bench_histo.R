
#' plot_bench_histo
#'
#' @param benchmark_data output from \code{\link{find_bench_peaks}}
#' @param var variable name to be plotted
#' @param choice_vector_bench named vector including variable to be plotted as element
#' @param color color of histogram
#' @param post_comp TRUE/FALSE are data from benchmark or comparison with non-targeted
#' @param rm_NF_legend for shiny functionality
#'
#'
#' @importFrom ggplot2 ggplot geom_line aes geom_point geom_vline theme labs annotate
#' scale_fill_manual ggtitle scale_colour_manual theme_classic geom_histogram element_blank xlab geom_bar
#' @return plotly object
#' @export
#'
plot_bench_histo <- function(benchmark_data, var, choice_vector_bench, color = "blue", post_comp = FALSE, rm_NF_legend = FALSE){

  if(post_comp == FALSE){
    if(is.data.table(benchmark_data) == FALSE){
      benchmark_data <- benchmark_data$PCal
    }
  } else if (post_comp == TRUE){
    dtf <- benchmark_data$Matches_BM_NPPpeaks_NPPfeatures
    benchmark_data <- data.table::rbindlist(list(benchmark_data$Matches_BM_NPPpeaks, benchmark_data$Unmatched_BM_NPPpeaks), fill = TRUE, use.names = TRUE)
    benchmark_data <- benchmark_data[main_peak == TRUE |
                                       is.na(peak_area_ug)]

    benchmark_data <- dtf[!is.na(area_b)][benchmark_data, on = .(molecule_b, adduct_b, isoab_b, sample_name_b), nomatch = NA]
    benchmark_data[, peak_found := FALSE]
    benchmark_data[!is.na(area_g), peak_found := TRUE]
  }

  suppressWarnings(
    if(!(var %in% c("molecule", "FileName", "Grp", "adduct", "molecule_b", "sample_name_b", "Grp_b", "adduct_b"))){

      if(var == "peak_height_b" |
         var == "peak_area_b" |
         var == "peaks.area" |
         var == "peaks.height" |
         var == "ExpectedArea" |
         var == "ExpectedArea_b" |
         var == "ExpectedHeight" |
         var == "ErrorAbs_A" |
         var == "ErrorAbs_H" |
         var == "ErrorAbs_A_b" |
         var == "ErrorAbs_H_b" |
         var == "ExpectedHeight_b"){
        benchmark_data[, eval(quote(var)) := log10(get(var))]
        if(var != "peak_height_b" & var != "peak_area_b"){
          names(choice_vector_bench)[choice_vector_bench == var] <- paste0("log10(",names(choice_vector_bench)[choice_vector_bench == var], ")")
        }
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
