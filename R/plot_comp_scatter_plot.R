#' plot_comp_scatter_plot
#'
#' @param x variable (column name) to be plotted on x axis
#' @param y variable (column name) to be plotted on y axis
#' @param col variable (column name) to be colored by
#' @param choice_vector_comp named vector including all variables used as elements
#' @param post_alignment TRUE/FALSE should NT data from before or after alignment be plotted
#' @param comparison_data output from \code{\link{compare_peaks}}
#'
#'
#' @importFrom ggplot2 ggplot geom_line aes geom_point geom_vline theme labs annotate
#' scale_fill_manual ggtitle scale_colour_manual theme_classic geom_histogram element_blank xlab
#' @return plotly object
#' @export
#'
plot_comp_scatter_plot <- function(comparison_data, x, y, col, choice_vector_comp, post_alignment = FALSE){
  if(missing(x) | missing(y) | missing (comparison_data)) return(plotly::ggplotly(ggplot() +
                                                                                    ggtitle("Missing arguments")))

  if(post_alignment == TRUE){

    feat_t <- comparison_data[["Matches_BM_NPPpeaks_NPPfeatures"]][main_feature == TRUE]

    feat_t <- feat_t[main_feature == TRUE & !is.na(area_b)]

    BM_bu <- data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE], comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)

    BM_bu$sample_id_b <- as.factor(BM_bu$sample_id_b)


    vct <- colnames(BM_bu)[grepl("_b", colnames(BM_bu))]

    f_nf_dt <- feat_t[!is.na(area_b) &
                        main_feature == TRUE, c("molecule_b",
                                                "adduct_b",
                                                "isoab_b",
                                                "sample_id_b",
                                                "area_g")][BM_bu[,..vct], on = .(molecule_b,
                                                                                 adduct_b,
                                                                                 isoab_b,
                                                                                 sample_id_b)]

    f_nf_dt <- f_nf_dt[, NPP_status := ifelse(!is.na(area_g), 'Found', 'Not Found')]


  } else if(post_alignment == FALSE){


    f_nf_dt <-  data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE][, Split_peak := FALSE], comparison_data$SplittedMatches_BM_NPPpeaks[present_in_found == FALSE][, Split_peak := TRUE], comparison_data$Unmatched_BM_NPPpeaks[, Split_peak := FALSE]), fill = TRUE)

    f_nf_dt <- f_nf_dt[, NPP_status := ifelse(!is.na(peak_area_ug), ifelse(Split_peak == "TRUE", 'Split', 'Found'), 'Not Found')]

    f_nf_dt <- unique(f_nf_dt, by = c("molecule_b", "adduct_b", "isoab_b", "sample_name_b"))

  }



  f_nf_dt <- suppressWarnings(f_nf_dt[order(as.numeric(f_nf_dt$NPP_status), decreasing = FALSE),])




  p <- ggplot()

  if(col == "F/NF"){

    suppressWarnings(
      p <- p +
        geom_point(data = f_nf_dt[NPP_status == 'Found'], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                            y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                            col = "F",
                                                            NPP_status = NPP_status,
                                                            molecule = molecule_b,
                                                            adduct = adduct_b,
                                                            isoab = round(isoab_b, 2),
                                                            mz_b = mz_b,
                                                            rt_b = rt_b,
                                                            sample_name = sample_name_b,
                                                            key = comp_id_b),
                   color = "#82e0aa", show.legend = T) +
        theme_classic() +

        geom_point(data = f_nf_dt[NPP_status == 'Not Found'], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                                y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                                col = "NF",
                                                                NPP_status = NPP_status,
                                                                molecule = molecule_b,
                                                                adduct = adduct_b,
                                                                isoab = round(isoab_b, 2),
                                                                mz_b = mz_b,
                                                                rt_b = rt_b,
                                                                sample_name = sample_name_b,
                                                                key = comp_id_b),
                   color = "#ccd1d1", show.legend = T) +

        geom_point(data = f_nf_dt[NPP_status == 'Split'], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                            y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                            col = "S",
                                                            NPP_status = NPP_status,
                                                            molecule = molecule_b,
                                                            adduct = adduct_b,
                                                            isoab = round(isoab_b, 2),
                                                            sample_name = sample_name_b,
                                                            mz_b = mz_b,
                                                            rt_b = rt_b,
                                                            key = comp_id_b),
                   color = "goldenrod2", show.legend = T)
    )
  } else {
    suppressWarnings(
      p <- p +
        geom_point(data = f_nf_dt, aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                       y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                       col = get(col),
                                       NPP_status = NPP_status,
                                       molecule = molecule_b,
                                       adduct = adduct_b,
                                       isoab = round(isoab_b, 2),
                                       sample_name = sample_name_b,
                                       mz_b = mz_b,
                                       rt_b = rt_b,
                                       key = comp_id_b)
        )+labs(col=if(col != "peak_height_b_off" & x != "peak_area_b_off") {names(choice_vector_comp)[choice_vector_comp == col]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == col], ")")})
    )

  }

  p <- p +
    labs(x = if(x != "peak_height_b_off" & x != "peak_area_b_off") {names(choice_vector_comp)[choice_vector_comp == x]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == x], ")")},
         y = if(y != "peak_height_b_off" & y != "peak_area_b_off") {names(choice_vector_comp)[choice_vector_comp == y]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == y], ")")}) +
    ggtitle("Overview of found/not found peaks against benchmark variables")

  p <- plotly::ggplotly(p, tooltip = c("NPP_status", "molecule", "adduct", "isoab", "mz_b", "rt_b", "sample_name"), dynamicTicks = TRUE, source = "scatter")
  return(p)
}

