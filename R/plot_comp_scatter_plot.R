#' plot_comp_scatter_plot
#'
#' @param comparison_data
#' @param plot_input_x
#' @param plot_input_y
#'
#' @return
#' @export
#'
#' @examples
plot_comp_scatter_plot <- function(comparison_data, x, y, col, choice_vector_comp, post_alignment = FALSE){


  if(post_alignment == TRUE){
  feat_t <- melt_fftable(comparison_data)

  BM_bu <- rbindlist(list(comparison_data$c_table[main_peak == TRUE], comparison_data$nf_b_table), fill = TRUE)

  BM_bu$sample_id_b <- as.factor(BM_bu$sample_id_b)

  feat_t <- feat_t[main_feature == TRUE]

  vct <- colnames(BM_bu)[grepl("_b", colnames(BM_bu))]

  f_nf_dt <- feat_t[!is.na(area_b) &
                     main_feature == TRUE, c("molecule_b",
                                             "adduct_b",
                                             "isoabb_b",
                                             "sample_id_b",
                                             "area_g")][BM_bu[,..vct], on = .(molecule_b,
                                                                              adduct_b,
                                                                              isoabb_b,
                                                                              sample_id_b)]

  f_nf_dt <- f_nf_dt[, f_nf_col := ifelse(!is.na(area_g), 'TRUE', 'FALSE')]


} else if(post_alignment == FALSE){


  f_nf_dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)

  f_nf_dt <- f_nf_dt[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

}


  f_nf_dt <- suppressWarnings(f_nf_dt[order(as.numeric(f_nf_dt$f_nf_col), decreasing = TRUE),])




    p <- ggplot()

    if(col == "F/NF"){

      suppressWarnings(
      p <- p +
      geom_point(data = f_nf_dt[f_nf_col == TRUE], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                         y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                         col = "F",
                                                         molecule = molecule_b,
                                                         adduct = adduct_b,
                                                         isoabb = round(isoabb_b, 2),
                                                         sample_name = sample_name_b),
                 color = "blue") +

      geom_point(data = f_nf_dt[f_nf_col == FALSE], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                          y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                          col = "NF",
                                                          molecule = molecule_b,
                                                          adduct = adduct_b,
                                                          isoabb = round(isoabb_b, 2),
                                                          sample_name = sample_name_b),
                 color = "red")
      )
    } else {
      suppressWarnings(
      p <- p +
        geom_point(data = f_nf_dt, aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                         y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                         col = get(col),
                                                         molecule = molecule_b,
                                                         adduct = adduct_b,
                                                         isoabb = round(isoabb_b, 2),
                                                         sample_name = sample_name_b)
                   )
      )

    }

    p <- p +
      labs(x = if(x != "peak_height_b" & x != "peak_area_b") {names(choice_vector_comp)[choice_vector_comp == x]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == x], ")")},
           y = if(y != "peak_height_b" & y != "peak_area_b") {names(choice_vector_comp)[choice_vector_comp == y]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == y], ")")}) +
      ggtitle("Overview of found/not found peaks and their variables")

  p <- plotly::ggplotly(p, tooltip = c(molecule = "molecule", "adduct", "isoabb", "sample_name"), dynamicTicks = TRUE)
  return(p)
}
