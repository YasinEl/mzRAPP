#' plot_comp_iso_pred_error
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#' @param post_alignment TRUE/FALSE should NT data from before or after alignment be plotted
#' @param BMvsPPvsAl TRUE/FALSE should argument post_alignment be ignored in order to plot both in one plot
#'
#' @importFrom ggplot2 ggplot geom_line aes geom_point geom_vline theme labs annotate
#' scale_fill_manual ggtitle scale_colour_manual theme_classic geom_histogram element_blank xlab scale_color_manual
#' @return plotly object
#' @export
#'
plot_comp_iso_pred_error <- function(comparison_data, post_alignment = FALSE, BMvsPPvsAl = TRUE) {

  IT_ratio_biases <- comparison_data$IT_ratio_biases

  if(nrow(IT_ratio_biases[!is.na(NPP_features)]) == 0) {
    BMvsPPvsAl <- FALSE
    }

  if(BMvsPPvsAl == FALSE){

  if(post_alignment == FALSE){
    IT_ratio_biases$diffH20PP <- IT_ratio_biases$diffH20PP_pp

    IT_ratio_biases <-
      data.table::melt(
        IT_ratio_biases,
        id.vars = c('molecule_b', 'adduct_b', 'Grp_b', 'isoab_b', 'sample_name_b', 'diffH20PP'),
        measure.vars = c("benchmark", "NPP_peak picking"),
        variable.name = 'data_type',
        value.name = 'Pred_error'
      )


  } else if(post_alignment == TRUE) {
    IT_ratio_biases$diffH20PP <- IT_ratio_biases$diffH20PP_ft

    IT_ratio_biases <-
      data.table::melt(
        IT_ratio_biases,
        id.vars = c('molecule_b', 'adduct_b', 'Grp_b', 'isoab_b', 'sample_name_b', 'diffH20PP'),
        measure.vars = c("benchmark", "NPP_features"),
        variable.name = 'data_type',
        value.name = 'Pred_error'
      )


  } else {stop("Argument post_alignment must be TRUE or FALSE!")}

  }else if(BMvsPPvsAl == TRUE){

    IT_ratio_biases$diffH20PP <- IT_ratio_biases$diffH20PP_ft
    IT_ratio_biases[diffH20PP_pp == "Inc. < 20%p" & (diffH20PP_ft == "Inc. > 20%p"), diffH20PP := "Feature Inc. > 20%p"]

    IT_ratio_biases <-
      data.table::melt(
        IT_ratio_biases,
        id.vars = c('molecule_b', 'adduct_b', 'isoab_b', 'sample_name_b', 'RT_neighbors', 'mz_neighbors', 'diffH20PP'),
        measure.vars = c("benchmark", "NPP_peak picking", "NPP_features"),
        variable.name = 'data_type',
        value.name = 'Pred_error'
      )



  }

  IT_ratio_biases[, grp_col := paste(molecule_b, adduct_b, isoab_b, sample_name_b, sep = "_;_")]

  IT_ratio_biases <- stats::na.omit(IT_ratio_biases, cols = "Pred_error")


  p <- ggplot(IT_ratio_biases[isoab_b < 100]) +
    suppressWarnings( geom_line(suppressWarnings( aes(x = data_type,
                                                      y = Pred_error,
                                                      group = paste(grp_col, diffH20PP),
                                                      color = diffH20PP,
                                                      molecule = molecule_b,
                                                      adduct = adduct_b,
                                                      isoab = isoab_b,
                                                      sample = sample_name_b,
                                                      RT_neighbors = RT_neighbors,
                                                      mz_neighbors = mz_neighbors,
                                                      diffH20PP = diffH20PP,
                                                      key = grp_col
    )), alpha = 0.3)) +
    theme_classic() +
    scale_color_manual(name = "+ > 20%p", values=c(`Inc. < 20%p` = "#82e0aa", `Inc. > 20%p` = "#ed7467", `Feature Inc. > 20%p` = "goldenrod2")) +
    ggtitle("Relative IT ratio bias") +
    labs(x = "", y = "IT ratio bias [%]") +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')


  return(plotly::ggplotly(p, tooltip = c("molecule", "adduct", "isoab", "sample", "RT_neighbors", "mz_neighbors", "Pred_error"),
                          dynamicTicks = "y",
                          source = "IRbias")  %>% plotly::layout(legend = list(orientation = "h", x = -0.05, y =-0.1)))

}
