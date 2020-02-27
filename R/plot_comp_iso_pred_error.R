#' plot_comp_iso_pred_error
#'
#' @param comparison_data
#'
#' @return
#' @export
#'
#' @examples
plot_comp_iso_pred_error <- function(comparison_data) {



  p <- ggplot(comparison_data$iso_err_dt[isoabb_b < 100]) +
    suppressWarnings( geom_line(suppressWarnings( aes(x = data_type,
                                                      y = Pred_error,
                                                      group = paste(grp_col, diffH20PP),
                                                      color = diffH20PP,
                                                      molecule = molecule_b,
                                                      adduct = adduct_b,
                                                      isoabb = isoabb_b,
                                                      sample = sample_name_b,
                                                      #grp = grp_b,
                                                      diffH20PP = diffH20PP
    )), alpha = 0.3)) +
    scale_color_manual(name = "+ > 20%p", values=c("blue", "red")) +
    ggtitle("Quality of peak abundances") +
    labs(x = "", y = "IT pred error [%]") +
    theme(legend.title = element_blank())

  return(ggplotly(p, tooltip = c("molecule", "adduct", "isoabb", "sample", "Pred_error")))

}
