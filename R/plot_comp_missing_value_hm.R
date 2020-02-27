#' plot_comp_missing_value_hm
#'
#' @param comparison_data
#'
#' @return
#' @export
#'
#' @examples
plot_comp_missing_value_hm <- function(comparison_data) {

  hm_dt <- comparison_data$rs_table



  #######RECHECK!!!!! TRUE TRUE?
  hm_dt <- (hm_dt[, main_feature_check := ifelse((length(unique(na.omit(feature_id_g))) == 1) &
                                                   (isoabb_b == 100), 'TRUE', 'TRUE'),
                  by = .(molecule_b, adduct_b, isoabb_b)])
  hm_dt <- hm_dt[, overgroup := paste0(molecule_b, adduct_b)]
  hm_dt <- hm_dt[, if (any(missing_peaks != 'F')) .SD, by = .(molecule_b, adduct_b, isoabb_b)]
  hm_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]


  plot_r_s <- ggplot(
    hm_dt,
    aes(
      x = as.character(plot_group),
      y = sample_id_b,
      fill = missing_peaks,
      molecule = molecule_b,
      mz = mz_acc_b,
      isoabb = isoabb_b,
      adduct = adduct_b,
      FileName = sample_name_b
    )
  ) +
    geom_tile() +
    scale_fill_manual(values=c("forestgreen", "firebrick", "royalblue4", "mediumpurple1")) +
    ggtitle("Missing peaks") +
    labs(x = "benchmark features", y = "sample IDs", fill = "b_peaks") +
    theme(legend.title = element_blank())

  return(plotly::ggplotly(plot_r_s,tooltip = c("molecule", "adduct", "isoabb", "FileName", "mz")

    ))

}
