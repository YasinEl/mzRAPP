#' plot_comp_missing_value_hm
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#' @param post_alignment TRUE/FALSE should NT data from before or after alignment be plotted
#' @param disable_plot if TRUE plot is not generated (for shiny app due to long loading times)
#'
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme labs annotate scale_fill_manual ggtitle scale_colour_manual theme_classic geom_histogram element_blank xlab
#' @return plotly object
#' @export
#'
plot_comp_missing_value_hm <- function(comparison_data, post_alignment = FALSE, disable_plot = FALSE) {

  if(missing (comparison_data) | disable_plot == TRUE) return(plotly::ggplotly(ggplot() + ggtitle("Disabeled")))

  hm_dt <- comparison_data$MissingPeak_classification
  hm_dt <- hm_dt[, missing_peaks := missing_peaks_ug]

  if(post_alignment == FALSE){
    hm_dt <- hm_dt[, missing_peaks := missing_peaks_ug]

  } else if(post_alignment == TRUE){
    hm_dt <- hm_dt[, missing_peaks := missing_peaks_g]

  } else {stop("Argument post_alignment must be TRUE or FALSE!")}

  hm_dt <- hm_dt[, if (any(missing_peaks != 'F')) .SD, by = .(molecule_b, adduct_b, isoab_b)]
  if(nrow(hm_dt) == 0) {return(plotly::ggplotly(ggplot() + ggtitle("No missing peaks present")))}
  hm_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoab_b)]
  hm_dt <- hm_dt[missing_peaks == "F", .(nr = .N), by = .(plot_group)][hm_dt, on =.(plot_group), nomatch = NA]
  hm_dt[is.na(nr)]$nr <- 0


  hm_dt$ord <- as.integer(hm_dt$sample_id_b)
  hm_dt$sample_id_b <- as.integer(hm_dt$sample_id_b)
  hm_dt <- hm_dt[, c("molecule_b", "adduct_b", "isoab_b", "sample_name_b", "plot_group", "sample_id_b", "missing_peaks", "nr", "ord")]

  if(post_alignment == TRUE){

  } else{

  }

  hm_dt[missing_peaks == "F", NPP_status := "Found"]
  hm_dt[missing_peaks == "R", NPP_status := "High NA"]
  hm_dt[missing_peaks == "S", NPP_status := "Low NA"]
  hm_dt[missing_peaks == "L", NPP_status := "feature missing"]
  hm_dt[missing_peaks == "NC", NPP_status := "not confirmable"]


  plot_r_s <- ggplot(
    hm_dt,
    aes(
      x = stats::reorder(as.factor(plot_group), nr),
      y = stats::reorder(as.factor(sample_name_b), ord),
      fill = NPP_status,
      molecule = molecule_b,
      #mz = mz_acc_b,
      isoab = round(isoab_b, 2),
      adduct = adduct_b,
      FileName = sample_name_b
      )
    ) +
    theme_classic() +
    geom_tile() +
    scale_fill_manual(values=c(`Found` = "#82e0aa", `High NA` = "red", `Low NA` ="goldenrod2", `feature missing` = "lightpink2", `not confirmable` = "grey76")) +
    ggtitle("Missing values") +
    labs(x = "benchmark features", y = "samples") +
    theme(legend.title = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    return(plotly::ggplotly(plot_r_s,tooltip = c("NPP_status", "molecule", "adduct", "isoab", "FileName")#, "mz")

  ))


}
