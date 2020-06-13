#' plot_comp_missing_value_hm
#'
#' @param comparison_data
#' @param post_alignment
#' @param disable_plot
#'
#' @return
#' @export
#'
#' @examples
plot_comp_missing_value_hm <- function(comparison_data, post_alignment = FALSE, disable_plot = FALSE) {

  if(missing (comparison_data) | disable_plot == TRUE) return(plotly::ggplotly(ggplot() + ggtitle("Disabeled")))

  hm_dt <- comparison_data$rs_table
  hm_dt <- hm_dt[, missing_peaks := missing_peaks_ug]

  if(post_alignment == FALSE){
    hm_dt <- hm_dt[, missing_peaks := missing_peaks_ug]

  } else if(post_alignment == TRUE){
    hm_dt <- hm_dt[, missing_peaks := missing_peaks_g]
    #dt <-  rbindlist(list(comparison_data$ff_table), fill = TRUE)
    #feat_t <- melt_fftable(comparison_data)

    #BM_bu <- rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)

    #BM_bu$sample_id_b <- as.factor(BM_bu$sample_id_b)

    #feat_t <- feat_t[main_feature == TRUE]


    #feat_t <- feat_t[!is.na(area_b) &
    #                      main_feature == TRUE][unique(na.omit(BM_bu[,c("molecule_b",
    #                                                                "isoab_b",
    #                                                                "adduct_b",
    #                                                                "sample_id_b",
    #                                                                "peak_area_b")])), on = .(molecule_b, adduct_b, isoab_b, sample_id_b)]
#



    #test_feat <<- feat_t
 #   feat_t <-
 #     feat_t[, Connected := File_con_test(
 #       sample_id_b,
 #       feature_id_g),
 #       by = .(molecule_b, adduct_b)]


    #feat_t <- feat_t[!is.na(area_b)]
#    hm_dt <-
#      feat_t[, missing_peaks := find_r_s_error(
#        peak_area_b,
#        area_g,
#        peak_area_b,
#        Connected
#      ), by = .(molecule_b, adduct_b, isoab_b)]
#    hm_dt$sample_id_b <- as.integer(hm_dt$sample_id_b)
    #tmp <- unique(data.table(sample_id_b = as.factor(ev_return_list[["c_table"]][["sample_id_b"]]),
    #                         sample_name_b = ev_return_list[["c_table"]][["sample_name_b"]]))
    #hm_dt <- hm_dt[tmp, on = .(sample_id_b)]


  } else {stop("Argument post_alignment must be TRUE or FALSE!")}



  #hm_dt <- hm_dt[, overgroup := paste0(molecule_b, adduct_b)]

  hm_dt <- hm_dt[, if (any(missing_peaks != 'F')) .SD, by = .(molecule_b, adduct_b, isoab_b)]
  if(nrow(hm_dt) == 0) {return(plotly::ggplotly(ggplot() + ggtitle("No missing peaks present")))}
  hm_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoab_b)]
  hm_dt <- hm_dt[missing_peaks == "F", .(nr = .N), by = .(plot_group)][hm_dt, on =.(plot_group), nomatch = NA]
  hm_dt[is.na(nr)]$nr <- 0


  hm_dt$ord <- as.integer(hm_dt$sample_id_b)
  hm_dt$sample_id_b <- as.integer(hm_dt$sample_id_b)
#hm_dt_test <<- hm_dt
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
      x = reorder(as.factor(plot_group), nr),
      y = reorder(as.factor(sample_name_b), ord),
      fill = NPP_status,
      molecule = molecule_b,
      #mz = mz_acc_b,
      isoab = round(isoab_b, 2),
      adduct = adduct_b,
      FileName = sample_name_b
      )
    ) +
    geom_tile() +
    #scale_fill_manual(values=c(`F` = "forestgreen", `L` = "firebrick", `R` = "royalblue4", `S` ="mediumpurple1", `NC` = "orange")) +
    #scale_fill_manual(values=c(`F` = "blue", `R` = "red", `S` ="goldenrod2", `L` = "lightpink2", `NC` = "grey76")) +
    #scale_fill_discrete(labels=c(`F` = "Found", `R` = "High NA", `S` ="Low NA", `L` = "feature missing", `NC` = "not confirmable")) +
    #scale_color_manual(values=c(`F` = "blue", `R` = "red", `S` ="goldenrod2", `L` = "lightpink2", `NC` = "grey76")) +
    scale_fill_manual(values=c(`Found` = "blue", `High NA` = "red", `Low NA` ="goldenrod2", `feature missing` = "lightpink2", `not confirmable` = "grey76")) +
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
