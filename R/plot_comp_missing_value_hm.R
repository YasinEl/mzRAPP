#' plot_comp_missing_value_hm
#'
#' @param comparison_data
#'
#' @return
#' @export
#'
#' @examples
plot_comp_missing_value_hm <- function(comparison_data, post_alignment = FALSE) {

  if(post_alignment == FALSE){
    hm_dt <- comparison_data$rs_table
  } else if(post_alignment == TRUE){

    #dt <-  rbindlist(list(comparison_data$ff_table), fill = TRUE)
    dt_n <- melt_fftable(comparison_data)

    dt_n <- dt_n[main_feature == TRUE]

    dt_n <- dt_n[!is.na(area_b)]
    hm_dt <-
      dt_n[, missing_peaks := find_r_s_error(
        area_b,
        area_g,
        area_b
      ), by = .(molecule_b, adduct_b, isoabb_b)]
    hm_dt$sample_id_b <- as.factor(hm_dt$sample_id_b)
    #tmp <- unique(data.table(sample_id_b = as.factor(ev_return_list[["c_table"]][["sample_id_b"]]),
    #                         sample_name_b = ev_return_list[["c_table"]][["sample_name_b"]]))
    #hm_dt <- hm_dt[tmp, on = .(sample_id_b)]


  } else {stop("Argument post_alignment must be TRUE or FALSE!")}



  hm_dt <- hm_dt[, overgroup := paste0(molecule_b, adduct_b)]
  hm_dt <- hm_dt[, if (any(missing_peaks != 'F')) .SD, by = .(molecule_b, adduct_b, isoabb_b)]
  if(nrow(hm_dt) == 0) {return(plotly::ggplotly(ggplot() + ggtitle("No missing peaks present")))}
  hm_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]
  hm_dt <- hm_dt[missing_peaks == "F", .(nr = .N), by = .(plot_group)][hm_dt, on =.(plot_group), nomatch = NA]
  hm_dt[is.na(nr)]$nr <- 0


  plot_r_s <- ggplot(
    hm_dt,
    aes(
      x = reorder(as.factor(plot_group), nr),
      y = as.factor(sample_id_b),
      fill = missing_peaks,
      molecule = molecule_b,
      #mz = mz_acc_b,
      isoabb = round(isoabb_b, 2),
      adduct = adduct_b,
      FileName = sample_name_b
      )
    ) +
    geom_tile() +
    scale_fill_manual(values=c("forestgreen", "firebrick", "royalblue4", "mediumpurple1")) +
    ggtitle("Missing peaks") +
    labs(x = "benchmark features", y = "sample IDs") +
    theme(legend.title = element_blank())
  return(plotly::ggplotly(plot_r_s,tooltip = c("molecule", "adduct", "isoabb", "FileName")#, "mz")

  ))


}
