#' check_IR_biases
#'
#' @param Matches_BM_NPPpeaks Matches_BM_NPPpeaks
#' @param Matches_BM_NPPpeaks_NPPfeatures Matches_BM_NPPpeaks_NPPfeatures
#' @param g_table g_table
#' @param b_table b_table
#'
#' @keywords internal
#'

check_IR_biases <- function(Matches_BM_NPPpeaks,
                            Matches_BM_NPPpeaks_NPPfeatures,
                            g_table,
                            b_table){

  if(nrow(g_table) > 0){

    IT_ratio_biases <- Matches_BM_NPPpeaks_NPPfeatures

    IT_ratio_biases <- IT_ratio_biases[b_table[, c("molecule_b",
                                                   "adduct_b",
                                                   "isoab_b",
                                                   "sample_name_b",
                                                   "peaks.rt_neighbors_b",
                                                   "peaks.mz_neighbors_b")], on = .(molecule_b, adduct_b, isoab_b, sample_name_b)]


  } else {
    Matches_BM_NPPpeaks[, sample_id_b := as.factor(sample_id_b)]
    Matches_BM_NPPpeaks[, area_g := as.numeric(NA)]
    IT_ratio_biases <- Matches_BM_NPPpeaks
  }

  IT_ratio_biases <- IT_ratio_biases[isoab_b != 100][IT_ratio_biases[isoab_b == 100,
                                                                     c("sample_id_b", "sample_name_b", "molecule_b", "adduct_b", "area_g", "peak_area_b", "peak_area_ug", "peaks.rt_neighbors_b", "peaks.mz_neighbors_b")],
                                                     on=.(sample_name_b, molecule_b, adduct_b),
                                                     nomatch = NA, allow.cartesian=TRUE][,c("benchmark",
                                                                                            "NPP_peak picking",
                                                                                            "NPP_features",
                                                                                            "RT_neighbors",
                                                                                            "mz_neighbors") := .((peak_area_b / ((i.peak_area_b * isoab_b) / 100) - 1) * 100,
                                                                                                                 (peak_area_ug / ((i.peak_area_ug * isoab_b) / 100) - 1) * 100,
                                                                                                                 (area_g / ((i.area_g * isoab_b) / 100) - 1) * 100,
                                                                                                                 paste0(paste0(i.peaks.rt_neighbors_b, " | "), peaks.rt_neighbors_b),
                                                                                                                 paste0(paste0(i.peaks.mz_neighbors_b, " | "), peaks.mz_neighbors_b))]


  IT_ratio_biases[, diffH20PP_pp := as.character(
    abs(abs(benchmark) - abs(`NPP_peak picking`)) > 10 &
      abs(`NPP_peak picking` - benchmark) > 20 &
      abs(`NPP_peak picking`) > 30)]

  IT_ratio_biases[, diffH20PP_ft := as.character(abs(abs(benchmark) - abs(NPP_features)) > 10 &
                                                   abs(NPP_features - benchmark) > 20 &
                                                   abs(NPP_features) > 30)]

  IT_ratio_biases[diffH20PP_pp == "TRUE"]$diffH20PP_pp <- "Inc. > 20%p"
  IT_ratio_biases[diffH20PP_pp == "FALSE"]$diffH20PP_pp <- "Inc. < 20%p"

  IT_ratio_biases[diffH20PP_ft == "TRUE"]$diffH20PP_ft <- "Inc. > 20%p"
  IT_ratio_biases[diffH20PP_ft == "FALSE"]$diffH20PP_ft <- "Inc. < 20%p"

  IT_ratio_biases <- IT_ratio_biases[!is.na(peak_area_b)]

  return(IT_ratio_biases)

}
