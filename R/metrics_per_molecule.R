#' metrics_per_molecule
#'
#' @param Matches_BM_NPPpeaks Matches_BM_NPPpeaks
#' @param Unmatched_BM_NPPpeaks Unmatched_BM_NPPpeaks
#' @param Matches_BM_NPPpeaks_NPPfeatures Matches_BM_NPPpeaks_NPPfeatures
#' @param IT_ratio_biases IT_ratio_biases
#' @param SplittedMatches_BM_NPPpeaks SplittedMatches_BM_NPPpeaks
#' @param MissingPeak_classification MissingPeak_classification
#' @param AlignmentErrors_per_moleculeAndAdduct AlignmentErrors_per_moleculeAndAdduct
#'
#' @keywords internal
#'

metrics_per_molecule <- function(Matches_BM_NPPpeaks,
                                 Unmatched_BM_NPPpeaks,
                                 Matches_BM_NPPpeaks_NPPfeatures,
                                 IT_ratio_biases,
                                 SplittedMatches_BM_NPPpeaks,
                                 MissingPeak_classification,
                                 AlignmentErrors_per_moleculeAndAdduct){

  bm_tab <- data.table::rbindlist(list(Matches_BM_NPPpeaks[main_peak == TRUE], Unmatched_BM_NPPpeaks), fill = TRUE, use.names = TRUE)
  bm_tab[is.na(main_peak), main_peak := FALSE]


  peaks_pp <- bm_tab[,.(Found_peaks_pp = sum(main_peak),
                        Not_Found_peaks_pp = length(main_peak) - sum(main_peak)), by = .(molecule_b)]

  peaks_ft <- Matches_BM_NPPpeaks_NPPfeatures[!is.na(peak_area_b) & (is.na(area_g) | main_feature == TRUE), .(Found_peaks_ft = sum(!is.na(area_g)),
                                                                                                              Not_Found_peaks_ft = sum(is.na(area_g))),
                                              by = .(molecule_b)]

  IRb <- IT_ratio_biases[, c("molecule_b", "diffH20PP_pp", "diffH20PP_ft")][, .(IRb_ok_pp = sum(diffH20PP_pp == "Inc. < 20%p", na.rm = TRUE),
                                                                                IRb_off_pp = sum(diffH20PP_pp == "Inc. > 20%p", na.rm = TRUE),
                                                                                IRb_ok_ft = sum(diffH20PP_ft == "Inc. < 20%p", na.rm = TRUE),
                                                                                IRb_off_ft = sum(diffH20PP_ft == "Inc. > 20%p", na.rm = TRUE)),
                                                                            by = .(molecule_b)]

  split_pp <- SplittedMatches_BM_NPPpeaks[,c("molecule_b")][,.( Split_peaks = .N), by = .(molecule_b)]

  mw_tab <- MissingPeak_classification[, .(R_pp = sum(missing_peaks_ug == "R", na.rm = TRUE),
                                           S_pp = sum(missing_peaks_ug == "S", na.rm = TRUE),
                                           R_ft = sum(missing_peaks_g == "R", na.rm = TRUE),
                                           S_ft = sum(missing_peaks_g == "S", na.rm = TRUE)),
                                       by = .(molecule_b)]


  ali_tab <- AlignmentErrors_per_moleculeAndAdduct[, .(Min.er = sum(Min.errors, na.rm = TRUE),
                                                       BM.div = sum(BM.div, na.rm = TRUE),
                                                       lost = sum(Lost_b.A, na.rm = TRUE)),
                                                   by = .(Molecule)]

  colnames(ali_tab)[1] <- "molecule_b"

  sum_tab <- unique(bm_tab[!is.na(molecule_b), "molecule_b"])
  sum_tab <- peaks_pp[sum_tab, on = .(molecule_b)]
  sum_tab <- peaks_ft[sum_tab, on = .(molecule_b)]
  sum_tab <- split_pp[sum_tab, on = .(molecule_b)]
  sum_tab <- IRb[sum_tab, on = .(molecule_b)]
  sum_tab <- mw_tab[sum_tab, on = .(molecule_b)]
  sum_tab <- ali_tab[sum_tab, on = .(molecule_b)]

  data.table::setnafill(sum_tab, fill=0, cols = colnames(sum_tab)[-1])


  return(sum_tab)


}
