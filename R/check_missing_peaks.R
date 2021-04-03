#' check_missing_peaks
#'
#' @param Matches_BM_NPPpeaks Matches_BM_NPPpeaks
#' @param Unmatched_BM_NPPpeaks Unmatched_BM_NPPpeaks
#' @param g_table g_table
#' @param Matches_BM_NPPpeaks_NPPfeatures Matches_BM_NPPpeaks_NPPfeatures
#'
#' @keywords internal
#'

check_missing_peaks <- function(Matches_BM_NPPpeaks,
                                Unmatched_BM_NPPpeaks,
                                Matches_BM_NPPpeaks_NPPfeatures,
                                g_table){

  MissingPeak_classification <- data.table::rbindlist(list(Matches_BM_NPPpeaks, Unmatched_BM_NPPpeaks), fill = TRUE)

  MissingPeak_classification <- MissingPeak_classification[, c("molecule_b", "adduct_b", "isoab_b", "sample_name_b", "peak_area_b", "peak_height_b",
                                                               "peak_area_ug", "peak_area_g", "feature_id_g", "sample_id_b"
  )]

  if(nrow(g_table) > 0){

    join_vct <- c("molecule_b",
                  "adduct_b",
                  "isoab_b",
                  "sample_name_b")

    join_on_dt <- unique(rbind(MissingPeak_classification[, ..join_vct],
                               Matches_BM_NPPpeaks_NPPfeatures[main_feature == TRUE &
                                                                 !is.na(peak_area_b),
                                                               ..join_vct]))


    MissingPeak_classification <- MissingPeak_classification[join_on_dt, on = .(molecule_b, adduct_b, isoab_b, sample_name_b)]

    MissingPeak_classification <- Matches_BM_NPPpeaks_NPPfeatures[main_feature == TRUE &
                                                                    !is.na(peak_area_b), c("molecule_b",
                                                                                           "adduct_b",
                                                                                           "isoab_b",
                                                                                           "sample_name_b",
                                                                                           "area_g")][MissingPeak_classification,
                                                                                                      on = .(molecule_b, adduct_b, isoab_b, sample_name_b)]


    MissingPeak_classification[, peak_area_g := area_g]

    MissingPeak_classification <- MissingPeak_classification[!is.na(peak_area_b)]
    MissingPeak_classification <- MissingPeak_classification[order(feature_id_g)]

    MissingPeak_classification <-
      MissingPeak_classification[, Connected := File_con_test(
        sample_name_b,
        feature_id_g),
        by = .(molecule_b, adduct_b)]

    colnames(MissingPeak_classification) <- replace(colnames(MissingPeak_classification), colnames(MissingPeak_classification) == "area_g", "peak_area_g")

  } else {

    MissingPeak_classification[, Connected := TRUE]
  }

  MissingPeak_classification <-
    MissingPeak_classification[, c("missing_peaks_ug", "missing_peaks_g") := .(find_r_s_error(
      peak_area_b,
      peak_area_ug,
      peak_height_b,
      Connected),
      find_r_s_error(
        peak_area_b,
        peak_area_g,
        peak_height_b,
        Connected)
    ), by = .(molecule_b, adduct_b, isoab_b)]



  MissingPeak_classification <- MissingPeak_classification[!is.na(peak_area_b)]

  return(MissingPeak_classification)

}
