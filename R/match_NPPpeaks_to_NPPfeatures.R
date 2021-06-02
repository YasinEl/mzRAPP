#' match_NPPpeaks_to_NPPfeatures
#'
#' @param Matches_BM_NPPpeaks Matches_BM_NPPpeaks
#' @param SplittedMatches_BM_NPPpeaks SplittedMatches_BM_NPPpeaks
#' @param g_table g_table
#'
#' @keywords internal

match_NPPpeaks_to_NPPfeatures <- function(Matches_BM_NPPpeaks,
                              SplittedMatches_BM_NPPpeaks,
                              g_table){


  match_tables_with_alignment_recovery_info <- list()
  length(match_tables_with_alignment_recovery_info) <- 2
  names(match_tables_with_alignment_recovery_info) <- c("Matches_BM_NPPpeaks", "SplittedMatches_BM_NPPpeaks")
  g_table[, peak_area_g := as.numeric(peak_area_g)]

  #Creating temp columns to prevent over-writing by join
  #If statement is solution for msdial

  if ('peak_area_rounded_ug' %in% colnames(Matches_BM_NPPpeaks)){
    Matches_BM_NPPpeaks[, peak_area_ug_temp := peak_area_rounded_ug]
    Matches_BM_NPPpeaks[, sample_id_b_temp := sample_id_b]
    SplittedMatches_BM_NPPpeaks[, peak_area_ug_temp := peak_area_rounded_ug]
    g_table[, peak_area_g_temp := peak_area_g]
    g_table[, sample_id_g_temp := sample_id_g]
  } else {
    Matches_BM_NPPpeaks[, peak_area_ug_temp := round(peak_area_ug,2)]
    Matches_BM_NPPpeaks[, sample_id_b_temp := sample_id_b]
    SplittedMatches_BM_NPPpeaks[, peak_area_ug_temp := round(peak_area_ug,2)]
    g_table[, peak_area_g_temp := round(peak_area_g,2)]
    g_table[, sample_id_g_temp := sample_id_g]
  }

  #Join
  Matches_BM_NPPpeaks <- g_table[Matches_BM_NPPpeaks, on=.(peak_area_g_temp == peak_area_ug_temp, sample_id_g_temp == sample_id_b_temp),
                                 allow.cartesian = TRUE, nomatch=NA, mult='all']


  #In case of duplicate area matches during ug - g match take the g-match which occurred most often for other peaks in the same BM feature
  Matches_BM_NPPpeaks[, N_fid := .N, by = .(molecule_b, adduct_b, isoab_b, feature_id_g)]
  Matches_BM_NPPpeaks <-  Matches_BM_NPPpeaks[order(-rank(N_fid))][, !"N_fid"]
  Matches_BM_NPPpeaks <- unique(Matches_BM_NPPpeaks, by = c("molecule_b", "adduct_b", "isoab_b", "peak_area_g_temp", "sample_id_b"))



  #Replace 0 in peak_area_g with NA (no idea why they appear in the first place)(maybe int64?)

  SplittedMatches_BM_NPPpeaks <- g_table[SplittedMatches_BM_NPPpeaks, on=.(peak_area_g_temp == peak_area_ug_temp),
                                         allow.cartesian = TRUE, nomatch=NA, mult='all']

  #Remove _temp Columns
  suppressWarnings(Matches_BM_NPPpeaks[,grep('_temp$', colnames(Matches_BM_NPPpeaks)):=NULL])


  Matches_BM_NPPpeaks[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]
  SplittedMatches_BM_NPPpeaks[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]

  SplittedMatches_BM_NPPpeaks[, present_in_found := ifelse(id_b_ug %in% Matches_BM_NPPpeaks$id_b_ug, 'TRUE', 'FALSE')]


  match_tables_with_alignment_recovery_info[["Matches_BM_NPPpeaks"]] <- Matches_BM_NPPpeaks
  match_tables_with_alignment_recovery_info[["SplittedMatches_BM_NPPpeaks"]] <- SplittedMatches_BM_NPPpeaks


  #Make sure main peaks only occur once
  if (any(duplicated(Matches_BM_NPPpeaks[main_peak==TRUE]))){
    stop('Duplicate Peaks still present after analysis')
  }


  return(match_tables_with_alignment_recovery_info)

}
