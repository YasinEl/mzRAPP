#' clean_peak_assignments
#'
#' @param Input_table
#'
#' @return
#' @export
#'
#' @examples
clean_peak_assignments <- function(Input_table){

  Depl_table <- Input_table
  Depl_table$IDX <- seq.int(nrow(Depl_table))
  dpl_peaks <- hutils::duplicated_rows(Depl_table, by = c("peaks.rt.unsm",
                                                                     "peaks.height",
                                                                     "FileName"))

  if("user.rt" %in% colnames(Depl_table) & nrow(dpl_peaks) > 0){

    dpl_peaks$rt_diff <- abs(dpl_peaks$user.rt - dpl_peaks$peaks.rt.unsm)
    dpl_peaks[, rt_diff_min := min(rt_diff), by = .(peaks.height, peaks.rt.unsm)]
    dpl_peaks$keep_rt <- dpl_peaks$rt_diff == dpl_peaks$rt_diff_min
    kick_list <- dpl_peaks[keep_rt == FALSE]$IDX
    Depl_table <- Depl_table[!(IDX %in% kick_list)]
    dpl_peaks <- hutils::duplicated_rows(Depl_table, by = c("peaks.rt.unsm",
                                                                       "peaks.height",
                                                                       "FileName"))

  }

  if(nrow(dpl_peaks) > 0){

    dpl_peaks[, mz_diff_min := min(peaks.mz_accuracy_ppm), by = .(peaks.height, peaks.rt.unsm)]
    dpl_peaks$keep_mz <- dpl_peaks$peaks.mz_accuracy_ppm == dpl_peaks$mz_diff_min
    kick_list <- dpl_peaks[keep_mz == FALSE]$IDX
    Depl_table <- Depl_table[!(IDX %in% kick_list)]



  }

  return(Depl_table)

}
