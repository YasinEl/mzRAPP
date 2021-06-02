#' pick_main_peak
#'
#' @param dt dt
#'
#'
#' @keywords internal
pick_main_peak <- function(dt){

  main_peak_dt <- dt[, pick_main_peak_sd(.SD), by=c('molecule_b', 'adduct_b', 'sample_id_b'), .SDcols=c('molecule_b', 'adduct_b', 'sample_id_b', 'comp_id_b', 'comp_id_ug', 'isoab_b', 'rt_start_ug', 'rt_end_ug', 'peak_area_ug')]
  dt <- merge(dt, main_peak_dt[,c('comp_id_b', 'comp_id_ug', 'main_peak')], by=c('comp_id_b', 'comp_id_ug'), all.x=TRUE)
  dt[is.na(main_peak) & !is.na(peak_area_ug), main_peak := FALSE]
  return(dt)

}


#' pick_main_peak_sd
#'
#' @param dt dt
#'
#'
#' @keywords internal
pick_main_peak_sd <- function(dt){
  dt <- data.table::copy(dt)
  all_iso_abbs <- sort(unique(dt[,isoab_b]), decreasing = TRUE)

  if(nrow(dt) == length(unique(all_iso_abbs))){
    #If number of rows is equal to isoabs mark all as main peak
    dt[, 'main_peak' := TRUE]
    return(dt[main_peak == TRUE, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  } else if(length(unique(all_iso_abbs)) == 1){
    #If only one isoab is present pick peak with shortest length
    dt[, 'peak_length' := rt_end_ug - rt_start_ug]
    dt[, 'main_peak' := ifelse(peak_length == min(peak_length), TRUE, FALSE)]
    if(nrow(dt[main_peak == TRUE]) > 1){
      dt[peak_area_ug < max(peak_area_ug), main_peak := FALSE]
    }
    return(dt[main_peak == TRUE, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  } else {
    #Iso_abb Comparison
    dt[, 'merge_key' := 1]
    dt[, 'peak_length' := rt_end_ug - rt_start_ug]

    #Build comparison DT
    comp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    comp_dt <- comp_dt[isoab_b.x > isoab_b.y]

    #Calculate %difference from expected ratio
    comp_dt <- comp_dt[,'ratio_diff' := abs((peak_area_ug.y/peak_area_ug.x)-(isoab_b.y/isoab_b.x))]

    #Calc best best peak per comparison (=comp)
    comp_dt <- comp_dt[, 'min_ratio_diff' := ifelse(ratio_diff == min(ratio_diff), TRUE, FALSE), by=c('isoab_b.x', 'isoab_b.y')]

    x_dt <- data.table::setnames(comp_dt[min_ratio_diff == TRUE, c('comp_id_b.x', 'comp_id_ug.x', 'ratio_diff')], c('comp_id_b.x', 'comp_id_ug.x'), c('comp_id_b', 'comp_id_ug'))
    y_dt <- data.table::setnames(comp_dt[min_ratio_diff == TRUE, c('comp_id_b.y', 'comp_id_ug.y', 'ratio_diff')], c('comp_id_b.y', 'comp_id_ug.y'), c('comp_id_b', 'comp_id_ug'))
    main_peaks_dt <- data.table::rbindlist(list(x_dt, y_dt), use.names = TRUE)
    main_peaks_dt <- main_peaks_dt[!duplicated(main_peaks_dt, by=c('comp_id_b', 'comp_id_ug'))]
    main_peaks_dt <- main_peaks_dt[, 'main_peak' := ifelse(ratio_diff == min(ratio_diff), TRUE, FALSE), by=c('comp_id_b')]
    if(nrow(main_peaks_dt[main_peak == TRUE]) != length(unique(dt$comp_id_b))){
      #If several possible peaks have the same ratio pick the smallest peak, if still multiple present pick first one
      main_peaks_dt <- merge(main_peaks_dt, dt[, c('comp_id_ug', 'peak_length')], by=c('comp_id_ug'), all.x=TRUE)
      main_peaks_dt <- main_peaks_dt[, 'main_peak' := ifelse(peak_length == min(peak_length), TRUE, FALSE), by=c('comp_id_b')]
      main_peaks_dt <- main_peaks_dt[!duplicated(main_peaks_dt, by=c('comp_id_b', 'main_peak'))]
    }
    return(main_peaks_dt[main_peak == TRUE, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  }
}
