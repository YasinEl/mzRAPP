#' pick_main_peak
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
pick_main_peak <- function(dt){
  (unique(dt$sample_id_b))
  main_peak_dt <- rbind(dt[, pick_main_peak_sd(.SD), by=c('molecule_b', 'adduct_b', 'sample_id_b'), .SDcols=c('comp_id_b', 'comp_id_ug', 'isoabb_b', 'peak_area_ug', 'rt_start_ug', 'rt_end_ug')])
  (nrow(main_peak_dt))
  (nrow(dt))
  dt <- merge(dt, main_peak_dt, by=c('molecule_b', 'adduct_b', 'sample_id_b', 'comp_id_b', 'comp_id_ug'), all.x = TRUE)
  #check if each group has excatly one main peak
  group_check_dt <- dt[, sum(main_peak, na.rm = TRUE), by=c('molecule_b', 'adduct_b', 'sample_id_b', 'isoabb_b')]
  #Make sure no main peak dosnt happen
  if (any(is.na(group_check_dt$V1))){
    (group_check_dt[is.na(V1)])
    stop()
  }
  #make sure not more than one main peak
  if(any(group_check_dt$V1 != 1)){
    ('Error picking main peak at:')
    (group_check_dt[V1 != 1])
    stop()
  }
  #Set NA to False
  dt <- dt[, 'main_peak' := ifelse(is.na(main_peak), FALSE, main_peak)]
  return(dt)
}


#' pick_main_peak_sd
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
pick_main_peak_sd <- function(dt){
  dt <- copy(dt)
  #isoabb check
  #Get list of all avaiable iso_abbs
  all_iso_abs <- sort(unique(dt[,isoabb_b]), decreasing = TRUE)



  #if number of peaks is equal to the number of iso abbs mark all as main peak
  if (nrow(dt) == length(all_iso_abs)){
    dt <- dt[,'main_peak' := TRUE]
    return(dt[, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  } else if(length(all_iso_abs) == 1){
    #When no other iso abb is present pick the one with shortest length, if the same take first one
    dt[, peak_length := rt_end_ug - rt_start_ug]
    dt <- dt[!duplicated(dt, by=c('peak_area_ug', 'peak_length'))]
    dt <- dt[, 'main_peak' := ifelse(peak_length == min(peak_length), TRUE, FALSE)]
    return(dt[, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  } else {
    #Iso_abb Comparison
    dt[, merge_key := 1]
    dt[, peak_length := rt_end_ug - rt_start_ug]
    #(dt)

    #Check for peaks with same lenght and area per iso_abb, if present pick first one
    dt <- dt[!duplicated(dt, by=c('peak_area_ug', 'peak_length'))]
    dt <- dt[, 'smallest_length' := ifelse(peak_length == min(peak_length), TRUE, FALSE), by=c('peak_area_ug')]
    dt <- dt[smallest_length==TRUE]


    #(dt)
    #Build compariosn DT
    temp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    temp_dt <- temp_dt[isoabb_b.x > isoabb_b.y]
    #Calculate %difference from expected ratio
    temp_dt <- temp_dt[,ratio_diff := (peak_area_ug.y/peak_area_ug.x)/(isoabb_b.y/isoabb_b.x)]
    #Assign row_id, group_id (comparison groups) and id within group
    temp_dt <- temp_dt[, row_id := seq_len(.N)]
    temp_dt <- temp_dt[, grp_id := .GRP, by=c('isoabb_b.x', 'isoabb_b.y')]
    temp_dt <- temp_dt[, id_in_group := seq_len(.N), by=c('isoabb_b.x', 'isoabb_b.y')]
    #(temp_dt)
    #Build all posible combinations of all rows
    #combination_list <- vector(mode='list', length = length(unique(tt$grp_id)))
    #(combination_list)
    combination_dt <- data.table()
    for (i in unique(temp_dt$grp_id)){
      if (i == 1){
        combination_dt <- combination_dt[, eval(quote(as.character(i))) := unique(temp_dt[grp_id == i, row_id])]
        combination_dt <- combination_dt[, 'merge_key' := 1]
      } else {
        combination_dt_temp <- data.table()
        combination_dt_temp <- combination_dt_temp[, eval(quote(as.character(i))) := unique(temp_dt[grp_id == i, row_id])]
        combination_dt_temp <- combination_dt_temp[, 'merge_key' := 1]
        combination_dt <- merge(combination_dt, combination_dt_temp, by=c('merge_key'), allow.cartesian = TRUE)
      }
    }
    combination_dt[,'merge_key' := NULL]

    #For each combination, get ratio_diff and summ up
    ratio_error_dt <- data.table()
    for (i in 1:nrow(combination_dt)){
      row <- combination_dt[i, ]
      ratio_error <- list('rows' = paste(row, collapse = ','), 'error' = sum(temp_dt[row_id %in% row, ratio_diff]))
      ratio_error_dt <- rbindlist(list(ratio_error_dt, ratio_error))
    }

    ratio_error_dt <- ratio_error_dt[, 'correct_rows' := ifelse(error == min(error), TRUE, FALSE)]
    (ratio_error_dt)
    #(sum(ratio_error_dt$correct_rows))
    #(unlist(strsplit(ratio_error_dt[correct_rows == TRUE, rows], ',')))
    if(nrow(ratio_error_dt[correct_rows == TRUE]) == 1){
      temp_dt <- temp_dt[, 'main_peak' := ifelse(row_id %in% unlist(strsplit(ratio_error_dt[correct_rows == TRUE, rows], ',')), TRUE, FALSE)]
      main_peaks <- unique(append(temp_dt[main_peak == TRUE, comp_id_ug.x], temp_dt[main_peak == TRUE, comp_id_ug.y]))
      dt <- dt[, 'main_peak' := ifelse(comp_id_ug %in% main_peaks, TRUE, FALSE)]
    } else {
      #Hier rein wenn es mehrere gleiche ratios gibt
      stop()
    }
    return(dt[, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  }
}
