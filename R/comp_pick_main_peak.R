pick_main_peak_2 <- function(dt){

  main_peak_dt <- dt[, pick_main_peak_2_sd(.SD), by=c('molecule_b', 'adduct_b', 'sample_id_b'), .SDcols=c('molecule_b', 'adduct_b', 'sample_id_b', 'comp_id_b', 'comp_id_ug', 'isoabb_b', 'rt_start_ug', 'rt_end_ug', 'peak_area_ug')]
  #print(main_peak_dt)
  #print(nrow(main_peak_dt))
  #print(nrow(dt))
  #print(length(unique(dt$comp_id_b)))
  #print(setdiff(unique(dt$comp_id_b), main_peak_dt$comp_id_b))
  #print(setdiff(main_peak_dt$comp_id_b, unique(dt$comp_id_b)))
  #print(main_peak_dt[duplicated(main_peak_dt, by=c('comp_id_b')) | duplicated(main_peak_dt, by=c('comp_id_b'), fromLast = TRUE)])

  dt <- merge(dt, main_peak_dt[,c('comp_id_b', 'comp_id_ug', 'main_peak')], by=c('comp_id_b', 'comp_id_ug'), all.x=TRUE)
  dt <- dt[, 'main_peak' := ifelse(main_peak == TRUE, TRUE, FALSE)]
  return(dt)
}


pick_main_peak_2_sd <- function(dt){
  dt <- copy(dt)
  all_iso_abbs <- sort(unique(dt[,isoabb_b]), decreasing = TRUE)
  if(nrow(dt) == length(all_iso_abbs)){
    #If number of rows is equal to isoabbs mark all as main peak
    dt[, 'main_peak' := TRUE]
    return(dt[main_peak == TRUE, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  } else if(length(all_iso_abbs) == 1){
    #If only one isoabb is present pick peak with shortest length
    dt[, 'peak_length' := rt_end_ug - rt_start_ug]
    dt[, 'main_peak' := ifelse(peak_length == min(peak_length), TRUE, FALSE)]
    return(dt[main_peak == TRUE, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  } else {
    #Iso_abb Comparison
    dt[, 'merge_key' := 1]
    dt[, 'peak_length' := rt_end_ug - rt_start_ug]

    #Build compariosn DT
    comp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    comp_dt <- comp_dt[isoabb_b.x > isoabb_b.y]

    #Calculate %difference from expected ratio
    comp_dt <- comp_dt[,'ratio_diff' := (peak_area_ug.y/peak_area_ug.x)/(isoabb_b.y/isoabb_b.x)]

    #Calc best best peak per comparison
    comp_dt <- comp_dt[, 'min_ratio_diff' := ifelse(ratio_diff == min(ratio_diff), TRUE, FALSE), by=c('isoabb_b.x', 'isoabb_b.y')]

    x_dt <- setnames(comp_dt[min_ratio_diff == TRUE, c('comp_id_b.x', 'comp_id_ug.x', 'ratio_diff')], c('comp_id_b.x', 'comp_id_ug.x'), c('comp_id_b', 'comp_id_ug'))
    y_dt <- setnames(comp_dt[min_ratio_diff == TRUE, c('comp_id_b.y', 'comp_id_ug.y', 'ratio_diff')], c('comp_id_b.y', 'comp_id_ug.y'), c('comp_id_b', 'comp_id_ug'))
    main_peaks_dt <- rbindlist(list(x_dt, y_dt), use.names = TRUE)
    main_peaks_dt <- main_peaks_dt[!duplicated(main_peaks_dt, by=c('comp_id_b', 'comp_id_ug'))]
    main_peaks_dt <- main_peaks_dt[, 'main_peak' := ifelse(ratio_diff == min(ratio_diff), TRUE, FALSE), by=c('comp_id_b')]

    if(nrow(main_peaks_dt[main_peak == TRUE]) != length(unique(dt$comp_id_b))){
      #If several possible peaks have the same ratio pick the smallest peak, if still multiple present pick first one
      main_peaks_dt <- merge(main_peaks_dt, dt[, c('comp_id_ug', 'peak_length')], by=c('comp_id_ug'), all.x=TRUE)
      main_peaks_dt <- main_peaks_dt[, 'main_peak' := ifelse(peak_length == min(peak_length), TRUE, FALSE), by=c('comp_id_b')]
      main_peaks_dt <- main_peaks_dt[!duplicated(main_peaks_dt, by=c('comp_id_b', 'main_peak'))]
      #print(dt)
      #print(comp_dt)
      #print(main_peaks_dt)
      #print('---')
    }

    return(main_peaks_dt[main_peak == TRUE, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  }
}




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
  main_peak_dt <- rbind(dt[, pick_main_peak_sd(.SD), by=c('molecule_b', 'adduct_b', 'sample_id_b'), .SDcols=c('molecule_b', 'comp_id_b', 'comp_id_ug', 'isoabb_b', 'peak_area_ug', 'rt_start_ug', 'rt_end_ug')])
  (nrow(main_peak_dt))
  (nrow(dt))
  dt <- merge(dt, main_peak_dt, by=c('molecule_b', 'adduct_b', 'sample_id_b', 'comp_id_b', 'comp_id_ug'), all.x = TRUE)
  #check if each group has excatly one main peak
  group_check_dt <- dt[, sum(main_peak, na.rm = TRUE), by=c('molecule_b', 'adduct_b', 'sample_id_b', 'isoabb_b')]
  #Make sure no main peak dosnt happen
  if (any(is.na(group_check_dt$V1))){
    print(group_check_dt[is.na(V1)])
    stop("Not main peak")
  }
  #make sure not more than one main peak
  if(any(group_check_dt$V1 != 1)){
    ('Error picking main peak at:')
    print(group_check_dt[V1 != 1])
    stop("Too many main peak")
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

    #Check for peaks with same lenght and area, if present pick first one
    dt <- dt[!duplicated(dt, by=c('peak_area_ug', 'peak_length'))]
    dt <- dt[, 'smallest_length' := ifelse(peak_length == min(peak_length), TRUE, FALSE), by=c('peak_area_ug')]
    dt <- dt[smallest_length==TRUE]

    #Build compariosn DT
    temp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    temp_dt <- temp_dt[isoabb_b.x > isoabb_b.y]
    #Calculate %difference from expected ratio
    temp_dt <- temp_dt[,ratio_diff := (peak_area_ug.y/peak_area_ug.x)/(isoabb_b.y/isoabb_b.x)]
    #Assign row_id, group_id (comparison groups) and id within group
    temp_dt <- temp_dt[, row_id := seq_len(.N)]
    temp_dt <- temp_dt[, grp_id := .GRP, by=c('isoabb_b.x', 'isoabb_b.y')]
    temp_dt <- temp_dt[, id_in_group := seq_len(.N), by=c('isoabb_b.x', 'isoabb_b.y')]
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
    if(nrow(ratio_error_dt[correct_rows == TRUE]) == 1){
      temp_dt <- temp_dt[, 'main_peak' := ifelse(row_id %in% unlist(strsplit(ratio_error_dt[correct_rows == TRUE, rows], ',')), TRUE, FALSE)]
      main_peaks <- unique(append(temp_dt[main_peak == TRUE, comp_id_ug.x], temp_dt[main_peak == TRUE, comp_id_ug.y]))
      if(length(main_peaks) > length(all_iso_abs)){
        dup_peaks <- dt[duplicated(dt, by=c('isoabb_b')) | duplicated(dt, by=c('isoabb_b'), fromLast = TRUE)]
        not_highest_peak <- dup_peaks[, 'smallest_length' := ifelse(peak_length == min(peak_length), TRUE, FALSE), by=c('comp_id_b')][smallest_length == FALSE, comp_id_ug]
        main_peaks <- main_peaks[!(main_peaks %in% not_highest_peak)]
        if(length(main_peaks) > length(all_iso_abs)){
          print(main_peaks)
          warning("Still to many main peaks") }
      }
      if(length(main_peaks) < length(all_iso_abs)){
        stop("Not enough main peaks")
      }
      dt <- dt[, 'main_peak' := ifelse(comp_id_ug %in% main_peaks, TRUE, FALSE)]
    } else {
      #Hier rein wenn es mehrere gleiche ratios gibt
      stop()
    }
    return(dt[, c('comp_id_b', 'comp_id_ug', 'main_peak')])
  }
}
