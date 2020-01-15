#' find_main_feature_1
#'
#' @param c_table
#' @param setting
#' @param nf_g_table
#' @param nf_b_table
#'
#' @return
#' @export
#'
#' @examples
find_main_feature_1 <- function(c_table, setting, nf_g_table, nf_b_table){

  if (setting == 'correct'){

    c_table[, 'main_feature' := as.integer(find_most_occuring_feature(feature_id_g, peak_area_g)), by=c('feature_id_b')]
    c_table <- c_table[, 'is_main_feature' := ifelse((main_feature == feature_id_g) & !is.na(feature_id_g), TRUE, FALSE)]
    fwrite (c_table, 'cd_dbug.csv')
    return (c_table)
  } else if (setting == 'match_iso_pattern'){
    #fwrite(dt[feature_id_g == 3152], 'debug.csv')
    max_possible_found_peaks <- rbindlist(list(c_table, nf_b_table), fill=TRUE)[, .N, by=feature_id_b]
    #print('111')
      best_max_iso <- c_table[,match_features_by_iso(.SD, max_possible_found_peaks, nf_g_table, 0.5), by=c('molecule_b', 'adduct_b')]
      #print('222')
      best_max_iso <- best_max_iso[, 'is_main_feature' := ifelse((best_feature == feature_id_g) & !is.na(feature_id_g), TRUE, FALSE)]
      #print('333')
      fwrite(best_max_iso, 'dbug.csv')

      return(best_max_iso)
    }

  }

#' find_most_occuring_feature
#'
#' @param feature_id_g
#' @param peak_area_g
#'
#' @return
#' @export
#'
#' @examples
find_most_occuring_feature <- function (feature_id_g, peak_area_g){
  dt <- setDT(list('feature_id_g' = feature_id_g, 'peak_area_g' = peak_area_g))
  if (!all(is.na(dt$feature_id_g))){
    main_feature <- as.list(names(which(table(dt$feature_id_g) == max(table(dt$feature_id_g)))))
    #If features occure same number of times, pick the one with higher mean area
    if (length(main_feature) >= 2){
      dt <- dt[feature_id_g %in% main_feature, mean(peak_area_g), by=c('feature_id_g')]
      main_feature <- dt[V1 == max(V1), feature_id_g]
    }
  } else {
    main_feature <- NA
  }
  #print('----')
  #print(main_feature)
  #print(length(main_feature))
  #print('----')
  #If mean area is equal pick feature with lower ID
  if (length(main_feature)>1){
    main_feature <- main_feature[1]
    #print(paste0('Selected main feature: ', main_feature))
  }
  return(main_feature)
}

#' match_features_by_iso
#'
#' @param dt
#' @param max_possible_found_peaks
#' @param nf_g_table
#' @param factor
#'
#' @return
#' @export
#'
#' @examples
match_features_by_iso <- function(dt, max_possible_found_peaks, nf_g_table, factor){
  #print('New Run')
  #print(length(unique(dt$feature_id_b)))
  if (length(unique(dt$feature_id_b)) >= 2){
  all_isos <- sort(unique(dt$isoabb_b), decreasing = TRUE)
  #print(all_isos)
  best_first_isos <- find_best_matching_iso_pairs(dt, all_isos[1], all_isos[2], max_possible_found_peaks, nf_g_table, 0.5)
  #dt <- merge(dt, best_first_isos, by.x = 'isoabb_b', by.y = 'isoabb', all.x = TRUE)
  best_first_iso_feature <- best_first_isos[1, best_feature]
  #print(best_first_iso_feature)
    best_feature_list <- data.table()
    for (i in 2:length(all_isos)){
      best_feature_list <- rbindlist(list(best_feature_list, find_best_matching_iso_pairs(dt, all_isos[1], all_isos[i], max_possible_found_peaks, nf_g_table, 0.5, iso_1_feature = best_first_iso_feature)))
      best_feature_list <- best_feature_list[!is.na(best_feature)]
      best_feature_list <- best_feature_list[!duplicated(best_feature_list)]
    }
    fwrite(dt, 'dt_msdail_feature_dbug.csv')
    #print(best_feature_list)
  dt <- merge(dt, best_feature_list, by.x = 'isoabb_b', by.y = 'isoabb', all.x = TRUE)
  #print('---------')
  return(dt)
  }
}


#' find_best_matching_iso_pairs
#'
#' @param dt
#' @param iso_1
#' @param iso_2
#' @param max_possible_found_peaks
#' @param nf_g_table
#' @param factor
#' @param iso_1_feature
#'
#' @return
#' @export
#'
#' @examples
find_best_matching_iso_pairs <- function(dt, iso_1, iso_2, max_possible_found_peaks, nf_g_table, factor, iso_1_feature = NA){

  #Flter dt, propably not needed when apply
  dt <- dt[, c('feature_id_g', 'peak_area_g', 'isoabb_b', 'sample_id_g', 'feature_id_b')]
  all_isos <- sort(unique(dt$isoabb_b), decreasing = TRUE)

  #Takes 2 benchmark features and two iso_abbs and finds the best matching pair between them
  #iso_2 must be smaller than iso_1
  if(iso_2 >= iso_1) {
    #print('111')
    #Better error check!
    stop()
  }
  #Exactly 2 benchmark features must be present
  #if(length(unique(dt$feature_id_b))!= 2){
  #  #print(dt$feature_id_b)
  #}
  #For first isoabb, find all not found peaks of that feature in g and add to dt, can be set for all features or a specific one from g
  ##print(iso_1_feature)
  if (is.na(iso_1_feature)){
    peaks_1 <- dt[isoabb_b == iso_1]
  } else {
    #print(iso_1_feature)
    peaks_1 <- dt[(feature_id_g == iso_1_feature) & (isoabb_b == iso_1)]
    #Error check if feature id is given but no peaks are found
    if (nrow(peaks_1) == 0){
      #print('warning')
    }
  }
  peaks_nf_g_1 <- nf_g_table[feature_id_g %in% na.omit(unique(peaks_1$feature_id_g)), c('feature_id_g', 'peak_area_g', 'sample_id_g'), with=T]
  peaks_nf_g_1 <- peaks_nf_g_1[,':='('isoabb_b'=iso_1,'nf_tag' = TRUE)]
  peaks_1 <- rbindlist(list(peaks_1, peaks_nf_g_1), fill=TRUE)
  benchmark_feature_1 <- unique(na.omit(peaks_1$feature_id_b))



  #repeat for second isoabb
  peaks_2 <- dt[isoabb_b == iso_2]
  peaks_nf_g_2 <- nf_g_table[feature_id_g %in% na.omit(unique(peaks_2$feature_id_g)), c('feature_id_g', 'peak_area_g', 'sample_id_g'), with=T]
  peaks_nf_g_2 <- peaks_nf_g_2[,':='('isoabb_b'=iso_2,'nf_tag' = TRUE)]
  peaks_2 <- rbindlist(list(peaks_2, peaks_nf_g_2), fill=TRUE)
  benchmark_feature_2 <- unique(na.omit(peaks_2$feature_id_b))

  #Merge
  compare_dt <- merge(peaks_1, peaks_2, by=c('sample_id_g'), allow.cartesian =T)
  #print(paste0('ROWS: ', nrow(compare_dt)))

  ###Stop if no comparison is done
  if (nrow(compare_dt)==0){
    return_dt <- as.data.table(list(isoabb = iso_1, best_feature = as.integer(iso_1_feature)))
    #print(return_dt)
    return(return_dt)
  }
  #Filter out NAs
  compare_dt <- compare_dt[!is.na(feature_id_g.x) & !is.na(feature_id_g.y)]




  #Calc Expected Ratio, Start and End
  exp_ratio <- iso_2/iso_1


  #ADJUST RATIO, make dynamic?
  exp_ratio_start <- exp_ratio-(exp_ratio*factor)
  exp_ratio_end <- exp_ratio+(exp_ratio*factor)

  #Calcultae the ratio of the areas to expected
  compare_dt[, area_ratio := (peak_area_g.y/peak_area_g.x)]

  #print('----')


  #####
  #Calc median area_ratio_diff per feature in isoabb_1 and occurances of feature)
  feature_median_1 <- compare_dt[, list(median(area_ratio), .N, length(unique(.SD[is.na(nf_tag.x), sample_id_g])), length(unique(.SD[is.na(nf_tag.x), sample_id_g]))/max_possible_found_peaks[feature_id_b == benchmark_feature_1, N]), by=c('feature_id_g.x')]
  setnames(feature_median_1, c('V1', 'N', 'V3', 'V4'), c('area_ratio_median', 'count', 'count_found', 'found_ratio'))
  feature_median_1 <- feature_median_1[, 'area_ratio_median_diff' := abs(area_ratio_median - exp_ratio)]
  feature_median_1 <- feature_median_1[order(area_ratio_median_diff, decreasing = FALSE)]
  ####

  #print(feature_median_1)



  #####
  #Calc median area_ratio_diff per feature in isoabb_2 and occurances of feature)
  feature_median_2 <- compare_dt[, list(median(area_ratio), .N, length(unique(.SD[is.na(nf_tag.x), sample_id_g])), length(unique(.SD[is.na(nf_tag.x), sample_id_g]))/max_possible_found_peaks[feature_id_b == benchmark_feature_1, N]), by=c('feature_id_g.y')]
  setnames(feature_median_2, c('V1', 'N', 'V3', 'V4'), c('area_ratio_median', 'count', 'count_found', 'found_ratio'))
  feature_median_2 <- feature_median_2[, 'area_ratio_median_diff' := abs(area_ratio_median - exp_ratio)]
  feature_median_2 <- feature_median_2[order(area_ratio_median_diff, decreasing = FALSE)]
  ####

  #print(feature_median_2)


  #Find the best fitting feature 1
  if (nrow(feature_median_1)==1){
    best_feature_1 <- as.integer(feature_median_1[,feature_id_g.x])
  } else {
    filterd_feature_median_1 <- feature_median_1[(area_ratio_median > exp_ratio_start) & (area_ratio_median < exp_ratio_end)]
    filterd_feature_median_1 <- filterd_feature_median_1[order(found_ratio,area_ratio_median_diff, decreasing =TRUE)]
    best_feature_1 <- as.integer(filterd_feature_median_1[1, feature_id_g.x])
  }

  #Find the best fitting feature 2
  if (nrow(feature_median_2)==1){
    best_feature_2 <- as.integer(feature_median_2[,feature_id_g.y])
  } else {
    filterd_feature_median_2 <- feature_median_2[(area_ratio_median > exp_ratio_start) & (area_ratio_median < exp_ratio_end)]
    filterd_feature_median_2 <- filterd_feature_median_2[order(found_ratio,area_ratio_median_diff, decreasing =TRUE)]
    best_feature_2 <- as.integer(filterd_feature_median_2[1, feature_id_g.y])
  }

  iso_vec <- c(iso_1, iso_2)
  best_feature_vec <- c(best_feature_1, best_feature_2)
  return_dt <- as.data.table(list(isoabb = iso_vec, best_feature = best_feature_vec))
  #print(return_dt)
  #return_dt <- return_dt[order(by='isoabb', decreasing=TRUE)]
return(return_dt)
}



#test <- find_main_feature(comparison_ev$c_table, setting='correct')
#View(test)
#fwrite(test, 'debug_main_feature.csv')




#test_dt_1 <- rbindlist(list(comparison_ev$c_table))#, comparison_ev$nf_b_table), fill=TRUE)#[feature_id_b == 66]#[c(1,2,7,13,3,4,5,6)]
#max_possible_found_peaks <- rbindlist(list(test_dt_1, comparison_ev$nf_b_table), fill=TRUE)[, .N, by=feature_id_b]



#test_dt <- test_dt_1#[(feature_id_b == 185) | (feature_id_b == 186)]
#all_isos <- sort(unique(test_dt$isoabb_b), decreasing = TRUE)
##print(all_isos)

#output_dt <- find_best_matching_iso_pairs(test_dt, all_isos[1], all_isos[2], max_pssible_found_peaks, comparison_ev$nf_g_table, 0.5, iso_1_feature = 3990)

##print(output_dt)
####test_dt <- find_main_feature_1(test_dt_1, setting = 'match_iso_pattern', comparison_ev$nf_g_table, comparison_ev$nf_b_table)
#####print(test_dt)
##print(length(unique(na.omit(test_dt[]))))#[,.(feature_id_g, feature_id_b, main_feature, is_main_feature), with=T])
#fwrite(test_dt[,.(feature_id_g, feature_id_b, main_feature, is_main_feature), with=T], 'debug_main_feature.csv')
