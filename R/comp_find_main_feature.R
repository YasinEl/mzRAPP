find_main_feature_1 <- function(dt, setting, nf_g_table, nf_b_table){

  if (setting == 'correct'){

    dt[, 'main_feature' := find_most_occuring_feature(feature_id_g, peak_area_g), by=c('feature_id_b')]
    dt <- dt[, 'is_main_feature' := ifelse((main_feature == feature_id_g) & !is.na(feature_id_g), TRUE, FALSE)]
    return (dt)
  } else if (setting == 'match_iso_pattern'){
    #fwrite(dt[feature_id_g == 3152], 'debug.csv')
    dt <- dt[,match_features_by_iso(feature_id_g, peak_area_g, isoabb_b, sample_id_g, nf_g_table, feature_id_b), by=c('molecule_b', 'adduct_b')]
    return(dt)
  }
}

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
  return(main_feature)
}

match_features_by_iso <- function(feature_id_g, peak_area_g, isoabb_b, sample_id_g, nf_g_table, nf_b_table, feature_id_b){

  dt <- setDT(list('feature_id_g' = feature_id_g, 'peak_area_g' = peak_area_g, 'isoabb_b' = isoabb_b, 'sample_id_g' = sample_id_g, 'feature_id_b' = feature_id_b))
  all_iso_groups <- sort(unique(dt$isoabb_b), decreasing = TRUE)
  #generate list of maximum possible peaks found based on benchmark
  max_possible_found_peaks <- rbindlist(list(dt, nf_b_table), fill=TRUE)[, .N, by=feature_id_b]
  #Find best Iso 100 feature by comparing it to the second iso group
  if(length(all_iso_groups >= 2)){
    all_iso_groups <- sort(unique(dt$isoabb_b), decreasing = TRUE)
    peaks_1 <- dt[isoabb_b == all_iso_groups[1]]
    benchmark_feature_1 <- unique(peaks_1[,feature_id_b])
    peaks_nf_g_1 <- nf_g_table[feature_id_g %in% na.omit(unique(peaks_1$feature_id_g)), c('feature_id_g', 'peak_area_g', 'sample_id_g'), with=T]
    peaks_nf_g_1 <- peaks_nf_g_1[,':='('isoabb_b'=all_iso_groups[1],
                             'nf_tag' = TRUE)]
    peaks_1 <- rbindlist(list(peaks_1, peaks_nf_g_1), fill=TRUE)



    peaks_2 <- dt[isoabb_b == all_iso_groups[2]]

    benchmark_feature_2 <- unique(peaks_2[,feature_id_b])

    peaks_nf_g_2 <- nf_g_table[feature_id_g %in% na.omit(unique(peaks_2$feature_id_g)), c('feature_id_g', 'peak_area_g', 'sample_id_g'), with=T]
    peaks_nf_g_2 <- peaks_nf_g_2[,':='('isoabb_b'=all_iso_groups[2],
                                       'nf_tag' = TRUE)]
    peaks_2 <- rbindlist(list(peaks_2, peaks_nf_g_2), fill=TRUE)


    compare_dt <- merge(peaks_1, peaks_2, by=c('sample_id_g'), allow.cartesian =T)
    #Filter out NAs
    compare_dt <- compare_dt[!is.na(feature_id_g.x) & !is.na(feature_id_g.y)]

    #Calc Expected Ratio, Start and End
    exp_ratio <- all_iso_groups[2]/all_iso_groups[1]


    #ADJUST RATIO, make dynamic?
    exp_ratio_start <- exp_ratio-(exp_ratio*0.5)
    exp_ratio_end <- exp_ratio+(exp_ratio*0.5)

    #Calcultae the ratio of the areas to expected
    compare_dt[, area_ratio := (peak_area_g.y/peak_area_g.x)]



    #####
    #Calc median area_ratio_diff per feature in 100 and occurances of feature) (do i need found_ratio? count should be enough! - no need ratio for later loop check)
    feature_median <- compare_dt[, list(median(area_ratio), .N, length(unique(.SD[is.na(nf_tag.x), sample_id_g])), length(unique(.SD[is.na(nf_tag.x), sample_id_g]))/max_possible_found_peaks[feature_id_b == benchmark_feature_1, N]), by=c('feature_id_g.x')]
    setnames(feature_median, c('V1', 'N', 'V3', 'V4'), c('area_ratio_median', 'count', 'count_found', 'found_ratio'))
    feature_median <- feature_median[, 'area_ratio_median_diff' := abs(area_ratio_median - exp_ratio)]
    feature_median <- feature_median[order(area_ratio_median_diff, decreasing = FALSE)]
    ####

    # print(compare_dt)
    # print(feature_median)

    #print(feature_median[count == 40])



    #Fin best 100 feature: check highest count, if in border of ration take, if not go to next
    mait_feature <- NA
    if(nrow(feature_median) == 1){
      mait_feature <- feature_median[,feature_id_g.x]
      #print(mait_feature)
    } else if(nrow(feature_median) >= 1) {
      print(unique(dt[,feature_id_b]))
      View(dt)
      print(paste0('Benchmark Feature: ',benchmark_feature_1))
      print(paste0('Benchmark Feature: ',benchmark_feature_2))
      print(feature_median)
      print(max_possible_found_peaks)
      print(exp_ratio_start)
      print(exp_ratio_end)

      print(compare_dt)




      #####Try New
      ##Filter out peaks not in boundary
      filterd_feature_median <- feature_median[(area_ratio_median > exp_ratio_start) & (area_ratio_median < exp_ratio_end)]
      print(feature_median)
      print(filterd_feature_median)
      print('---')
      filterd_feature_median <- filterd_feature_median[order(found_ratio, decreasing =TRUE)]
      print(filterd_feature_median)
      mait_feature <- filterd_feature_median[1, feature_id_g.x]
      print(paste0('mait f: ',mait_feature))
      mait_feature <- as.integer(9000000)


      # #If feature with smallest median_diff hast match ratio of 1 pick it
      # if(feature_median[1, found_ratio] == 1){
      #   mait_feature <- feature_median[1, feature_id_g.x]
      # }
      #
      # #If not complete group feature: Go through sotrted feature median list, check if element still in boundary
      #
      # i <- 1
      #
      # while (is.na(mait_feature)){
      #   area_ratio <- feature_median[i, area_ratio_median]
      #   print(feature_median)
      #   print(area_ratio)
      #   print(exp_ratio_start)
      #   print(exp_ratio_end)
      #   if((exp_ratio_start < area_ratio) & (area_ratio < exp_ratio_end)){
      #     mait_feature <- feature_median[area_ratio_median == area_ratio, feature_id_g.x]
      #     #print(mait_feature)
      #   }
      #   i <- i+1
      # }
    }




    #print(feature_median)



    #calculate mean area diff for every feature, get the smaller one, this is the 100 main feature
    #feature_mean_diff <- compare_dt[, median(area_ratio_diff), by=c('feature_id_g.x')]
    #smaller_feature <- feature_mean_diff[V1 == min(na.omit(V1), na.rm=TRUE), feature_id_g.x]
    #print(exp_ratio)
    #print(smaller_feature)




    #if (length(unique(na.omit(peaks_1$feature_id_g)))>=2){
      #print(compare_dt)

    #}
    # if (length(unique(na.omit(dt[isoabb_b == 100]$feature_id_g))) >=2 ){
    #   peaks
    #
    #   print(length(unique(na.omit(dt[isoabb_b == 100]$feature_id_g))))
    #   print(dt[order(feature_id_g)])
    #   print('-----')
    # }
  }
}



#test <- find_main_feature(comparison_ev$c_table, setting='correct')
#View(test)
#fwrite(test, 'debug_main_feature.csv')


#test_dt_1 <- rbindlist(list(comparison_ev$c_table))#, comparison_ev$nf_b_table), fill=TRUE)#[feature_id_b == 66]#[c(1,2,7,13,3,4,5,6)]
#test_dt <- find_main_feature_1(test_dt_1, setting = 'match_iso_pattern', comparison_ev$nf_g_table, comparison_ev$nf_b_table)
#print(length(unique(na.omit(test_dt[]))))#[,.(feature_id_g, feature_id_b, main_feature, is_main_feature), with=T])
#fwrite(test_dt[,.(feature_id_g, feature_id_b, main_feature, is_main_feature), with=T], 'debug_main_feature.csv')
