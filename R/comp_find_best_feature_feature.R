#' find_best_feature_feature
#'
#' @param dt
#' @param bys
#'
#' @return
#' @export
#'
#' @examplesx
find_best_feature_feature <- function(dt, bys) {

  dt <- copy(dt)
  dt <- dt[, 'cross_join_key' := 1]

  #print(dt)
  #print('--')
  #print(bys)
  #print('--')
  #new_dt <- merge(dt, dt, by=NULL)
  #print(dt[dt, on=.(isoabb_b>isoabb_b), allow.cartesian= TRUE, mult='all'])
  #print(new_dt)
  #print('$$$$$$$$')

  all_iso <- sort(unique(dt$isoabb_b), decreasing=TRUE)
  #print(dt)
  #Stop if each iso occures axactly once, mark all as main feature
  if (length(all_iso) == nrow(dt)){
    return_dt <- setDT(list(feature_id_g = unique(dt$feature_id_g)))
    return_dt[, 'main_feature' := TRUE]
    #print(return_dt)
    #return(setDT(list(feature_id_g = unique(dt$feature_id_g))))
  } else if (length(all_iso) < nrow(dt)){

    return_dt <- setDT(list(feature_id_g = unique(dt$feature_id_g)))

    exp_ratio = all_iso[2]/all_iso[1]

    #Figure out first (100) iso main feature
    merged_dt <- merge(dt[isoabb_b == all_iso[1]], dt[isoabb_b == all_iso[2]], by='cross_join_key', allow.cartesian = TRUE)
    #print(dt)
    #print(merged_dt)
    print(exp_ratio)
    print('***')
    merged_dt[, area_ratio := mean_area_g.y/mean_area_g.x]
    merged_dt[, main_feature := ifelse(abs(area_ratio-exp_ratio) == min(abs(area_ratio-exp_ratio)), TRUE, FALSE)]
    print(merged_dt)
    print('***')

    print(nrow(merged_dt[main_feature == TRUE]))

    #Debug check - exactly one main feature should be present and no na
    if (nrow(merged_dt[main_feature == TRUE]) > 1){
      print('error in main feature')
      stop()
    }

    highest_main_feature <- merged_dt[main_feature == TRUE]$feature_id_g.x
    main_features <- list(highest_main_feature)

    #for Debug
    second_h_feature <- merged_dt[main_feature == TRUE]$feature_id_g.y

    print(highest_main_feature)

    rm(merged_dt)

    for (i in 2:length(all_iso)) {
      exp_ratio = all_iso[i]/all_iso[1]
      merged_dt <- merge(dt[feature_id_g == highest_main_feature], dt[isoabb_b == all_iso[i]], by='cross_join_key', allow.cartesian = TRUE)
      merged_dt[, area_ratio := mean_area_g.y/mean_area_g.x]
      merged_dt[, main_feature := ifelse(abs(area_ratio-exp_ratio) == min(abs(area_ratio-exp_ratio)), TRUE, FALSE)]

      #Debug check - exactly one main feature should be present and no na
      if (nrow(merged_dt[main_feature == TRUE]) > 1){
        print('error in main feature')
        stop()
      }

      main_features[i] <- merged_dt[main_feature == TRUE]$feature_id_g.y
      rm(merged_dt)
    }
    print('----')
    print(main_features)
    return_dt[, main_feature := ifelse(feature_id_g %in% main_features, TRUE, FALSE)]
  }
  print(return_dt)
  return(return_dt)
}