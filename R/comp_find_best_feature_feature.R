#' find_best_feature_feature
#'
#' @param dt dt
#' @param bys bys
#'
#' @return best feature
#'
#' @keywords internal
find_best_feature_feature <- function(dt, bys) {

  dt <- copy(dt)
  dt <- dt[, 'cross_join_key' := 1]

  all_iso <- sort(unique(dt$isoab_b), decreasing=TRUE)
  #Stop if each iso occures axactly once, mark all as main feature
  if (length(all_iso) == nrow(dt)){
    return_dt <- setDT(list(feature_id_g = unique(dt$feature_id_g)))
    return_dt[, 'main_feature' := TRUE]
  } else if (length(all_iso) < nrow(dt)){

    return_dt <- setDT(list(feature_id_g = unique(dt$feature_id_g)))

    exp_ratio = all_iso[2]/all_iso[1]

    #Figure out first (100) iso main feature
    merged_dt <- merge(dt[isoab_b == all_iso[1]], dt[isoab_b == all_iso[2]], by='cross_join_key', allow.cartesian = TRUE)
    merged_dt[, area_ratio := mean_area_g.y/mean_area_g.x]
    merged_dt[, main_feature := ifelse(abs(area_ratio-exp_ratio) == min(abs(area_ratio-exp_ratio)), TRUE, FALSE)]

    #Debug check - exactly one main feature should be present and no na
    if (nrow(merged_dt[main_feature == TRUE]) > 1){
      stop('error in main feature')
    }

    highest_main_feature <- merged_dt[main_feature == TRUE]$feature_id_g.x
    main_features <- list(highest_main_feature)

    #for Debug
    second_h_feature <- merged_dt[main_feature == TRUE]$feature_id_g.y

    rm(merged_dt)

    for (i in 2:length(all_iso)) {
      exp_ratio = all_iso[i]/all_iso[1]
      merged_dt <- merge(dt[feature_id_g == highest_main_feature], dt[isoab_b == all_iso[i]], by='cross_join_key', allow.cartesian = TRUE)
      merged_dt[, area_ratio := mean_area_g.y/mean_area_g.x]
      merged_dt[, main_feature := ifelse(abs(area_ratio-exp_ratio) == min(abs(area_ratio-exp_ratio)), TRUE, FALSE)]

      #Debug check - exactly one main feature should be present and no na
      if (nrow(merged_dt[main_feature == TRUE]) > 1){
        stop('error in main feature')
      }

      main_features[i] <- merged_dt[main_feature == TRUE]$feature_id_g.y
    }
    return_dt[, main_feature := ifelse(feature_id_g %in% main_features, TRUE, FALSE)]
  }
  return(return_dt)
}
