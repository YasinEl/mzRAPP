#' pick_main_feature
#'
#' @param dt dt
#'
#'
#' @keywords internal
pick_main_feature <- function(dt){

  all_g_samples <- colnames(dt)[grep('sample_\\d{1,}_g', colnames(dt))]

  main_features_dt <- dt[, pick_main_feature_sd(.SD), by=c('molecule_b', 'adduct_b'), .SDcols = c('feature_id_b', 'feature_id_g', 'isoab_b', 'total_area_b', 'total_area_g', 'samples_to_compare', all_g_samples)]

  dt <- merge(dt, main_features_dt[,c('feature_id_b', 'feature_id_g', 'main_feature')], by=c('feature_id_b', 'feature_id_g'), all.x=TRUE)

  dt[is.na(main_feature), main_feature := FALSE]
  dt[!is.na(main_feature) & main_feature != FALSE, main_feature := TRUE]

  return(dt)
}


#' pick_main_feature_sd
#'
#' @param dt dt
#'
#'
#' @keywords internal
pick_main_feature_sd <- function(dt){
  dt <- data.table::copy(dt)

  #Get list of all available iso_abs
  all_iso_abs <- sort(unique(dt[,isoab_b]), decreasing = TRUE)

  if (nrow(dt)==length(unique(all_iso_abs))){
    #If list of features is equal to number of unique iso abs set all to main peak
    dt <- dt[, 'main_feature' := TRUE]
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
  } else if (length(unique(all_iso_abs)) == 1){
    #If only one isoab is present no comparison can be done! Choose feature with higher comparable mean area
    dt$average_area <- apply(dt, 1, function(x){
      compare_samples <- paste0('sample_', unlist(x['samples_to_compare']), '_g')
      if(length(x[["samples_to_compare"]]) == 0){return(-1)}
    return(mean(unlist(x[compare_samples]), na.rm = TRUE))
    })
    dt <- dt[, 'main_feature' := ifelse(average_area == min(average_area), TRUE, FALSE)]
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
  } else {
    #Isoab Comparison
    dt[, merge_key := 1]

    #Check for features with same area, if present pick first one
    dt <- dt[!duplicated(dt, by=c('total_area_g'))]

    #Build compariosn DT // here feature with best IR bias is selected
    comp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    comp_dt <- comp_dt[isoab_b.x > isoab_b.y]
    comp_dt <- comp_dt[, 'compare_ratio' := isoab_b.y/isoab_b.x]
    comp_dt[, 'group_temp_id' := .GRP, by=c('isoab_b.x', 'isoab_b.y')]
    comp_dt[, c('ratio_diff', 'min_ratio_diff') := best_feature_per_comparison(.SD), by=c('isoab_b.x', 'isoab_b.y')]
    x_dt <- data.table::setnames(comp_dt[min_ratio_diff == TRUE, c('feature_id_b.x', 'feature_id_g.x', 'ratio_diff')], c('feature_id_b.x', 'feature_id_g.x'), c('feature_id_b', 'feature_id_g'))
    y_dt <- data.table::setnames(comp_dt[min_ratio_diff == TRUE, c('feature_id_b.y', 'feature_id_g.y', 'ratio_diff')], c('feature_id_b.y', 'feature_id_g.y'), c('feature_id_b', 'feature_id_g'))
    main_features_dt <- data.table::rbindlist(list(x_dt, y_dt), use.names = TRUE)
    main_features_dt <- main_features_dt[!duplicated(main_features_dt, by=c('feature_id_b', 'feature_id_g'))]
    main_features_dt <- main_features_dt[, 'main_feature' := ifelse(ratio_diff == suppressWarnings(min(ratio_diff)), TRUE, FALSE), by=c('feature_id_b')]
    if(any(duplicated(main_features_dt[main_feature == TRUE]$feature_id_b))){
      #Handle if still more than one main feature per benchmark feature, in case of "draw" use feature with most compareable samples
      main_features_dt <- merge(main_features_dt, dt[, c('feature_id_b', 'feature_id_g', 'samples_to_compare')], on=c('feature_id_b', 'feature_id_g'))
      main_features_dt$no_of_samples <- apply(main_features_dt,1, function(x){length(unlist(x['samples_to_compare']))})
      main_features_dt[, 'main_feature' := ifelse(no_of_samples == max(no_of_samples), TRUE, FALSE), by=c('feature_id_b')]
    }

    return(main_features_dt[main_feature == TRUE, c('feature_id_b', 'feature_id_g', 'main_feature')])
  }
}

#' best_feature_per_comparison
#'
#' @param dt dt
#'
#'
#' @keywords internal
best_feature_per_comparison <- function(dt){
  dt <- data.table::copy(dt)
  dt$ratio_diff <- as.numeric(apply(dt, 1, function(x){compare_samples <- intersect(unlist(x['samples_to_compare.x']), unlist(x['samples_to_compare.y']))
                                                          if(length(compare_samples) < 1){
                                                            return(as.numeric(NULL))
                                                          }

                                                          compare_samples <- paste0('sample_', compare_samples, '_g')
                                                          ratio_errors <- list()
                                                          for (i in compare_samples){
                                                            if(i == 'sample__g'){
                                                              stop("Sample_g error")
                                                            }
                                                            ratio_errors <- append(ratio_errors, (abs(x[[paste0(i,'.y')]]/x[[paste0(i,'.x')]]-x[['compare_ratio']])))
                                                          }
                                                          return(median(unlist(ratio_errors)))
                                                        }
                                    )
                              )


  #Prevent warning in min()
  if(all(is.na(dt$ratio_diff))){
    dt <- dt[,min_ratio_diff := NA]
  } else {
    dt <- dt[,min_ratio_diff := ifelse(ratio_diff == min(ratio_diff, na.rm = TRUE), TRUE, FALSE)]
  }
  return(list(dt$ratio_diff, dt$min_ratio_diff))
}
