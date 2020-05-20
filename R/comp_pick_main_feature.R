#' pick_main_feature
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
pick_main_feature <- function(dt){

  all_g_samples <- colnames(dt)[grep('sample_\\d{1,}_g', colnames(dt))]

  main_features_dt <- dt[, pick_main_feature_sd(.SD), by=c('molecule_b', 'adduct_b'), .SDcols = c('feature_id_b', 'feature_id_g', 'isoabb_b', 'total_area_b', 'total_area_g', 'samples_to_compare', all_g_samples)]

  dt <- merge(dt, main_features_dt[,c('feature_id_b', 'feature_id_g', 'main_feature')], by=c('feature_id_b', 'feature_id_g'), all.x=TRUE)
  dt <- dt[, 'main_feature' := ifelse(main_feature == TRUE, TRUE, FALSE)]

  return(dt)
}


#' pick_main_feature_sd
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
pick_main_feature_sd <- function(dt){
  dt <- copy(dt)

  #Get list of all avaiable iso_abbs
  all_iso_abs <- sort(unique(dt[,isoabb_b]), decreasing = TRUE)

  if (nrow(dt)==length(all_iso_abs)){
    #If list of features is equal to number of unique iso abbs set all to main peak
    dt <- dt[, 'main_feature' := TRUE]
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
  } else if (length(all_iso_abs) == 1){
    #If only one is abb is present no comparison can be done! Choose featurte with higher compareable mean area
    dt$average_area <- apply(dt, 1, function(x){compare_samples <- paste0('sample_', unlist(x['samples_to_compare']), '_g')
    return(mean(unlist(x[compare_samples]), na.rm = TRUE))
    })
    dt <- dt[, 'main_feature' := ifelse(average_area == min(average_area), TRUE, FALSE)]
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
  } else {
    #Iso_abb Comparison
    dt[, merge_key := 1]

    #Check for features with same area, if present pick first one
    dt <- dt[!duplicated(dt, by=c('total_area_g'))]

    #Build compariosn DT
    comp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    comp_dt <- comp_dt[isoabb_b.x > isoabb_b.y]
    comp_dt <- comp_dt[, 'compare_ratio' := isoabb_b.y/isoabb_b.x]
    comp_dt[, 'group_temp_id' := .GRP, by=c('isoabb_b.x', 'isoabb_b.y')]
    comp_dt[, c('ratio_diff', 'min_ratio_diff') := best_feature_per_comparison(.SD), by=c('isoabb_b.x', 'isoabb_b.y')]
    x_dt <- setnames(comp_dt[min_ratio_diff == TRUE, c('feature_id_b.x', 'feature_id_g.x', 'ratio_diff')], c('feature_id_b.x', 'feature_id_g.x'), c('feature_id_b', 'feature_id_g'))
    y_dt <- setnames(comp_dt[min_ratio_diff == TRUE, c('feature_id_b.y', 'feature_id_g.y', 'ratio_diff')], c('feature_id_b.y', 'feature_id_g.y'), c('feature_id_b', 'feature_id_g'))
    main_features_dt <- rbindlist(list(x_dt, y_dt), use.names = TRUE)
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

best_feature_per_comparison <- function(dt){
  dt <- copy(dt)
  #checkT <<- dt
  dt$ratio_diff <- as.numeric(apply(dt, 1, function(x){compare_samples <- intersect(unlist(x['samples_to_compare.x']), unlist(x['samples_to_compare.y']))
                                                          if(length(compare_samples) < 1){
                                                            return(as.numeric(NULL))
                                                          }

                                                          compare_samples <- paste0('sample_', compare_samples, '_g')
                                                          ratio_errors <- list()
                                                          for (i in compare_samples){
                                                            if(i == 'sample__g'){
                                                              #Weird error encounterd before, not sure if fixed, check left in
                                                              stop("Sample_g error")
                                                            }
                                                            ratio_errors <- append(ratio_errors, (x[[paste0(i,'.y')]]/x[[paste0(i,'.x')]])/x[['compare_ratio']])
                                                          }
                                                          #return(Reduce('median',ratio_errors))
                                                          return(mean(unlist(ratio_errors)))
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
