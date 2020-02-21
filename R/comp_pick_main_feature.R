#' pick_main_feature
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
pick_main_feature <- function(dt){
  #print(length(unique(dt$feature_id_b)))
  #print(nrow(dt))

  all_g_samples <- colnames(dt)[grep('sample_\\d{1,}_g', colnames(dt))]

  main_feature_dt <- rbind(dt[, pick_main_feature_sd(.SD), by=c('molecule_b', 'adduct_b'), .SDcols = c('feature_id_b', 'feature_id_g', 'isoabb_b', 'total_area_b', 'total_area_g', 'samples_to_compare', all_g_samples)])


  #print(nrow(main_feature_dt[main_feature==TRUE]))
  #print('-----')
  dt <- merge(dt, main_feature_dt[,c('feature_id_b', 'feature_id_g', 'main_feature')], by=c('feature_id_b', 'feature_id_g'), all.x=TRUE)
  #print(nrow(dt[main_feature==TRUE]))
  #print(nrow(dt))
  #temp_dup_dt <<- dt[duplicated(dt, by=c('molecule_b', 'adduct_b', 'isoabb_b')) | duplicated(dt, by=c('molecule_b', 'adduct_b', 'isoabb_b'), fromLast=TRUE)][main_feature==TRUE]
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
  ######################TO DO#################
  #Unify returns with error checks
  dt <- copy(dt)
  #Get list of all avaiable iso_abbs
  all_iso_abs <- sort(unique(dt[,isoabb_b]), decreasing = TRUE)
  if (nrow(dt)==length(all_iso_abs)){
    #If list of features is equal to number of unique is abbs set all to main peak
    dt <- dt[, 'main_feature' := TRUE]
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
  } else if(length(all_iso_abs) == 1){
    #DECIDING FACTOR??? - Pick feature with higher mean area
    dt$average_area <- apply(dt, 1, function(x){compare_samples <- paste0('sample_', unlist(x['samples_to_compare']), '_g')
                                                return(mean(unlist(x[compare_samples])))
                                               })
    dt <- dt[, 'main_feature' := ifelse(average_area == min(average_area), TRUE, FALSE)]
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
    #stop("Deciding Factor")
  } else {
    #Iso_abb Comparison
    dt[, merge_key := 1]

    #Check for features with same area, if present pick first one
    dt <- dt[!duplicated(dt, by=c('total_area_g'))]

    #Build compariosn DT
    temp_dt <- merge(dt, dt, by=c('merge_key'), allow.cartesian = TRUE)
    temp_dt <- temp_dt[isoabb_b.x > isoabb_b.y]
    temp_dt <- temp_dt[, 'compare_ratio' := isoabb_b.y/isoabb_b.x]
    #print(temp_dt)
    temp_dt$ratio_diff <- as.numeric(apply(temp_dt, 1, function(x){compare_samples <- intersect(unlist(x['samples_to_compare.x']), unlist(x['samples_to_compare.y']))
                                                        if(length(compare_samples) < 1){
                                                          return(as.numeric(NULL))
                                                        }
                                                        compare_samples <- paste0('sample_', compare_samples, '_g')
                                                        ratio_errors <- list()
                                                        for (i in compare_samples){
                                                          #x[[paste0(i,'.y')]]
                                                          if(i == 'sample__g'){
                                                            #print(compare_samples)
                                                            #print(x)
                                                            #print(as.data.table(x))
                                                            stop("Sample_g error")
                                                          }
                                                          ratio_errors <- append(ratio_errors, (x[[paste0(i,'.y')]]/x[[paste0(i,'.x')]])/x[['compare_ratio']])
                                                        }
                                                        #print(Reduce('sum',ratio_errors))
                                                        #print(typeof(Reduce('sum',ratio_errors)))
                                                        if(length(Reduce('sum',ratio_errors))<1){
                                                          #print(ratio_errors)
                                                          stop("ratio_error")
                                                        }
                                                        return(Reduce('sum',ratio_errors))
                                                       }))
    temp_dt <- temp_dt[!is.na(ratio_diff)]
    #print(temp_dt, class=TRUE)
    #Calculate %difference from expected ratio
    #temp_dt <- temp_dt[,ratio_diff := (total_area_g.y/total_area_g.x)/(isoabb_b.y/isoabb_b.x)]
    #Assign row_id, group_id (comparison groups) and id within group
    temp_dt <- temp_dt[, row_id := seq_len(.N)]
    temp_dt <- temp_dt[, grp_id := .GRP, by=c('isoabb_b.x', 'isoabb_b.y')]
    temp_dt <- temp_dt[, id_in_group := seq_len(.N), by=c('isoabb_b.x', 'isoabb_b.y')]

    #temp_dt <- temp_dt[, grp_id := .GRP, by=c('feature_id_b.x')]
    #temp_dt <- temp_dt[, id_in_group := seq_len(.N), by=c('feature_id_b.x')]


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

    new_combination_dt <- combination_dt

    new_combination_dt$rows <- apply(new_combination_dt,1,function(x){paste(x, collapse = ',')})
    new_combination_dt <- melt(new_combination_dt, id.vars='rows', variable.name='position', value.name='row_id')
    new_combination_dt <- new_combination_dt[temp_dt[,c('row_id', 'ratio_diff')], on=c('row_id')]
    new_ratio_error_dt <- new_combination_dt[, sum(ratio_diff), by=c('rows')]
    setnames(new_ratio_error_dt, c('V1'), c('error'))
    new_ratio_error_dt <- new_ratio_error_dt[, 'correct_rows' := ifelse(error == min(error), TRUE, FALSE)]
    ratio_error_dt <- new_ratio_error_dt[order(error)]
    if(nrow(ratio_error_dt[correct_rows == TRUE]) != 1){
      print('here')
      ratio_error_dt <- ratio_error_dt[1,][correct_rows == TRUE]
      print(ratio_error_dt)
    }

    if(nrow(ratio_error_dt[correct_rows == TRUE]) == 1){
      temp_dt <- temp_dt[, 'main_feature' := ifelse(row_id %in% unlist(strsplit(ratio_error_dt[correct_rows == TRUE, rows], ',')), TRUE, FALSE)]
      main_features <- unique(append(temp_dt[main_feature == TRUE, feature_id_g.x], temp_dt[main_feature == TRUE, feature_id_g.y]))
      dt <- dt[, 'main_feature' := ifelse(feature_id_g %in% main_features, TRUE, FALSE)]
      if(sum(dt$main_feature) != length(unique(dt$feature_id_b))){
        #print(dt)
        double_b_features <- unique(dt[duplicated(dt, by=c('feature_id_b'))]$feature_id_b)
        #print(double_b_features)
        selected_main_features <- list()
        for (f in double_b_features){
          #print(f)
          b_feature_dt <- temp_dt[(feature_id_b.x == f | feature_id_b.y == f)]
          main_comparison <- b_feature_dt[ratio_diff == min(ratio_diff)]
          #print(main_comparison)
          if (main_comparison[1, feature_id_b.x] == f){
            chosen_g_feature <- main_comparison[1, feature_id_g.x]
          } else {
            chosen_g_feature <- chosen_g_feature <- main_comparison[1, feature_id_g.y]
          }
          #print(chosen_g_feature)
          selected_main_features <- rbind(selected_main_features, list('feature_id_b' = f, 'feature_id_g' = chosen_g_feature, 'main_feature_override' = TRUE))

          #print('---')
        }
        selected_main_features_dt <- as.data.table(selected_main_features)
        selected_main_features_dt[, ':=' (feature_id_g = as.integer(feature_id_g), feature_id_b = as.integer(feature_id_b), main_feature_override = as.logical(main_feature_override))]
        #print(dt[,c('feature_id_b', 'feature_id_g')], class=TRUE)
        #print(selected_main_features_dt, class=TRUE)
        dt <- merge(dt, selected_main_features_dt, by=c('feature_id_b', 'feature_id_g'), all.x=TRUE)
        dt <- dt[, double_feature := ifelse(feature_id_b %in% double_b_features, TRUE, FALSE)]
        dt <- dt[, main_feature := ifelse(((is.na(main_feature_override)) & (double_feature == TRUE)), FALSE, main_feature)]
        dt <- dt[, ':='(main_feature_override = NULL, double_feature = NULL)]
        #print(dt, class=TRUE)
        if(sum(dt$main_feature) != length(unique(dt$feature_id_b))){
          print('-------------------')
          print(sum(dt$main_feature))
          print(length(unique(dt$feature_id_b)))
          print(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
          stop()
        }
      }
    }
    return(dt[, c('feature_id_b', 'feature_id_g', 'main_feature')])
  }
}
