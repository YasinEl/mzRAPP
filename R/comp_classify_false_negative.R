#' classify_false_negative
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
classify_false_negative <- function(dt) {

  #check if feature ID = main ID
  dt <- dt[, false_negative := ifelse(is.na(feature_id_g), 'TRUE',
                                      ifelse(feature_id_g != main_feature, 'SPLIT','FALSE'))]



  dt <- dt[, feature_id_b_temp := feature_id_b]
  fwrite(dt, file="debug.csv")
  temp_groups <- dt[, if(any(false_negative == 'TRUE')) .SD, by=.(feature_id_b_temp)]
  temp_groups <- temp_groups[, if(!all(false_negative == 'TRUE')) .SD, by=.(feature_id_b_temp)]


  r_s_check <- function(group){


    group <- group[order(peak_area_b)]
    group <- group[, order_temp := .I]

    #print(identical(pre_order, post_order))

    first_found_area <- min(which(!is.na(group$peak_area_g)))

    group <- group[, area_diff := abs(((peak_area_b-min(peak_area_b))*100)/min(peak_area_b))]
    group <- group[, false_negative_type := as.character(ifelse(order_temp < first_found_area & false_negative == 'TRUE', 'R',
                                                                ifelse(order_temp > first_found_area&false_negative == 'TRUE'&area_diff >=20, 'S',
                                                                       ifelse(order_temp > first_found_area&false_negative == 'TRUE'&area_diff <20, 'R', NA))))]
    if (group$feature_id_b[1]==406) {
      print(first_found_area)
      print(group$false_negative)
      fwrite(group, file="group_debug.csv")
      print('------')
    }
    return(group)
  }

  temp_groups <- temp_groups[, r_s_check(.SD), by=.(feature_id_b)]
  temp_groups <- temp_groups[, false_negative := paste(false_negative, false_negative_type, sep="_")]


  #Split-Apply-Combine aproach, propably very inefficent, try to vectorise or use lapply!!!!!
  #all_temp_groups <- split(temp_groups, by=c('feature_id_b'))
  #return_groups <- list()
  #for (group in all_temp_groups) {
  #  group <- group[order(peak_area_b)]
  #  group <- group[, b_order_temp := .N]
  #}

  return(temp_groups)
}
