#' classify_false_negative
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @noRd
classify_false_negative <- function(dt) {

  #check if feature ID = main ID
  dt <- dt[, false_negative := ifelse(is.na(feature_id_g), 'TRUE',
                                      ifelse(feature_id_g != main_feature, 'SPLIT','FALSE'))]



  dt <- dt[, feature_id_b_temp := feature_id_b]
  temp_groups <- dt[, if(any(false_negative == 'TRUE')) .SD, by=.(feature_id_b_temp)]
  temp_groups <- temp_groups[, if(!all(false_negative == 'TRUE')) .SD, by=.(feature_id_b_temp)]


  r_s_check <- function(group){


    group <- group[order(peak_area_b)]
    group <- group[, order_temp := .I]

    first_found_area <- min(which(!is.na(group$peak_area_g)))

    group <- group[, area_diff := abs(((peak_area_b-min(peak_area_b))*100)/min(peak_area_b))]
    group <- group[, false_negative_type := as.character(ifelse(order_temp < first_found_area & false_negative == 'TRUE', 'R',
                                                                ifelse(order_temp > first_found_area&false_negative == 'TRUE'&area_diff >=20, 'S',
                                                                       ifelse(order_temp > first_found_area&false_negative == 'TRUE'&area_diff <20, 'R', NA))))]
    return(group)
  }

  temp_groups <- temp_groups[, r_s_check(.SD), by=.(feature_id_b)]
  temp_groups <- temp_groups[, false_negative := paste(false_negative, false_negative_type, sep="_")]

  return(temp_groups)
}
