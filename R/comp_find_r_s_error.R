#' find_r_s_error
#'
#' @param comp_id_b
#' @param molecule_b
#' @param adduct_b
#' @param sample_id_b
#' @param isoabb_b
#' @param peak_area_b
#' @param peak_area_ug
#'
#' @return
#' @export
#'
#' @examples
find_r_s_error <- function(comp_id_b, molecule_b, adduct_b, sample_id_b, isoabb_b, peak_area_b, peak_area_ug, peak_height_b){

  temp_dt <- data.table(comp_id_b, molecule_b, adduct_b, sample_id_b, isoabb_b,  peak_area_b, peak_area_ug, peak_height_b)

  temp_dt[, r_s_error := as.character(NA)]

  if (all(is.na(temp_dt$peak_area_ug))){
    first_found_ug_area <- NA
    first_found_ug_height <- NA
  } else {
    first_found_ug_area <- temp_dt[which.min(peak_area_ug), peak_area_b]
    first_found_ug_height <- temp_dt[which.min(peak_area_ug), peak_height_b]
  }

  #No UG Peaks where found
  if(is.na(first_found_ug_area)){
    temp_dt[, r_s_error := 'NF']
  }

  #UG Peaks where found
  else {
    #20% of first found area
    temp_dt[, first_found_area_temp := first_found_ug_area*2]
    temp_dt[is.na(peak_area_ug), r_s_error := ifelse((peak_area_b > first_found_ug_area * 1.5 & peak_height_b > first_found_ug_height * 1.5), 'R', 'S')]
  }
  temp_dt[is.na(r_s_error), r_s_error := 'F']

  return(temp_dt$r_s_error)
}
