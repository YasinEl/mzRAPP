#' eliminate_duplicates_no_for
#'
#' @param comp_id_b
#' @param comp_id_ug
#' @param isoabb_b
#' @param peak_area_ug
#' @param peak_height_ug
#' @param peak_height_b
#'
#' @return
#' @export
#'
#' @examples
eliminate_duplicates_no_for <- function(comp_id_b, comp_id_ug, isoabb_b, peak_area_ug, peak_height_ug, peak_height_b){

  ##############
  #pick best iso 100 peak by comparing it to to iso group below
  #compare all other isoab peaks to this peak, pick as main peak the one with closest match to the expected area
  ##############

  #Convert input columns back to data.table
  dt <- data.table(comp_id_b, comp_id_ug, isoabb_b, peak_area_ug, peak_height_ug, peak_height_b)

  #Check if necesary to run
  if(any(duplicated(dt, by='comp_id_b'))){
    #Find Best 100 peak
    all_iso_groups <- sort(unique(dt$isoabb_b), decreasing = TRUE)
    if (length(all_iso_groups) > 1){

      #Compare isoabb 1 with isoabb 2
      peaks_1 <- dt[isoabb_b == all_iso_groups[1]][, merge_key := 1]
      peaks_2 <- dt[isoabb_b == all_iso_groups[2]][, merge_key := 1]
      merged_peaks <- merge(peaks_1, peaks_2, by='merge_key', all=FALSE, allow.cartesian=TRUE)
      merged_peaks[, diff_area := abs(((peak_area_ug.x*isoabb_b.y)/100)-peak_area_ug.y)]
      #Get M0 Peak with minimal area_diff
      m0_peak_id <- merged_peaks[diff_area == min(diff_area), comp_id_ug.x]
      m0_peak <- dt[comp_id_ug == m0_peak_id][, merge_key := 1]


      #Merge M0 to all other peaks
      rest_peaks <- dt[isoabb_b != all_iso_groups[1]][, merge_key := 1]
      merged_peaks <- merge(rest_peaks, m0_peak, by='merge_key', all=FALSE, allow.cartesian=TRUE)
      #Calculate area diff between m0 and all others
      merged_peaks[, diff_area := abs(((peak_area_ug.y*isoabb_b.x)/100)-peak_area_ug.x)]

      #select peaks per iso abb with lowest diff
      merged_peaks[, min_diff_area := min(diff_area), by=isoabb_b.x]
      merged_peaks <- merged_peaks[diff_area == min_diff_area]

      #Write IDs to list
      main_peak_list <- append(merged_peaks$comp_id_ug.x, m0_peak_id)

      #Add main_peak column and fill it according to main_peak_list
      dt <- dt[, main_peak := ifelse(comp_id_ug %in% main_peak_list, 'TRUE', 'FALSE')]

    } else if(length(all_iso_groups) == 1) {
      #Choose higher peak if only one iso group is present
      dt[, height_diff := abs(peak_height_b - peak_height_ug)]
      dt <- dt[, main_peak := ifelse(height_diff == min(height_diff), 'TRUE', 'FALSE')]
    }
  } else {
    #If no duplicate peaks are present mark all as main peak
    dt <- dt[, main_peak := 'TRUE']
  }
  return(dt$main_peak)
}
