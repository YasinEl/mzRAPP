#' generate_results_text
#'
#' @param comparison
#'
#' @return
#' @export
#'
#' @examples
generate_results_text <- function(comparison_data){
  UT_peaks <-
    nrow(rbindlist(list(comparison_data$c_table[main_peak == TRUE], comparison_data$nf_g), fill = TRUE))


  main_peak_table <- comparison_data$c_table[main_peak == TRUE]

  found_ug_peaks <- length(unique(main_peak_table$comp_id_ug))
  #split_features <- sum(main_peak_table[, count_split_features(feature_id_g), by=feature_id_b]$V1, na.rm=TRUE)

  results_text <- paste0("Assessed tool: ", comparison_data$info_list$algorithm, "     UT-peaks: ", UT_peaks,     "     ", "Found peaks: ",  found_ug_peaks, "/",
                         comparison_data$info_list$nr_of_b_peaks, " (", round((found_ug_peaks/comparison_data$info_list$nr_of_b_peaks)*100, 1), "%)     Peak fragments: ",  length(unique(comparison_data$split_table$comp_id_b)),
                         "     Missing Value (S|R): ", nrow(comparison_data$rs_table[missing_peaks == "S"]), "|", nrow(comparison_data$rs_table[missing_peaks == "R"]),
                         "     Pred. error increase >20%p: ", nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. > 20%p"]), "/", nrow(comparison_data$iso_err_dt[!is.na(diffH20PP_pp)]), " (",
                         round(nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. > 20%p"])/nrow(comparison_data$iso_err_dt[!is.na(diffH20PP_pp)]) * 100, 1), "%)",
                         "     Min. # of alignment errors: ", sum(comparison_data$ali_error_table$errors, na.rm = TRUE),  "     Found peaks (features): ",
                         nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE]), "/", comparison_data$info_list$nr_of_b_peaks, " (",
                         round(nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE])/comparison_data$info_list$nr_of_b_peaks * 100, 1), "%)",
                         "     Found features (features): ",
                         nrow(unique(comparison_data$feature_table[!is.na(area_b) &
                                                                     !is.na(area_g) &
                                                                     main_feature == TRUE,
                                                                   c("molecule_b", "adduct_b", "isoabb_b")], cols = c("molecule_b", "adduct_b", "isoabb_b"))), "/",
                         comparison_data$info_list$nr_of_b_features, " (",
                         round(nrow(unique(comparison_data$feature_table[!is.na(area_b) &
                                                                           !is.na(area_g) &
                                                                           main_feature == TRUE,
                                                                         c("molecule_b", "adduct_b", "isoabb_b")], cols = c("molecule_b", "adduct_b", "isoabb_b")))/
                           comparison_data$info_list$nr_of_b_features*100, 1), "%)")

  return(results_text)
}

#' count_split_features
#'
#' @param feature_id_ug
#'
#' @return
#' @export
#'
#' @examples
count_split_features <- function(feature_id_ug){
  if(length(unique(feature_id_ug))>1){return (TRUE)}
  else {return (FALSE)}
}

#generate_results_text(comparison_ug_g)
