#' generate_results_text
#'
#' @param comparison
#'
#' @return
#' @export
#'
#' @examples
generate_results_text <- function(comparison){

  #print(nrow(comparison$c_table[main_peak == TRUE]))
  #print(nrow(comparison$nf_b_table))
  #print(comparison$info_list)

  UT_peaks <-
    nrow(rbindlist(list(comparison$c_table[main_peak == TRUE], comparison$nf_g), fill = TRUE))


  main_peak_table <- comparison$c_table[main_peak == TRUE]

  #main_peak_table <- main_peak_table[, split_peak := ifelse(comp_id_b %in% comparison$split_table$comp_id_b, 'TRUE', 'FALSE')]

  main_peak_table <- main_peak_table[, main_feature := max(feature_id_g), by=.(molecule_b, adduct_b, isoabb_b)]

  fwrite(main_peak_table, 'splitpeak_main_debug.csv')



  found_ug_peaks <- length(unique(main_peak_table$comp_id_ug))
  print(found_ug_peaks)
  found_g_features <- length(unique(main_peak_table$main_feature))
  print(found_g_features)
  split_features <- sum(main_peak_table[, count_split_features(feature_id_g), by=feature_id_b]$V1, na.rm=TRUE)

  #results_text <- paste0('Untargeted tool: ', comparison$info_list$algorithm, '\n')
  #results_text <- paste0(results_text, 'Total Benchmark Peaks: ', comparison$info_list$nr_of_b_peaks, '\n')
  #results_text <- paste0(results_text, '-- Found Peaks: ', found_ug_peaks, ' (', round((found_ug_peaks/comparison$info_list$nr_of_b_peaks)*100, 2),'%)' , '\n')
  #results_text <- paste0(results_text, '-- Splitted Peaks: ', length(unique(comparison$split_table$comp_id_b)), ' (', round((length(unique(comparison$split_table$comp_id_b)))/(comparison$info_list$nr_of_b_peaks)*100, 2),'%)' , '\n')
  #results_text <- paste0(results_text, 'Total Benchmark Features: ', comparison$info_list$nr_of_b_features, '\n')
  #results_text <- paste0(results_text, '-- Found Features: ', found_g_features, ' (', round((found_g_features/comparison$info_list$nr_of_b_features)*100, 2),'%)' , '\n')
  #results_text <- paste0(results_text, '-- Split Features: ', split_features, ' (', round((split_features/found_g_features)*100, 2),'%)' , '\n')

  results_text <- paste0("Assessed tool: ", comparison$info_list$algorithm, "     UT-peaks: ", UT_peaks,     "     ", "Found peaks: ",  found_ug_peaks, "/",
                         comparison$info_list$nr_of_b_peaks, " (", round((found_ug_peaks/comparison$info_list$nr_of_b_peaks)*100, 1), "%)     Peak fragments: ",  length(unique(comparison$split_table$comp_id_b)))



  #print(results_text)
  #print(class(comparison$info_list$nr_of_b_features))
  #print(round((found_g_features/comparison$info_list$nr_of_b_features)*100, 2))
  #print(class(round((found_g_features/comparison$info_list$nr_of_b_features)*100, 2)))
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
