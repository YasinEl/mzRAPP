#' compare_peaks
#'
#' Matches peaks reported in the aligned and unaligned non-targeted output against the provided benchmark as prepared by \code{\link{check_benchmark_input}}. For details on how this matching procedure is conducted please check the mzRAPP readme
#' \url{https://github.com/YasinEl/mzRAPP#matching-between-bm-and-npp-output-background}.
#'
#' @param b_table output from \code{\link{check_benchmark_input}}
#' @param ug_table one of the listed objects (ug_table) in output of \code{\link{check_nonTargeted_input}}. e.g. check_nonTargeted_input_output$ug_table
#' @param g_table one of the listed objects (g_table) in output of \code{\link{check_nonTargeted_input}}.  e.g. check_nonTargeted_input_output$g_table
#' @param algo output format of ug_table and g_table. Can be "XCMS", "XCMS3", "Metaboanalyst", "El-Maven", "OpenMS", "MS-DIAL", "CompoundDiscoverer" or "MZmine 2"
#'
#' @return returns list containing different tables including data from different types of comparisons
#'
#' @details The output of this function is a list containing 9 different elements. In the following benchmark peaks are referred to as BM, non-targeted unaligned peaks as NP and NP which have been aligned across samples (features) as NF. In all
#' following tables (provided as data.table objects) benchmark variables are labeled with a "_b", unaligned non-targeted peaks via "_ug" and aligned non-targeted peaks via "_g" suffix.
#' @details \strong{BM_NPPoutput_size:} Gives peak and aligned feature counts for the used benchmark as well as non-targeted outputs
#' @details nr_of_b_peaks: benchmark peak count
#' @details nr_of_b_features: benchmark feature count
#' @details nr_of_ug_peaks: NP count
#' @details nr_of_g_peaks: number of peaks reported in the aligned output
#' @details nr_of_g_features: NF count
#' @details algorithm: non-targeted output format
#' @details \strong{Overview_per_molecule:} Provides overview of different performance metrics per benchmark molecule. (metrics are summed up over whole molecule)
#' @details molecule_b: molecule name
#' @details Min.er: count of alignment errors as defined in the mzRAPP readme
#' @details BM.div: count of alignment divergences as defined in the mzRAPP readme
#' @details lost: count of NP for which no match among NF was found (NP lost during alignment)
#' @details R_pp: count of random (high) missing values in the unaligned non-targeted output as defined in mzRAPP readme
#' @details S_pp: count of systematic (low) missing values in the unaligned non-targeted output as defined in mzRAPP readme
#' @details R_ft: count of random (high) missing values in the aligned non-targeted output as defined in mzRAPP readme
#' @details S_ft: count of systematic (low) missing values in the aligned non-targeted output as defined in mzRAPP readme
#' @details IRb_ok_pp: count of isotopologue ratios calculated from NP classified as "good" as defined in the mzRAPP readme
#' @details IRb_off_pp: count of isotopologue ratios calculated from NP classified as "bad" as defined in the mzRAPP readme
#' @details IRb_ok_ft: count of isotopologue ratios calculated from NF classified as "good" as defined in the mzRAPP readme
#' @details IRb_off_ft: count of isotopologue ratios calculated from NF classified as "bad" as defined in the mzRAPP readme
#' @details Split_peaks: count of splitted peaks as defined in the mzRAPP readme
#' @details Found_peaks_ft: count of BP for which a match among NF was detected. For details on the matching procedure see the mzRAPP readme
#' @details Not_Found_peaks_ft: count of BP for which no match among NF was detected. For details on matching procedure see the mzRAPP readme
#' @details Found_peaks_pp: count of BP for which a match among NP was detected. For details on matching procedure see the mzRAPP readme
#' @details Not_Found_peaks_pp: count of BP for which no match among NP was detected. For details on matching procedure see the mzRAPP readme
#' @details Extra_feature_matches_ft: Number of additional NF matching on a given benchmark feature on top of best match
#' @details Extra_peak_matches_pp: Number of additional NP matching on a given BP on top of the best match
#' @details \strong{Matches_BM_NPPpeaks:} Extensive table allowing to inspect matches between the benchmark and the unaligned non-targeted output in detail.
#' Unaligned non-targeted peaks are also matched to aligned non-targeted peaks via reported abundance values in order to count alignment errors.
#' In order to allow the inspection of all input variables all variables available in benchmark and the non-targeted output are kept. It is worth noting that more than one match
#' can occur per BP. For the best match (as defined in the mzRAPP readme) the variable main_peak equals TRUE.
#' @details \strong{Unmatched_BM_NPPpeaks:} Benchmark peaks for which no match could be found.
#' @details \strong{SplittedMatches_BM_NPPpeaks:} All splitted matches (as defined in the mzRAPP readme) are summarized in this table. Splitted matches for which also a full match exists in
#' in the Matches_BM_NPPpeaks table have "present_in_found" = TRUE
#' @details \strong{MissingPeak_classification:} Classifies the missing value status of peaks before and after alignment. This classification
#' is made only correct alignment of the benchmark is in general agreement with the alignment of the non-targeted output per molecule (as described in mzRAPP readme). Peaks for which this was confirmed
#' show "Connected" = TRUE. The MissingPeak status is determined for the unaligned ("missing_peaks_ug") as well as the aligned ("missing_peaks_g") output individually and can hold the following values.
#' @details F (found): Peak is not missing
#' @details NC (not confirmable): alignment could not be confirmed as described above
#' @details L (lost): all peaks of the respective features were not found
#' @details R (random): random (high) missing value as defined in mzRAPP readme
#' @details S (systematic): systematic (low) missing value as defined in mzRAPP readme
#' @details \strong{IT_ratio_biases:} Table showing the status of calculated isotopologue ratio biases as compared to enviPat predicted ratios calculated from abundances reported
#' from BP ("benchmark"), NP ("NPP_peak picking") and NF ("NPP_features"). Biases are classified for NP ("diffH20PP_pp") and NF ("diffH20PP_ft") into good ("Inc. < 20%p") or bad ("Inc. > 20%p")
#' as defined in the mzRAPP readme.
#' @details \strong{AlignmentErrors_per_moleculeAndAdduct:} Overview of alignment problems per molecule and adduct.
#' @details Min.errors: count of alignment errors as defined in the mzRAPP readme
#' @details BM.div: count of alignment divergences as defined in the mzRAPP readme
#' @details Lost_b.A: count of NP for which no match among NaP was found (NP lost during alignment)
#' @details \strong{Matches_BM_NPPpeaks_NPPfeatures:} Table containing matched non-targeted unaligned and aligned peaks for each BP with their respective reported abundances.
#' @details min_mz_start: lowest mz allowed for matched NF to be considered (as described in mzRAPP readme)
#' @details max_mz_end: highest mz allowed for matched NF to be considered (as described in mzRAPP readme)
#' @details min_rt_start: lowest RT allowed for matched NF to be considered (as described in mzRAPP readme)
#' @details max_rt_end: highest RT allowed for matched NF to be considered (as described in mzRAPP readme)
#' @details peak_area_b: peak area as reported in benchmark
#' @details peak_area_ug: peak area as reported in unaligned NP
#' @details area_g: peak area as reported in NF
#'
#'
#' @importFrom data.table data.table is.data.table
#'
#' @export
#'
compare_peaks <- function(b_table, ug_table, g_table, algo){

  #If no g_table exists create empty one
  if(is.null(g_table)){
    g_table = data.table('comp_id_g' = integer(),
                         'sample_id_g' = integer(),
                         'rt_start_g' = double(),
                         'rt_end_g' = double(),
                         'rt_g' = double(),
                         'mz_g' = double(),
                         'sample_name_g' = character(),
                         'peak_area_g' = double(),
                         'feature_id_g' = integer()
    )
  }


  #check format of data
  if (!is.data.table(b_table) | !is.data.table(ug_table) | !is.data.table(g_table)){
    stop('Benchmark, ungrouped and/or grouped dataset are not type DataTable')
  }

  #Benchmark table Check
  b_req_cols <- c('comp_id_b', 'sample_id_b', 'rt_start_b', 'rt_end_b', 'rt_b', 'mz_start_b',
                  'mz_end_b', 'molecule_b', 'adduct_b', 'sample_name_b', 'peak_area_b', 'peak_height_b')


  if(!all(b_req_cols %in% colnames(b_table))){
    cols_not_found <- setdiff(b_req_cols, colnames(b_table))
    stop('Columns not present in benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #UG table check
  ug_req_cols <- c('comp_id_ug', 'sample_id_ug', 'rt_start_ug', 'rt_end_ug', 'rt_ug', 'mz_ug', 'sample_name_ug', 'peak_area_ug')


  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns not present in ug dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #G table check
  g_req_cols <- c('comp_id_g', 'sample_id_g', 'rt_g', 'mz_g',
                  'sample_name_g', 'peak_area_g', 'feature_id_g')

  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns not present in g dataset: ', paste0(cols_not_found, sep = " - "))
  }


  #Write relevant NPP information to info list
  BM_NPPoutput_size <- list()
  BM_NPPoutput_size <- append(BM_NPPoutput_size, list(nr_of_b_peaks = length(unique(b_table$comp_id_b)),
                                                      nr_of_b_features = length(unique(b_table$feature_id_b)),
                                                      nr_of_ug_peaks = length(unique(ug_table$comp_id_ug)),
                                                      nr_of_g_peaks = length(unique(g_table$comp_id_g)),
                                                      nr_of_g_features = length(unique(g_table$feature_id_g)),
                                                      algorithm = algo))


  #Check for duplicate peaks
  total_ug_peaks <- nrow(ug_table)
  ug_table <- ug_table[!duplicated(ug_table, by = c('rt_ug', 'mz_ug', 'peak_area_ug'))]

  if(total_ug_peaks - nrow(ug_table) > 0){
    message(paste0('Ignored ',total_ug_peaks - nrow(ug_table), ' duplicated non-aligned peaks from non-targeted output (identical rt, mz and area)'))
  }


  #Add peak core borders
  #b_table[, ':=' (peak_core_rt_range_start_b = rt_b - min(rt_end_b - rt_b,  rt_b - rt_start_b) * 0.3,
  #                peak_core_rt_range_end_b = rt_b + min(rt_end_b - rt_b,  rt_b - rt_start_b) * 0.3)]
  b_table[, rt_add_temp := ifelse((rt_end_b - rt_b) < (rt_b - rt_start_b),
                                  rt_end_b - rt_b,  rt_b - rt_start_b)]

  b_table[, ':=' (peak_core_rt_range_start_b = rt_b - rt_add_temp*0.3,
                  peak_core_rt_range_end_b = rt_b + rt_add_temp*0.3)]


  ##############
  #Start comparison!
  ##############
  message("Starting to compare benchmark with non-targeted output")


  #Finding matches between benchmark and ug_peaks
  Matches_BM_NPPpeaks <- match_peaks_to_benchmark(b_table,
                                                  ug_table)

  Matches_BM_NPPpeaks_all <- data.table::copy(Matches_BM_NPPpeaks)

  Unmatched_BM_NPPpeaks <- b_table[!b_table$comp_id_b %in% unique(Matches_BM_NPPpeaks[main_peak == TRUE]$comp_id_b)]


  #Finding split peaks
  SplittedMatches_BM_NPPpeaks <- match_peaks_to_benchmark_split(b_table,
                                                                ug_table)


  #Checking in which aligned features NPP peaks are
  match_tables_with_alignment_recovery_info <- match_NPPpeaks_to_NPPfeatures(Matches_BM_NPPpeaks_all,
                                                                             SplittedMatches_BM_NPPpeaks,
                                                                             g_table)

  Matches_BM_NPPpeaks_all <- data.table::copy(match_tables_with_alignment_recovery_info[["Matches_BM_NPPpeaks"]])
  Matches_BM_NPPpeaks <- data.table::copy(match_tables_with_alignment_recovery_info[["Matches_BM_NPPpeaks"]][main_peak == TRUE])
  SplittedMatches_BM_NPPpeaks <- match_tables_with_alignment_recovery_info[["SplittedMatches_BM_NPPpeaks"]]

  #Generate alignment error table
  AlignmentErrors_per_moleculeAndAdduct <- assess_alignment(Matches_BM_NPPpeaks[main_peak == TRUE],
                                                            Unmatched_BM_NPPpeaks,
                                                            g_table)



  #Generate feature table
  Matches_BM_NPPpeaks_NPPfeatures <- match_features_to_benchmark(g_table,
                                                                 b_table,
                                                                 Matches_BM_NPPpeaks[main_peak == TRUE],
                                                                 Unmatched_BM_NPPpeaks)



  Matches_BM_NPPpeaks_NPPfeatures_all <- data.table::copy(Matches_BM_NPPpeaks_NPPfeatures)
  Matches_BM_NPPpeaks_NPPfeatures <- data.table::copy(Matches_BM_NPPpeaks_NPPfeatures[main_feature == TRUE | is.na(main_feature)])


  #Generate Random and systematic error DT
  MissingPeak_classification <- check_missing_peaks(Matches_BM_NPPpeaks[main_peak == TRUE],
                                                    Unmatched_BM_NPPpeaks,
                                                    Matches_BM_NPPpeaks_NPPfeatures,
                                                    g_table)




  #Generate Isotopologe error dt
  IT_ratio_biases <- check_IR_biases(Matches_BM_NPPpeaks[main_peak == TRUE],
                                     Matches_BM_NPPpeaks_NPPfeatures[main_feature == TRUE | is.na(main_feature)],
                                     g_table,
                                     b_table)


  #create overview table per compound
  sum_tab <- metrics_per_molecule(Matches_BM_NPPpeaks_all,
                                  Unmatched_BM_NPPpeaks,
                                  Matches_BM_NPPpeaks_NPPfeatures_all,
                                  IT_ratio_biases,
                                  SplittedMatches_BM_NPPpeaks,
                                  MissingPeak_classification,
                                  AlignmentErrors_per_moleculeAndAdduct)



  #Return in a list
  return_list <- list('BM_NPPoutput_size' = BM_NPPoutput_size,
                      'Overview_per_molecule' = sum_tab,
                      'Matches_BM_NPPpeaks' = Matches_BM_NPPpeaks_all,
                      'Unmatched_BM_NPPpeaks' = Unmatched_BM_NPPpeaks,
                      'SplittedMatches_BM_NPPpeaks' = SplittedMatches_BM_NPPpeaks,
                      'MissingPeak_classification'= MissingPeak_classification,
                      'IT_ratio_biases' = IT_ratio_biases,
                      'AlignmentErrors_per_moleculeAndAdduct' = AlignmentErrors_per_moleculeAndAdduct,
                      'Matches_BM_NPPpeaks_NPPfeatures' = Matches_BM_NPPpeaks_NPPfeatures_all)

  message('Done!')

  return(return_list)
}
