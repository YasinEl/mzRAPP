#' compare_peaks
#'
#' Matches peaks reported in the aligned and unaligned non-targeted output against the provided benchmark as prepared by \code{\link{import_benchmark}}. For details on how this matching procedure is conducted please check the mzRAPP readme.
#'
#' @param b_table output from \code{\link{import_benchmark}}
#' @param ug_table one of the listed objects (ug_table) in output of \code{\link{pick_algorithm}}. e.g. pick_algo_output$ug_table
#' @param g_table one of the listed objects (g_table) in output of \code{\link{pick_algorithm}}.  e.g. pick_algo_output$g_table
#' @param algo string of length 1. output format of ug_table and g_table. Can be "XCMS", "El-Maven", "OpenMS", "msDial", "CompoundDiscoverer" or "mzMine"
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
#' @details \strong{Overview_per_molecule:} Provides overview of different performance metrics per benchmark molecule.
#' @details molecule_b: molecule name
#' @details Min.er: count of alignment errors as defined in the mzRAPP readme
#' @details BM.div: count of alignment divergences as defined in the mzRAPP readme
#' @details lost: count of NP for which no match among NaP was found (NP lost during alignment)
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
#' @details \strong{Matches_BM_NPPpeaks:} Extensive table allowing to inspect matches between the benchmark and the unaligned non-targeted output in detail.
#' Unaligned non-targeted peaks are also matched to aligned non-targeted peaks via reported abundance values in order to count alignment errors.
#' In order to allow the inspection of all input variables all variables available in benchmark and the non-targeted output are kept. It is worth noting that more than one match
#' can occur per BP. For the best match (as defined in the mzRAPP readme) the variable main_peak equals TRUE.
#' @details \strong{Unmatched_BM_NPPpeaks:} Benchmark peaks for which no match could be found.
#' @details \strong{SplittedMatches_BM_NPPpeaks:} All splitted matches (as defined in the mzRAPP readme) are summarized in this table. Splitted matches for which also a full match exists in
#' in the Matches_BM_NPPpeaks table have "present_in_found" = TRUE
#' @details \strong{MissingPeak_classification:} Classifies the missing value status of peaks before and after alignment. This classification
#' is made only correct alignment of the benchmark is in general agreement with the alignment of the non-targeted output per molecule (as described in mzRAPP readme). Peaks for which this was confirmed
#' show "Connected" = TRUE. The MissingPeak status is done for the unaligned ("missing_peaks_ug") as well as the aligned ("missing_peaks_g") output individually and can hold the following values.
#' @details F (found): Peak is not missing
#' @details NC (not confirmable): alignment could not be confirmed as described above
#' @details L (lost): all peaks of the respective features were not found
#' @deatils R (random): random (high) missing value as defined in mzRAPP readme
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
#' @export
#'
compare_peaks <- function(b_table, ug_table, g_table, algo){

  BM_NPPoutput_size <- list()

  #If no g_table exists crate empty one
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

  ##############
  #Make sure input data is in correct formats
  ##############

  if (!is.data.table(b_table) | !is.data.table(ug_table) | !is.data.table(g_table)){
    stop('Benchmark, ungrouped and/or grouped dataset are not type DataTable')
  }

  ##############
  #Check if all necessary columns are present
  ##############

  #Benchmark table Check
  b_req_cols <- c('comp_id_b', 'sample_id_b', 'rt_start_b', 'rt_end_b', 'rt_b', 'mz_start_b',
                  'mz_end_b', 'molecule_b', 'adduct_b', 'sample_name_b', 'peak_area_b', 'peak_height_b')


  if(!all(b_req_cols %in% colnames(b_table))){
    cols_not_found <- setdiff(b_req_cols, colnames(b_table))
    stop('Columns not present in benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #UG table check
  #ug_req_cols <- c('comp_id_ug', 'sample_id_ug', 'rt_start_ug', 'rt_end_ug', 'rt_ug', 'mz_ug', 'sample_name_ug', 'peak_area_ug', 'peak_height_ug')
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

  ##############
  #Write relevant information to info list
  ##############
  BM_NPPoutput_size <- append(BM_NPPoutput_size, list(nr_of_b_peaks = length(unique(b_table$comp_id_b)),
                                      nr_of_b_features = length(unique(b_table$feature_id_b)),
                                      nr_of_ug_peaks = length(unique(ug_table$comp_id_ug)),
                                      nr_of_g_peaks = length(unique(g_table$comp_id_g)),
                                      nr_of_g_features = length(unique(g_table$feature_id_g)),
                                      algorithm = algo))


  ##############
  #Check for duplicate peaks
  ##############

  #if (any(duplicated(b_table, by = c('rt_b', 'mz_b', 'peak_height_b')))){
  #  print('Duplicate peaks present in benchmark. This can lead to further errors during analysis.')
  #}
  #if (any(duplicated(ug_table, by = c('rt_ug', 'mz_ug', 'peak_height_ug')))){
  #  print('Duplicate peaks present in ungrouped dataset. This can lead to further errors during analysis.')
    total_ug_peaks <- nrow(ug_table)
    ug_table <- ug_table[!duplicated(ug_table, by = c('rt_ug', 'mz_ug', 'peak_area_ug'))]
    message(paste0('Removed ',total_ug_peaks - nrow(ug_table), ' duplicated non-aligned peaks from non-targeted output (identical rt, mz and area)'))
  #}
  #if (any(duplicated(g_table, by = c('rt_g', 'mz_g', 'peak_area_g')))){
  #  print('Duplicate peaks present in grouped. This can lead to further errors during analysis.')
  #}

  ##############
  #Start comparison!
  ##############


    message("Staring to compare benchmark with non-targeted output")
  ##############
  #Generating minimum peak bounderies in benchmark
  #Untrageted rt range must completely envelope these bounderies
  #Defined as taking the shorter of rt_start_b to rt_b or rt_end_b to rt_b,
  #taking 50% of this distance, adding and subtracting it from rt_b
  ##############
  b_table[, rt_add_temp := ifelse((rt_end_b - rt_b) < (rt_b - rt_start_b),
                                  rt_end_b - rt_b,  rt_b - rt_start_b)]
  b_table[, ':=' (new_rt_start_b = rt_b - rt_add_temp*0.3,new_rt_end_b = rt_b + rt_add_temp*0.3)]


  #Creating temp columns to prevent over-writing by join
  ug_table[, ':=' (sample_id_ug_temp = sample_id_ug,
                   rt_start_ug_temp = rt_start_ug,
                   rt_end_ug_temp = rt_end_ug,
                   rt_ug_temp = rt_ug,
                   mz_ug_temp = mz_ug)]


  b_table[, ':=' (sample_id_b_temp = sample_id_b,
                  new_rt_start_b_temp = new_rt_start_b,
                  new_rt_end_b_temp = new_rt_end_b,
                  mz_start_b_temp = mz_start_b - 0.0002,
                  mz_end_b_temp = mz_end_b + 0.0002)]


  ##############
  #Conducting non-equi join.
  #rt range must be larger on both sides than calculated peak limits,
  #mz must fall within mz start and end of benchmark
  ##############
  Matches_BM_NPPpeaks <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                    new_rt_start_b_temp >= rt_start_ug_temp,
                                    new_rt_end_b_temp <= rt_end_ug_temp,
                                    mz_start_b_temp <= mz_ug_temp,
                                    mz_end_b_temp >= mz_ug_temp,
                                    new_rt_start_b_temp < rt_ug_temp,
                                    new_rt_end_b_temp > rt_ug_temp),
                     allow.cartesian=TRUE, nomatch=NULL, mult='all']




  ##############
  #Finding split peaks by checking if only rt_start or rt_end of ug file fits inside the boundaries
  ##############

  #Find Peaks to the left of benchmark bounderies
  split_left_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                    new_rt_start_b_temp >= rt_start_ug_temp,
                                    new_rt_start_b_temp <= rt_end_ug_temp,
                                    new_rt_end_b_temp >= rt_end_ug_temp,
                                    mz_start_b_temp <= mz_ug_temp,
                                    mz_end_b_temp >= mz_ug_temp),
                     allow.cartesian=TRUE, nomatch=NULL, mult='all']


  #Find Peaks to the right of benchmark bounderies
  split_right_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                             new_rt_start_b_temp <= rt_start_ug_temp,
                                             new_rt_end_b_temp >= rt_start_ug_temp,
                                             new_rt_end_b_temp <= rt_end_ug_temp,
                                             mz_start_b_temp <= mz_ug_temp,
                                             mz_end_b_temp >= mz_ug_temp),
                              allow.cartesian=TRUE, nomatch=NULL, mult='all']

  #Find Peaks inside of benchmark boundaries
  split_middle_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                              new_rt_start_b_temp <= rt_start_ug_temp,
                                              new_rt_end_b_temp >= rt_end_ug_temp,
                                              mz_start_b_temp <= mz_ug_temp,
                                              mz_end_b_temp >= mz_ug_temp),
                               allow.cartesian=TRUE, nomatch=NULL, mult='all']

  #Combine the split peak tables
  SplittedMatches_BM_NPPpeaks <- rbindlist(list('split_left_table' = split_left_table, 'split_right_table' = split_right_table, 'split_middle_table' = split_middle_table), fill=TRUE, use.names = TRUE, idcol='file')



  #print(paste('Before Main Peak Check: ', nrow(Matches_BM_NPPpeaks)))
  Matches_BM_NPPpeaks <- pick_main_peak(Matches_BM_NPPpeaks)
  message(paste('Number of benchmark peaks with unaligned non-targeted peak matches: ', nrow(Matches_BM_NPPpeaks[main_peak == TRUE])))


  Matches_BM_NPPpeaks <- Matches_BM_NPPpeaks[main_peak == TRUE]

  ##############
  #Joining peaks from groupd file onto found areas of ungrouped
  #Could be a problem with rounded areas
  ##############


  #Creating temp columns to prevent over-writing by join
  #If statement is solution for msdial

  if ('peak_area_rounded_ug' %in% colnames(Matches_BM_NPPpeaks)){
    Matches_BM_NPPpeaks[, peak_area_ug_temp := peak_area_rounded_ug]
    Matches_BM_NPPpeaks[, sample_id_b_temp := sample_id_b]
    SplittedMatches_BM_NPPpeaks[, peak_area_ug_temp := peak_area_rounded_ug]
    g_table[, peak_area_g_temp := peak_area_g]
    g_table[, sample_id_g_temp := sample_id_g]
  } else {
    Matches_BM_NPPpeaks[, peak_area_ug_temp := peak_area_ug]
    Matches_BM_NPPpeaks[, sample_id_b_temp := sample_id_b]
    SplittedMatches_BM_NPPpeaks[, peak_area_ug_temp := peak_area_ug]
    g_table[, peak_area_g_temp := peak_area_g]
    g_table[, sample_id_g_temp := sample_id_g]
  }

  #Join
  Matches_BM_NPPpeaks <- g_table[Matches_BM_NPPpeaks, on=.(peak_area_g_temp == peak_area_ug_temp, sample_id_g_temp == sample_id_b_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']


  #In case of duplicate area matches during ug - g match take the g-match which occurred most often for other peaks in the same BM feature
  Matches_BM_NPPpeaks[, N_fid := .N, by = .(molecule_b, adduct_b, isoab_b, feature_id_g)]
  Matches_BM_NPPpeaks <-  Matches_BM_NPPpeaks[order(-rank(N_fid))][, !"N_fid"]
  Matches_BM_NPPpeaks <- unique(Matches_BM_NPPpeaks, by = c("molecule_b", "adduct_b", "isoab_b", "sample_id_b"))

  #Replace 0 in peak_area_g with NA (no idea why they appear in the first place)(maybe int64?)
  #Matches_BM_NPPpeaks <- Matches_BM_NPPpeaks[, peak_area_g := ifelse(peak_area_g == 0, NA, peak_area_g)]

  SplittedMatches_BM_NPPpeaks <- g_table[SplittedMatches_BM_NPPpeaks, on=.(peak_area_g_temp == peak_area_ug_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']

  #Remove _temp Columns
  Matches_BM_NPPpeaks[,grep('_temp$', colnames(Matches_BM_NPPpeaks)):=NULL]
  b_table[,grep('_temp$', colnames(b_table)):=NULL]


  Matches_BM_NPPpeaks[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]
  SplittedMatches_BM_NPPpeaks[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]

  SplittedMatches_BM_NPPpeaks[, present_in_found := ifelse(id_b_ug %in% Matches_BM_NPPpeaks$id_b_ug, 'TRUE', 'FALSE')]


  #Make sure main peaks only occure once
  if (any(duplicated(Matches_BM_NPPpeaks[main_peak==TRUE]))){
    stop('Duplicate Peaks still present after analysis')
  }


  ##############
  #Create benchmark, ungrouped and grouped tables for not found peaks
  ##############

  #Not found B Peaks
  Unmatched_BM_NPPpeaks <- b_table[!b_table$comp_id_b %in% unique(Matches_BM_NPPpeaks$comp_id_b)]

  #Not found UG Peaks
  nf_ug_table <- ug_table[!ug_table$comp_id_ug %in% unique(Matches_BM_NPPpeaks$comp_id_ug)]

  #Not found G Peaks
  nf_g_table <- g_table[!g_table$comp_id_g %in% unique(Matches_BM_NPPpeaks$comp_id_g)]



  ###################################################################################################################
  #feature_feature comparison
  ###################################################################################################################

  #Generate alignment error table
  if(nrow(g_table) > 0){


  AlignmentErrors_per_moleculeAndAdduct <-
    rbindlist(list(Matches_BM_NPPpeaks, Unmatched_BM_NPPpeaks), fill = TRUE)

  if('peak_area_rounded_ug' %in% colnames(AlignmentErrors_per_moleculeAndAdduct)){
    AlignmentErrors_per_moleculeAndAdduct <- AlignmentErrors_per_moleculeAndAdduct[, 'peak_area_ug' := peak_area_rounded_ug]
  }

  AlignmentErrors_per_moleculeAndAdduct <- AlignmentErrors_per_moleculeAndAdduct[, as.list(count_errors_max(.SD)), .SDcols=c('molecule_b',
                                                    'adduct_b',
                                                    'main_peak',
                                                    'sample_id_b',
                                                    'isoab_b',
                                                    'feature_id_g',
                                                    'peak_area_g',
                                                    'peak_area_ug'),
                 by=.(molecule_b, adduct_b)]

  AlignmentErrors_per_moleculeAndAdduct <- setnames(AlignmentErrors_per_moleculeAndAdduct, c('errors', 'Lost_b.A', 'diff_BM', 'molecule_b', 'adduct_b'), c('Min.errors', 'Lost_b.A', 'BM.div', 'Molecule', 'Adduct'))

  } else {

    AlignmentErrors_per_moleculeAndAdduct <- setNames(data.table(matrix(nrow = 0, ncol = 5)), c("Molecule", "Adduct", "Min.errors", "Lost_b.A", "BM.div"))

  }


  #Generate feature table

    if(nrow(g_table) > 0){

      ff_table_dt <- pick_main_feature(feature_compare(b_table, g_table))

  dt <- ff_table_dt[main_feature == TRUE]

  id.cols <- c("feature_id_b", "feature_id_g", "molecule_b", "isoab_b", "adduct_b",
               "total_area_b", "min_mz_start", "max_mz_end", "min_rt_start",
               "max_rt_end", "main_feature")

  dt_melt_b <- melt(dt,
                    id.vars = id.cols,
                    measure.vars = colnames(dt)[grepl(glob2rx("sample_*_b"), colnames(dt))],
                    value.name = "area_b",
                    variable.name = "sample_id_b",
                    variable.factor = FALSE)

  dt_melt_b$sample_id_b <-  as.factor(substr(dt_melt_b$sample_id_b, 8, nchar(dt_melt_b$sample_id_b) - 2))

  dt_melt_g <- melt(dt,
                    id.vars = id.cols,
                    measure.vars = colnames(dt)[grepl(glob2rx("sample_*_g"), colnames(dt))],
                    value.name = "area_g",
                    variable.name = "sample_id_b",
                    variable.factor = FALSE)

  dt_melt_g$sample_id_b <- as.factor(substr(dt_melt_g$sample_id_b, 8, nchar(dt_melt_g$sample_id_b) - 2))

  dt_n <- dt_melt_g[dt_melt_b, on = colnames(dt_melt_b)[-length(dt_melt_b)]]

  tmp <- unique(data.table(sample_id_b = as.factor(Matches_BM_NPPpeaks[["sample_id_b"]]),
                           sample_name_b = Matches_BM_NPPpeaks[["sample_name_b"]]))
  Matches_BM_NPPpeaks_NPPfeatures <- dt_n[tmp, on = .(sample_id_b)]

  ug_info <- rbindlist(list(Matches_BM_NPPpeaks, Unmatched_BM_NPPpeaks), fill = TRUE, use.names = TRUE)

  Matches_BM_NPPpeaks_NPPfeatures <-
  Matches_BM_NPPpeaks_NPPfeatures[!is.na(area_b)][ug_info[, c("molecule_b",
                                            "adduct_b",
                                            "isoab_b",
                                            "sample_name_b",
                                            "peak_area_b",
                                            "peak_area_ug")],
                                on = .(molecule_b, adduct_b, isoab_b, sample_name_b)]

    } else {

      Matches_BM_NPPpeaks_NPPfeatures <- setNames(data.table(matrix(nrow = 0, ncol = 15)), c("feature_id_b", "feature_id_g", "molecule_b", "isoab_b", "adduct_b",
                                                                          "total_area_b", "min_mz_start", "max_mz_end", "min_rt_start",
                                                                          "max_rt_end", "main_feature", "sample_id_b", "area_g", "area_b",
                                                                          "sample_name_b"))
      ff_table_dt <- data.table(NULL)
    }



  #Generate Random and systematic error DT
  MissingPeak_classification <- rbindlist(list(Matches_BM_NPPpeaks, Unmatched_BM_NPPpeaks), fill = TRUE)

  MissingPeak_classification <- MissingPeak_classification[, c("molecule_b", "adduct_b", "isoab_b", "sample_name_b", "peak_area_b", "peak_height_b",
                           "peak_area_ug", "peak_area_g", "feature_id_g", "sample_id_b"
  )]

  if(nrow(g_table) > 0){

 # feat_t <- melt_fftable(ff_table_dt, Matches_BM_NPPpeaks)
#tt <<- feat_t
  MissingPeak_classification <- Matches_BM_NPPpeaks_NPPfeatures[main_feature == TRUE & !is.na(area_b), c("molecule_b",
                                                                     "adduct_b",
                                                                     "isoab_b",
                                                                     "sample_name_b",
                                                                     "area_g")][MissingPeak_classification,
                       on =.(molecule_b, adduct_b, isoab_b, sample_name_b)]

#tt <<- MissingPeak_classification



  MissingPeak_classification[, peak_area_g := area_g]



  MissingPeak_classification <- MissingPeak_classification[!is.na(peak_area_b)]
  MissingPeak_classification <- MissingPeak_classification[order(feature_id_g)]

   MissingPeak_classification <-
    MissingPeak_classification[, Connected := File_con_test(
      sample_name_b,
      feature_id_g),
      by = .(molecule_b, adduct_b)]

   colnames(MissingPeak_classification) <- replace(colnames(MissingPeak_classification), colnames(MissingPeak_classification) == "area_g", "peak_area_g")

  } else {

    MissingPeak_classification[, Connected := TRUE]
  }

  MissingPeak_classification <-
    MissingPeak_classification[, c("missing_peaks_ug", "missing_peaks_g") := .(find_r_s_error(
      peak_area_b,
      peak_area_ug,
      peak_height_b,
      Connected),
      find_r_s_error(
        peak_area_b,
        peak_area_g,
        peak_height_b,
        Connected)
    ), by = .(molecule_b, adduct_b, isoab_b)]



  MissingPeak_classification <- MissingPeak_classification[!is.na(peak_area_b)]



  #MissingPeak_classification[, missing_peaks := find_r_s_error(
  #  peak_area_b,
  #  peak_area_ug,
  #  peak_height_b
  #), by = .(molecule_b, adduct_b, isoab_b)]


#Generate Isotopologe error dt
Matches_BM_NPPpeaks_t <- Matches_BM_NPPpeaks
if(nrow(g_table) > 0){

  Matches_BM_NPPpeaks_t$sample_id_b <- as.factor(Matches_BM_NPPpeaks_t$sample_id_b)
  IT_ratio_biases <- Matches_BM_NPPpeaks_t[main_peak == TRUE,c("molecule_b",
                                   "adduct_b",
                                   "isoab_b",
                                   "sample_id_b",
                                   "peak_area_ug",
                                   "peak_area_b",
                                   "Grp_b",
                                   "sample_name_b")][Matches_BM_NPPpeaks_NPPfeatures[main_feature == TRUE &
                                                            !is.na(area_b) &
                                                            !is.na(area_g),
                                                          c("molecule_b",
                                                            "adduct_b",
                                                            "isoab_b",
                                                            "sample_id_b",
                                                            "area_b",
                                                            "area_g",
                                                            "sample_name_b")],
                                                     on = .(molecule_b, adduct_b, isoab_b, sample_id_b)]

  IT_ratio_biases[is.na(peak_area_b)]$peak_area_b <- IT_ratio_biases[is.na(peak_area_b)]$area_b
  IT_ratio_biases[is.na(sample_name_b)]$sample_name_b <- IT_ratio_biases[is.na(sample_name_b)]$i.sample_name_b


} else {
  Matches_BM_NPPpeaks_t$sample_id_b <- as.factor(Matches_BM_NPPpeaks_t$sample_id_b)
  Matches_BM_NPPpeaks_t[, area_g := as.numeric(NA)]

  IT_ratio_biases <- Matches_BM_NPPpeaks_t


}

  IT_ratio_biases <- IT_ratio_biases[isoab_b != 100][IT_ratio_biases[isoab_b == 100,
                                     c("sample_id_b", "sample_name_b", "molecule_b", "adduct_b", "area_g", "peak_area_b", "peak_area_ug")],
                                on=.(sample_id_b, molecule_b, adduct_b),
                                nomatch = NA, allow.cartesian=TRUE][,c("benchmark",
                                                                       "NPP_peak picking",
                                                                       "NPP_features") := .((peak_area_b / ((i.peak_area_b * isoab_b) / 100) - 1) * 100,
                                                                                            (peak_area_ug / ((i.peak_area_ug * isoab_b) / 100) - 1) * 100,
                                                                                            (area_g / ((i.area_g * isoab_b) / 100) - 1) * 100)]


  IT_ratio_biases[, diffH20PP_pp := as.character(abs(abs(benchmark) - abs(`NPP_peak picking`)) > 10 &
                                        abs(`NPP_peak picking` - benchmark) > 20 &
                                        abs(`NPP_peak picking`) > 30)]

  IT_ratio_biases[, diffH20PP_ft := as.character(abs(abs(benchmark) - abs(NPP_features)) > 10 &
                                        abs(NPP_features - benchmark) > 20 &
                                        abs(NPP_features) > 30)]


  IT_ratio_biases[diffH20PP_pp == "TRUE"]$diffH20PP_pp <- "Inc. > 20%p"
  IT_ratio_biases[diffH20PP_pp == "FALSE"]$diffH20PP_pp <- "Inc. < 20%p"

  IT_ratio_biases[diffH20PP_ft == "TRUE"]$diffH20PP_ft <- "Inc. > 20%p"
  IT_ratio_biases[diffH20PP_ft == "FALSE"]$diffH20PP_ft <- "Inc. < 20%p"

  IT_ratio_biases <- IT_ratio_biases[!is.na(peak_area_b)]


  #create overview table per compound


  bm_tab <- rbindlist(list(Matches_BM_NPPpeaks[main_peak == TRUE], Unmatched_BM_NPPpeaks), fill = TRUE, use.names = TRUE)
  bm_tab[is.na(main_peak), main_peak := FALSE]


  peaks_pp <- bm_tab[,.(Found_peaks_pp = sum(main_peak),
                        Not_Found_peaks_pp = length(main_peak) - sum(main_peak)), by = .(molecule_b)]

  peaks_ft <- Matches_BM_NPPpeaks_NPPfeatures[!is.na(peak_area_b) & !is.na(main_feature) && main_feature == TRUE, .(Found_peaks_ft = sum(!is.na(area_g)),
                                                                                                                  Not_Found_peaks_ft = sum(is.na(area_g))),
                                            by = .(molecule_b)]

  IRb <- IT_ratio_biases[, c("molecule_b", "diffH20PP_pp", "diffH20PP_ft")][, .(IRb_ok_pp = sum(diffH20PP_pp == "Inc. < 20%p", na.rm = TRUE),
                                                                                           IRb_off_pp = sum(diffH20PP_pp == "Inc. > 20%p", na.rm = TRUE),
                                                                                           IRb_ok_ft = sum(diffH20PP_ft == "Inc. < 20%p", na.rm = TRUE),
                                                                                           IRb_off_ft = sum(diffH20PP_ft == "Inc. > 20%p", na.rm = TRUE)),
                                                                                       by = .(molecule_b)]



  split_pp <- SplittedMatches_BM_NPPpeaks[,c("molecule_b")][,.( Split_peaks = .N), by = .(molecule_b)]

  mw_tab <- MissingPeak_classification[, .(R_pp = sum(missing_peaks_ug == "R", na.rm = TRUE),
                                         S_pp = sum(missing_peaks_ug == "S", na.rm = TRUE),
                                         R_ft = sum(missing_peaks_g == "R", na.rm = TRUE),
                                         S_ft = sum(missing_peaks_g == "S", na.rm = TRUE)),
                                     by = .(molecule_b)]


  ali_tab <- AlignmentErrors_per_moleculeAndAdduct[, .(Min.er = sum(Min.errors, na.rm = TRUE),
                                                 BM.div = sum(BM.div, na.rm = TRUE),
                                                 lost = sum(Lost_b.A, na.rm = TRUE)),
                                             by = .(Molecule)]

  colnames(ali_tab)[1] <- "molecule_b"



  sum_tab <- unique(bm_tab[!is.na(molecule_b), "molecule_b"])
  sum_tab <- peaks_pp[sum_tab, on = .(molecule_b)]
  sum_tab <- peaks_ft[sum_tab, on = .(molecule_b)]
  sum_tab <- split_pp[sum_tab, on = .(molecule_b)]
  sum_tab <- IRb[sum_tab, on = .(molecule_b)]
  sum_tab <- mw_tab[sum_tab, on = .(molecule_b)]
  sum_tab <- ali_tab[sum_tab, on = .(molecule_b)]



  setnafill(sum_tab, fill=0, cols = colnames(sum_tab)[-1])





  #############################





  ##############
  #Return the found and 3 notfoundtables in a list
  ##############

  return_list <- list('BM_NPPoutput_size' = BM_NPPoutput_size,
                      'Overview_per_molecule' = sum_tab,
                      'Matches_BM_NPPpeaks' = Matches_BM_NPPpeaks,
                      'Unmatched_BM_NPPpeaks' = Unmatched_BM_NPPpeaks,
                      'SplittedMatches_BM_NPPpeaks' = SplittedMatches_BM_NPPpeaks,
                      'MissingPeak_classification'= MissingPeak_classification,
                      'IT_ratio_biases' = IT_ratio_biases,
                      'AlignmentErrors_per_moleculeAndAdduct' = AlignmentErrors_per_moleculeAndAdduct,
                      'Matches_BM_NPPpeaks_NPPfeatures' = Matches_BM_NPPpeaks_NPPfeatures)

  message('Successful comparison!')


  return(return_list)
}
