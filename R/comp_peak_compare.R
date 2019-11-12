

#' compare_peaks_ug_g
#'
#' @param b_table
#' @param ug_table
#' @param g_table
#' @param algo
#'
#' @return
#' @export
#'
#' @examples
compare_peaks_ug_g <- function(b_table, ug_table, g_table, algo){

  info_list <- list()


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
  ug_req_cols <- c('comp_id_ug', 'sample_id_ug', 'rt_start_ug', 'rt_end_ug', 'rt_ug', 'mz_ug', 'sample_name_ug', 'peak_area_ug', 'peak_height_ug')


  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns not present in benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #G table check
  g_req_cols <- c('comp_id_g', 'sample_id_g', 'rt_start_g', 'rt_end_g', 'rt_g', 'mz_g',
                  'sample_name_g', 'peak_area_g', 'feature_id_g')


  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns not present in benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  ##############
  #Write relevant information to info list
  ##############
  info_list <- append(info_list, list(nr_of_b_peaks = length(unique(b_table$comp_id_b)),
                                      nr_of_b_features = length(unique(b_table$feature_id_b)),
                                      nr_of_ug_peaks = length(unique(ug_table$comp_id_ug)),
                                      nr_of_g_peaks = length(unique(g_table$comp_id_g)),
                                      nr_of_g_features = length(unique(g_table$feature_id_g)),
                                      algorithm = algo))

  ##############
  #Check for duplicate peaks
  ##############

  if (any(duplicated(b_table, by = c('rt_b', 'mz_b', 'peak_height_b')))){
    warning('Duplicate peaks present in benchmark. This can lead to further errors during analysis.')
  }
  if (any(duplicated(ug_table, by = c('rt_ug', 'mz_ug', 'peak_height_ug')))){
    warning('Duplicate peaks present in ungrouped dataset. This can lead to further errors during analysis.')
  }
  if (any(duplicated(g_table, by = c('rt_g', 'mz_g', 'peak_area_g')))){
    warning('Duplicate peaks present in grouped. This can lead to further errors during analysis.')
  }

  ##############
  #Start comparison!
  ##############

  ##############
  #Generating minimum peak bounderies in benchmark
  #Untrageted rt range must completely envelope these bounderies
  #Defined as taking the shorter of rt_start_b to rt_b or rt_end_b to rt_b,
  #taking 30% of this distance, adding and subtracting it from rt_b
  ##############
  b_table[, rt_add_temp := ifelse((rt_end_b - rt_b) < (rt_b - rt_start_b),
                                  rt_end_b - rt_b,  rt_b - rt_start_b)]
  b_table[, ':=' (new_rt_start_b = rt_b - rt_add_temp*0.3,new_rt_end_b = rt_b + rt_add_temp*0.3)]

  #Creating temp columns to prevent over-writing by join
  ug_table[, ':=' (sample_id_ug_temp = sample_id_ug,
                   rt_start_ug_temp = rt_start_ug,
                   rt_end_ug_temp = rt_end_ug,
                   mz_ug_temp = mz_ug)]

  b_table[, ':=' (sample_id_b_temp = sample_id_b,
                  new_rt_start_b_temp = new_rt_start_b,
                  new_rt_end_b_temp = new_rt_end_b,
                  mz_start_b_temp = mz_start_b,
                  mz_end_b_temp = mz_end_b)]

  ##############
  #Conducting non-equi join.
  #rt range must be larger on both sides than calculated peak limits,
  #mz must fall within mz start and end of benchmark
  ##############
  c_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                    new_rt_start_b_temp >= rt_start_ug_temp,
                                    new_rt_end_b_temp <= rt_end_ug_temp,
                                    mz_start_b_temp <= mz_ug_temp,
                                    mz_end_b_temp >= mz_ug_temp),
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

  #Find Peaks inside of benchmark bounderies
  split_middle_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                              new_rt_start_b_temp <= rt_start_ug_temp,
                                              new_rt_end_b_temp >= rt_end_ug_temp,
                                              mz_start_b_temp <= mz_ug_temp,
                                              mz_end_b_temp >= mz_ug_temp),
                               allow.cartesian=TRUE, nomatch=NULL, mult='all']

  #Combine the split peak tables
  split_table <- rbindlist(list('split_left_table' = split_left_table, 'split_right_table' = split_right_table, 'split_middle_table' = split_middle_table), fill=TRUE, use.names = TRUE, idcol='file')

  ##############
  #Joining peaks from groupd file onto found areas of ungrouped
  #Could be a problem with rounded areas
  ##############


  #Creating temp columns to prevent over-writing by join
  c_table[, peak_area_ug_temp := peak_area_ug]
  g_table[, peak_area_g_temp := peak_area_g]
  split_table[, peak_area_ug_temp := peak_area_ug]

  #Join
  c_table <- g_table[c_table, on=.(peak_area_g_temp == peak_area_ug_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']

  split_table <- g_table[split_table, on=.(peak_area_g_temp == peak_area_ug_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']

  #Remove _temp Columns
  c_table[,grep('_temp$', colnames(c_table)):=NULL]
  b_table[,grep('_temp$', colnames(b_table)):=NULL]

  fwrite(split_left_table, "split_left_debug.csv")
  fwrite(split_right_table, "split_right_debug.csv")
  fwrite(split_middle_table, "split_middle_debug.csv")
  fwrite(split_table, "split_debug.csv")






  ##############
  #Create main_peak column to identify main peaks
  #when several peaks have been asigned to on benchmark peak
  ##############




  #print(nrow(c_table[duplicated(c_table, by = c('comp_id_b', 'comp_id_ug')) | duplicated(c_table, by = c('comp_id_b', 'comp_id_ug'), fromLast = TRUE)]))
  #fwrite(c_table[duplicated(c_table, by = c('comp_id_b', 'comp_id_ug'))| duplicated(c_table, by = c('comp_id_b', 'comp_id_ug'), fromLast = TRUE)], 'finddups.csv')
  #print(nrow(c_table[duplicated(c_table, by=c('feature_id_g', 'sample_name_g', ))]))
  #fwrite(c_table[duplicated(c_table, by=c('feature_id_g', 'sample_name_g', 'molecule_b', 'adduct_b')) | duplicated(c_table, by=c('feature_id_g', 'sample_name_g', 'molecule_b', 'adduct_b'), fromLast = TRUE)], 'duptest.csv')
  #######
  #Eleminiate Double joined peaks (same peak was grouped several times)
  #REWORK FOR LOOP LATER
  #Not sure if good comparison since feature_id can be duplicated for different ug peaks
  ########################################FIX LATER, FOR NOE REMOVE DUPLICATES############################
  #######
  #duplicate_table <- c_table[duplicated(c_table, by = c('comp_id_b', 'comp_id_ug')) | duplicated(c_table, by = c('comp_id_b', 'comp_id_ug'), fromLast = TRUE)]
  #for (i in 1:nrow(duplicate_table)){
  #  dup_feature_id <- duplicate_table[i, feature_id_g]
  #  dup_molecule <- duplicate_table[i, molecule_b]
  #  dup_adduct <- duplicate_table[i, adduct_b]
  #  #Count occurance of feature
  #  print(paste(dup_adduct, dup_molecule, dup_feature_id))
  #  count <- nrow(c_table[molecule_b == dup_molecule & adduct_b == dup_adduct & feature_id_g == dup_feature_id])
  #  duplicate_table[feature_id_g == dup_feature_id, feature_count := count]
  #}


  #duplicate_table <- duplicate_table[, max_feature_count := max(feature_count), by = c('comp_id_b', 'comp_id_ug')]
  #duplicate_table <- duplicate_table[max_feature_count == max_feature_count]

  #fwrite(duplicate_table, 'dupCount.csv')


  ####FIND BETTER WAY TO GET RID OF DUPLICATES

  #Get height diff to benchmark and pick peak with smallest diff
  print(paste('c_table: ', nrow(c_table)))
  c_table <- c_table[, height_diff := abs(peak_height_ug - peak_height_b)]
  c_table <- c_table[, smaller_height := ifelse(height_diff == min(height_diff), 'TRUE', 'FALSE'), by=c('comp_id_b', 'comp_id_ug')]
  c_table <- c_table[smaller_height == 'TRUE']
  print(paste('Eliminating dups based on height: ', nrow(c_table)))
  #If dups still present take g peak with smallest rt_diff
  c_table <- c_table[, rt_diff := rt_end_g - rt_start_g]
  c_table <- c_table[, smaller_rt_diff := ifelse(rt_diff == min(rt_diff) | is.na(rt_end_g) | is.na(rt_start_g), 'TRUE', 'FALSE'), by=c('comp_id_b', 'comp_id_ug')]
  c_table <- c_table[smaller_rt_diff == 'TRUE']
  print(paste('Eliminating dups based on rt: ', nrow(c_table)))

  #######REWROK WHEN FUCTION IS REWORKED
  c_table[, main_peak := eliminate_duplicates_no_for(comp_id_b, comp_id_ug, isoabb_b, peak_area_ug, peak_height_ug, peak_height_b), by=.(molecule_b, adduct_b, sample_id_b)]

  c_table[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]
  split_table[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]

  split_table[, present_in_found := ifelse(id_b_ug %in% c_table$id_b_ug, 'TRUE', 'FALSE')]


  #Make sure main peaks only occure once
  if (any(duplicated(c_table[main_peak==TRUE]))){
    stop('Duplicate Peaks still present after analysis')
  }

  ##############
  #Create benchmark, ungrouped and grouped tables for not found peaks
  ##############

  #Not found B Peaks
  nf_b_table <- b_table[!b_table$comp_id_b %in% unique(c_table$comp_id_b)]

  #Not found UG Peaks
  nf_ug_table <- ug_table[!ug_table$comp_id_ug %in% unique(c_table$comp_id_ug)]

  #Not found G Peaks
  nf_g_table <- g_table[!g_table$comp_id_g %in% unique(c_table$comp_id_g)]

  ##############
  #Return the found and 3 notfoundtables in a list
  ##############
  print('Compare Succesfull')
  return(list('c_table' = c_table, 'nf_b_table' = nf_b_table, 'nf_ug_table' = nf_ug_table, 'nf_g_table' = nf_g_table, 'info_list' = info_list, 'split_table' = split_table))
}
