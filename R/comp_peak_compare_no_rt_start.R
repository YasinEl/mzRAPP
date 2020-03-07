#' #' compare_peaks_ug_g_no_rt_start
#' #'
#' #' @param b_table
#' #' @param ug_table
#' #' @param g_table
#' #' @param algo
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' compare_peaks_ug_g_no_rt_start <- function(b_table, ug_table, g_table, algo){
#'
#'   info_list <- list()
#'
#'   ##############
#'   #Make sure input data is in correct formats
#'   ##############
#'
#'   if (!is.data.table(b_table) | !is.data.table(ug_table) | !is.data.table(g_table)){
#'     stop('Benchmark, ungrouped and/or grouped dataset are not type DataTable')
#'   }
#'
#'   ##############
#'   #Check if all necessary columns are present
#'   ##############
#'
#'   #Benchmark table Check
#'   b_req_cols <- c('comp_id_b', 'sample_id_b', 'rt_start_b', 'rt_end_b', 'rt_b', 'mz_start_b',
#'                   'mz_end_b', 'molecule_b', 'adduct_b', 'sample_name_b', 'peak_area_b', 'peak_height_b')
#'
#'
#'   if(!all(b_req_cols %in% colnames(b_table))){
#'     cols_not_found <- setdiff(b_req_cols, colnames(b_table))
#'     stop('Columns not present in benchmark dataset: ', paste0(cols_not_found, sep = " - "))
#'   }
#'
#'   #UG table check
#'   ug_req_cols <- c('comp_id_ug', 'sample_id_ug', 'rt_ug','mz_ug', 'sample_name_ug', 'peak_area_ug', 'peak_height_ug')
#'
#'
#'   if(!all(ug_req_cols %in% colnames(ug_table))){
#'     cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
#'     stop('Columns not present in ungrouped dataset: ', paste0(cols_not_found, sep = " - "))
#'   }
#'
#'   #G table check
#'   g_req_cols <- c('comp_id_g', 'sample_id_g', 'rt_g', 'mz_g', 'sample_name_g', 'peak_area_g', 'feature_id_g')
#'
#'
#'   if(!all(g_req_cols %in% colnames(g_table))){
#'     cols_not_found <- setdiff(g_req_cols, colnames(g_table))
#'     stop('Columns not present in grouped dataset: ', paste0(cols_not_found, sep = " - "))
#'   }
#'
#'   ##############
#'   #Write relevant information to info list
#'   ##############
#'   info_list <- append(info_list, list(nr_of_b_peaks = length(unique(b_table$comp_id_b)),
#'                                       nr_of_b_features = length(unique(b_table$feature_id_b)),
#'                                       nr_of_ug_peaks = length(unique(ug_table$comp_id_ug)),
#'                                       nr_of_g_peaks = length(unique(g_table$comp_id_g)),
#'                                       nr_of_g_features = length(unique(g_table$feature_id_g)),
#'                                       algorithm = algo))
#'
#'   print(info_list)
#'
#'   ##############
#'   #Check for duplicate peaks
#'   ##############
#'
#'   if (any(duplicated(b_table, by = c('rt_b', 'mz_b', 'peak_height_b')))){
#'     warning('Duplicate peaks present in benchmark. This can lead to further errors during analysis.')
#'   }
#'   if (any(duplicated(ug_table, by = c('rt_ug', 'mz_ug', 'peak_height_ug')))){
#'     warning('Duplicate peaks present in ungrouped dataset. This can lead to further errors during analysis.')
#'   }
#'   if (any(duplicated(g_table, by = c('rt_g', 'mz_g', 'peak_area_g')))){
#'     warning('Duplicate peaks present in grouped. This can lead to further errors during analysis.')
#'   }
#'
#'   ##############
#'   #Start comparison!
#'   ##############
#'
#'   ##############
#'   #Generating minimum peak bounderies in benchmark
#'   #Untrageted rt range must completely envelope these bounderies
#'   #Defined as taking the shorter of rt_start_b to rt_b or rt_end_b to rt_b,
#'   #taking 30% of this distance, adding and subtracting it from rt_b
#'   ##############
#'   b_table[, rt_add_temp := ifelse((rt_end_b - rt_b) < (rt_b - rt_start_b),
#'                                   rt_end_b - rt_b,  rt_b - rt_start_b)]
#'   b_table[, ':=' (new_rt_start_b = rt_b - rt_add_temp*0.5,new_rt_end_b = rt_b + rt_add_temp*0.5)]
#'
#'   #Creating temp columns to prevent over-writing by join
#'   ug_table[, ':=' (sample_id_ug_temp = sample_id_ug,
#'                    rt_ug_temp = rt_ug,
#'                    mz_ug_temp = mz_ug)]
#'
#'   b_table[, ':=' (sample_id_b_temp = sample_id_b,
#'                   new_rt_start_b_temp = new_rt_start_b,
#'                   new_rt_end_b_temp = new_rt_end_b,
#'                   mz_start_b_temp = mz_start_b,
#'                   mz_end_b_temp = mz_end_b)]
#'
#'   ##############
#'   #Conducting non-equi join.
#'   #rt of ut peak must fall within new calc range of benchmark
#'   ##############
#'   c_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
#'                                     new_rt_start_b_temp <= rt_ug_temp,
#'                                     new_rt_end_b_temp >= rt_ug_temp,
#'                                     mz_start_b_temp <= mz_ug_temp,
#'                                     mz_end_b_temp >= mz_ug_temp),
#'                      allow.cartesian=TRUE, nomatch=NULL, mult='all']
#'
#'   ##############
#'   #Joining peaks from groupd file onto found areas of ungrouped
#'   #Could be a problem with rounded areas
#'   ##############
#'
#'
#'   #Creating temp columns to prevent over-writing by join
#'   c_table[, peak_area_ug_temp := peak_area_ug]
#'   g_table[, peak_area_g_temp := peak_area_g]
#'
#'   #Join
#'   c_table <- g_table[c_table, on=.(peak_area_g_temp == peak_area_ug_temp),
#'                      allow.cartesian = TRUE, nomatch=NA, mult='all']
#'
#'   #Remove _temp Columns
#'   c_table[,grep('_temp$', colnames(c_table)):=NULL]
#'   b_table[,grep('_temp$', colnames(b_table)):=NULL]
#'
#'
#'   ####FIND BETTER WAY TO GET RID OF DUPLICATES
#'
#'   #Get height diff to benchmark and pick peak with smallest diff
#'   print(paste('c_table: ', nrow(c_table)))
#'   c_table <- c_table[, height_diff := abs(peak_height_ug - peak_height_b)]
#'   c_table <- c_table[, smaller_height := ifelse(height_diff == min(height_diff), 'TRUE', 'FALSE'), by=c('comp_id_b', 'comp_id_ug')]
#'   c_table <- c_table[smaller_height == 'TRUE']
#'   print(paste('Eliminating dups based on height: ', nrow(c_table)))
#'   ##If dups still present take g peak with rt  and mz closest to b
#'   c_table <- c_table[, rt_diff := abs(rt_g - rt_b)]
#'   c_table <- c_table[, mz_diff := abs(mz_g - mz_b)]
#'   c_table <- c_table[, smaller_diff := ifelse(rt_diff == min(rt_diff) & mz_diff == min(mz_diff), 'TRUE', 'FALSE'), by=c('comp_id_b', 'comp_id_ug')]
#'   c_table <- c_table[smaller_diff == 'TRUE']
#'   print(paste('Eliminating dups based on rt: ', nrow(c_table)))
#'
#'   #######REWROK WHEN FUCTION IS REWORKED
#'
#'
#'   c_table[, main_peak := eliminate_duplicates_no_for(comp_id_b, comp_id_ug, isoabb_b, peak_area_ug, peak_height_ug, peak_height_b), by=.(molecule_b, adduct_b, sample_id_b)]
#'   print('here')
#'   c_table[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]
#'
#'   fwrite(c_table, 'dup_debug.csv')
#'
#'
#'
#'
#'   #Make sure main peaks only occure once
#'   if (any(duplicated(c_table[main_peak==TRUE]))){
#'     stop('Duplicate Peaks still present after analysis')
#'   }
#'
#'   ##############
#'   #Create benchmark, ungrouped and grouped tables for not found peaks
#'   ##############
#'
#'   #Not found B Peaks
#'   nf_b_table <- b_table[!b_table$comp_id_b %in% unique(c_table$comp_id_b)]
#'
#'   #Not found UG Peaks
#'   nf_ug_table <- ug_table[!ug_table$comp_id_ug %in% unique(c_table$comp_id_ug)]
#'
#'   #Not found G Peaks
#'   nf_g_table <- g_table[!g_table$comp_id_g %in% unique(c_table$comp_id_g)]
#'
#'   ##############
#'   #Return the found and 3 notfoundtables in a list
#'   ##############
#'   return(list('c_table' = c_table, 'nf_b_table' = nf_b_table, 'nf_ug_table' = nf_ug_table, 'nf_g_table' = nf_g_table, 'info_list' = info_list, 'split_table' = NULL))
#' }
