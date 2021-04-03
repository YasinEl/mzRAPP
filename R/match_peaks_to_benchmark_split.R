#' match_peaks_to_benchmark_split
#'
#' @param b_table b_table
#' @param ug_table ug_table
#'
#' @keywords internal

match_peaks_to_benchmark_split <- function(b_table,
                                           ug_table){

  ug_table[, ':=' (sample_id_ug_temp = sample_id_ug,
                   rt_start_ug_temp = rt_start_ug,
                   rt_end_ug_temp = rt_end_ug,
                   rt_ug_temp = rt_ug,
                   mz_ug_temp = mz_ug)]


  b_table[, ':=' (sample_id_b_temp = sample_id_b,
                  rt_start_b_temp = rt_start_b,
                  rt_end_b_temp = rt_end_b,
                  peak_core_rt_range_start_b_temp = peak_core_rt_range_start_b,
                  peak_core_rt_range_end_b_temp = peak_core_rt_range_end_b,
                  mz_start_b_temp = mz_start_b - 0.0002,
                  mz_end_b_temp = mz_end_b + 0.0002)]

  #Find Peaks to the left of benchmark boundaries
  split_left_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                             peak_core_rt_range_start_b_temp >= rt_start_ug_temp,
                                             peak_core_rt_range_start_b_temp <= rt_end_ug_temp,
                                             peak_core_rt_range_end_b_temp >= rt_end_ug_temp,
                                             mz_start_b_temp <= mz_ug_temp,
                                             mz_end_b_temp >= mz_ug_temp),
                              allow.cartesian=TRUE, nomatch=NULL, mult='all']


  #Find Peaks to the right of benchmark boundaries
  split_right_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                              peak_core_rt_range_start_b_temp <= rt_start_ug_temp,
                                              peak_core_rt_range_end_b_temp >= rt_start_ug_temp,
                                              peak_core_rt_range_end_b_temp <= rt_end_ug_temp,
                                              mz_start_b_temp <= mz_ug_temp,
                                              mz_end_b_temp >= mz_ug_temp),
                               allow.cartesian=TRUE, nomatch=NULL, mult='all']

  #Find Peaks inside of benchmark boundaries
  split_middle_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                               peak_core_rt_range_start_b_temp <= rt_start_ug_temp,
                                               peak_core_rt_range_end_b_temp >= rt_end_ug_temp,
                                               mz_start_b_temp <= mz_ug_temp,
                                               mz_end_b_temp >= mz_ug_temp),
                                allow.cartesian=TRUE, nomatch=NULL, mult='all']

  #Combine the split peak tables
  SplittedMatches_BM_NPPpeaks <- data.table::rbindlist(list('split_left_table' = split_left_table, 'split_right_table' = split_right_table, 'split_middle_table' = split_middle_table), fill=TRUE, use.names = TRUE, idcol='file')

  suppressWarnings(b_table[,grep('_temp$', colnames(b_table)):=NULL])
  suppressWarnings(ug_table[,grep('_temp$', colnames(ug_table)):=NULL])
  suppressWarnings(SplittedMatches_BM_NPPpeaks[,grep('_temp$', colnames(SplittedMatches_BM_NPPpeaks)):=NULL])

  return(SplittedMatches_BM_NPPpeaks)

}
