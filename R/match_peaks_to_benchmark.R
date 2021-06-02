#' match_peaks_to_benchmark
#'
#' @param b_table b_table
#' @param ug_table ug_table
#'
#' @keywords internal

match_peaks_to_benchmark <- function(b_table,
                                     ug_table){

  ##############
  #Generating minimum peak boundaries in benchmark
  #Untargeted rt range must completely envelope these boundaries
  #Defined as taking the shorter of rt_start_b to rt_b or rt_end_b to rt_b,
  #taking 50% of this distance, adding and subtracting it from rt_b
  ##############

  #Creating temp columns to prevent over-writing by join
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


  ##############
  #Conducting non-equi join.
  #rt range must be larger on both sides than calculated peak limits,
  #mz must fall within mz start and end of benchmark
  ##############
  Matches_BM_NPPpeaks <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
                                                peak_core_rt_range_start_b_temp >= rt_start_ug_temp,
                                                peak_core_rt_range_end_b_temp <= rt_end_ug_temp,
                                                mz_start_b_temp <= mz_ug_temp,
                                                mz_end_b_temp >= mz_ug_temp,
                                                rt_start_b_temp < rt_ug_temp,
                                                rt_end_b_temp > rt_ug_temp),
                                 allow.cartesian=TRUE, nomatch=NULL, mult='all']


  Matches_BM_NPPpeaks <- pick_main_peak(Matches_BM_NPPpeaks)

  #Matches_BM_NPPpeaks <- Matches_BM_NPPpeaks[main_peak == TRUE]
  suppressWarnings(b_table[,grep('_temp$', colnames(b_table)):=NULL])
  suppressWarnings(ug_table[,grep('_temp$', colnames(ug_table)):=NULL])
  suppressWarnings(Matches_BM_NPPpeaks[,grep('_temp$', colnames(Matches_BM_NPPpeaks)):=NULL])

  return(Matches_BM_NPPpeaks)

}
