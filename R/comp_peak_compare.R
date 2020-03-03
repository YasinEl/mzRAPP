

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
compare_peaks_ug_g <- function(b_table, ug_table, g_table, algo, main_feature_method){

  info_list <- list()
  tic()

  print('b table')
  str(b_table)
  print('ug table')
  str(ug_table)
  print('g table')
  str(g_table)

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
  info_list <- append(info_list, list(nr_of_b_peaks = length(unique(b_table$comp_id_b)),
                                      nr_of_b_features = length(unique(b_table$feature_id_b)),
                                      nr_of_ug_peaks = length(unique(ug_table$comp_id_ug)),
                                      nr_of_g_peaks = length(unique(g_table$comp_id_g)),
                                      nr_of_g_features = length(unique(g_table$feature_id_g)),
                                      algorithm = algo))

  print(info_list)


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
  #taking 50% of this distance, adding and subtracting it from rt_b
  ##############
  b_table[, rt_add_temp := ifelse((rt_end_b - rt_b) < (rt_b - rt_start_b),
                                  rt_end_b - rt_b,  rt_b - rt_start_b)]
  b_table[, ':=' (new_rt_start_b = rt_b - rt_add_temp*0.5,new_rt_end_b = rt_b + rt_add_temp*0.5)]


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


  fwrite(c_table, 'firstJoinDebug.csv')





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



  print(paste('Before Main Peak Check: ', nrow(c_table)))
  c_table <- pick_main_peak(c_table)
  #c_table[, main_peak := choose_main_peak(comp_id_b, comp_id_ug, isoabb_b, peak_area_ug, peak_height_ug, peak_height_b, rt_start_b, rt_end_b, rt_start_ug, rt_end_ug), by=.(molecule_b, adduct_b, sample_id_b)]
  print(paste('After Main Peak Check: ', nrow(c_table[main_peak == TRUE])))




  ##############
  #Joining peaks from groupd file onto found areas of ungrouped
  #Could be a problem with rounded areas
  ##############


  #Creating temp columns to prevent over-writing by join
  #If statement is solution for msdial

  if ('peak_area_rounded_ug' %in% colnames(c_table)){
    c_table[, peak_area_ug_temp := peak_area_rounded_ug]
    c_table[, sample_id_b_temp := sample_id_b]
    split_table[, peak_area_ug_temp := peak_area_rounded_ug]
    g_table[, peak_area_g_temp := peak_area_g]
    g_table[, sample_id_g_temp := sample_id_g]
  } else {
    c_table[, peak_area_ug_temp := peak_area_ug]
    c_table[, sample_id_b_temp := sample_id_b]
    split_table[, peak_area_ug_temp := peak_area_ug]
    g_table[, peak_area_g_temp := peak_area_g]
    g_table[, sample_id_g_temp := sample_id_g]
  }


  #Join
  c_table <- g_table[c_table, on=.(peak_area_g_temp == peak_area_ug_temp, sample_id_g_temp == sample_id_b_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']


  #Replace 0 in peak_area_g with NA (no idea why they appear in the first place)(maybe int64?)
  #c_table <- c_table[, peak_area_g := ifelse(peak_area_g == 0, NA, peak_area_g)]

  split_table <- g_table[split_table, on=.(peak_area_g_temp == peak_area_ug_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']


  #Remove _temp Columns
  c_table[,grep('_temp$', colnames(c_table)):=NULL]
  b_table[,grep('_temp$', colnames(b_table)):=NULL]

  fwrite(g_table, 'msdail_full_g_debug.csv')
  fwrite(c_table, 'grouping_check.csv')


  c_table[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]
  split_table[, id_b_ug := paste(comp_id_b, comp_id_ug, sep='_')]

  split_table[, present_in_found := ifelse(id_b_ug %in% c_table$id_b_ug, 'TRUE', 'FALSE')]


  #Make sure main peaks only occure once
  if (any(duplicated(c_table[main_peak==TRUE]))){
    stop('Duplicate Peaks still present after analysis')
  }

  ##############
  #Compare Feature with Feature
  ##############

  #Generate Feature Table from Benchmarl
  #bf_table <- b_table[, .(mean_area_b = mean(peak_area_b), rt_start_b = min(rt_start_b), rt_end_b = max(rt_end_b),
  #                        mz_start_b = min(mz_start_b), mz_end_b = max(mz_end_b), isoabb_b = unique(isoabb_b), molecule_b = unique(molecule_b), adduct_b = unique(adduct_b)), by=.(feature_id_b)]
  #gf_table <- g_table[, .(mean_area_g = mean(peak_area_g), rt_g = mean(rt_g), mz_g = min(mz_g)), by=.(feature_id_g)]

  ###Create Temp cols for merge

  #bf_table <- bf_table[, ':=' (rt_start_b_temp = rt_start_b, rt_end_b_temp = rt_end_b, mz_start_b_temp = mz_start_b, mz_end_b_temp = mz_end_b)]
  #gf_table <- gf_table[, ':=' (rt_g_temp = rt_g, mz_g_temp = mz_g)]


  #print(bf_table)

  #cf_table <- bf_table[gf_table, on=.(rt_start_b_temp < rt_g_temp,
  #                                    rt_end_b_temp >  rt_g_temp,
  #                                    mz_start_b_temp < mz_g_temp  ,
  #                                    mz_end_b_temp > mz_g_temp), allow.cartesian=TRUE, nomatch=NULL, mult='all']

  #cf_table <- cf_table[,grep('_temp$', colnames(c_table)):=NULL]


    #c_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
    #                                  new_rt_start_b_temp >= rt_start_ug_temp,
    #                                  new_rt_end_b_temp <= rt_end_ug_temp,
    #                                  mz_start_b_temp <= mz_ug_temp,
    #                                  mz_end_b_temp >= mz_ug_temp),
    #                   allow.cartesian=TRUE, nomatch=NULL, mult='all']


  #fwrite(bf_table, 'bfdebug.csv')
  #fwrite(gf_table, 'gfdebug.csv')
  #fwrite(cf_table, 'cfdebug.csv')

  #print(cf_table, class=TRUE)

  ###################################################################################################################
  #feature_feature comparison
  ###################################################################################################################
  print('Start FF Compare')
  ff_table_dt <- pick_main_feature(feature_compare(b_table, g_table))
  fwrite(ff_table_dt, 'ff_comp_dt.csv')

  ##############
  #Create benchmark, ungrouped and grouped tables for not found peaks
  ##############

  #Not found B Peaks
  nf_b_table <- b_table[!b_table$comp_id_b %in% unique(c_table$comp_id_b)]

  #Not found UG Peaks
  nf_ug_table <- ug_table[!ug_table$comp_id_ug %in% unique(c_table$comp_id_ug)]

  #Not found G Peaks
  nf_g_table <- g_table[!g_table$comp_id_g %in% unique(c_table$comp_id_g)]

  fwrite(c_table, 'msdial_dbug.csv')

  print(paste0('Before Main Feature: ', nrow(c_table)))
  if (main_feature_method != '---'){
    c_table <- find_main_feature_1(c_table, main_feature_method, nf_g_table, nf_b_table)
    c_table <- c_table[is_main_feature == TRUE]
  }
  print(paste0('After Main Feature: ', nrow(c_table)))
  print(paste0('Only Main Peak: ', nrow(c_table[main_peak == TRUE])))

  #Generate Random and systematic error DT
  rs_table <- rbindlist(list(c_table, nf_b_table), fill = TRUE)

  rs_table[, missing_peaks := find_r_s_error(
    #comp_id_b,
    #molecule_b,
    #adduct_b,
    #sample_id_b,
    #isoabb_b,
    peak_area_b,
    peak_area_ug,
    peak_height_b
  ), by = .(molecule_b, adduct_b, isoabb_b)]


  #Generate Isotopologe error dt
  iso_err_dt <- na.omit(c_table, cols = c('peak_area_ug'))

  iso_err_dt <- na.omit(iso_err_dt, cols = c("peak_area_b", "peak_area_ug"))

  iso_err_dt <- iso_err_dt[main_peak == TRUE]



  DT_tmp <- iso_err_dt[isoabb_b != 100][iso_err_dt[isoabb_b == 100],
                                        on=.(sample_name_b, molecule_b, adduct_b),
                                        nomatch = 0L, allow.cartesian=TRUE][,c("benchmark", "non_targeted") := .((peak_area_b / ((i.peak_area_b * isoabb_b) / 100) - 1) * 100,
                                                                                                                 (peak_area_ug / ((i.peak_area_ug * isoabb_b) / 100) - 1) * 100)]


  iso_err_dt <- merge(iso_err_dt, DT_tmp[,.(comp_id_b, benchmark, non_targeted)], by = 'comp_id_b', all.x = TRUE, allow.cartesian = TRUE)


  iso_err_dt[, diffH20PP := as.character(abs(abs(benchmark) - abs(non_targeted)) > 10 &
                                           abs(non_targeted - benchmark) > 20 &
                                           abs(non_targeted) > 30)]

  iso_err_dt[diffH20PP == "TRUE"]$diffH20PP <- "Inc. > 20%p"
  iso_err_dt[diffH20PP == "FALSE"]$diffH20PP <- "Inc. < 20%p"

  iso_err_dt <-
    melt(
      iso_err_dt,
      id.vars = c('molecule_b', 'adduct_b', 'Grp_b', 'isoabb_b', 'sample_name_b', 'diffH20PP'),
      measure.vars = c("benchmark", "non_targeted"),
      variable.name = 'data_type',
      value.name = 'Pred_error'
    )


  iso_err_dt[, grp_col := paste0(molecule_b, adduct_b, Grp_b, isoabb_b, sample_name_b)]

  iso_err_dt <- na.omit(iso_err_dt, cols = "diffH20PP")

  #Generate alignment error table
  ali_error_table <-
    rbindlist(list(c_table, nf_b_table), fill = TRUE)

  ali_error_table <- ali_error_table[, count_errors_max(.SD), .SDcols=c('molecule_b',
                                                    'adduct_b',
                                                    'main_peak',
                                                    'sample_id_b',
                                                    'isoabb_b',
                                                    'feature_id_g',
                                                    'peak_group_b',
                                                    'peak_area_g',
                                                    'peak_area_ug'),
                 by=.(molecule_b, adduct_b)]
  ali_error_table <- setnames(ali_error_table, c('V1', 'molecule_b', 'adduct_b'), c('errors', 'Molecule', 'Adduct'))

  ##############
  #Return the found and 3 notfoundtables in a list
  ##############

  return_list <- list('c_table' = c_table, 'nf_b_table' = nf_b_table, 'nf_ug_table' = nf_ug_table, 'nf_g_table' = nf_g_table, 'info_list' = info_list,
                      'split_table' = split_table, 'ff_table' = ff_table_dt, 'rs_table'=rs_table, 'iso_err_dt' = iso_err_dt, 'ali_error_table' = ali_error_table)
  ev_return_list <<- return_list
  toc()

  saveRDS(return_list, "RObject.rds")
  print('Compare Succesfull')


  return(return_list)
}
