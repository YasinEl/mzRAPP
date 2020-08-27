#' compare_peaks
#'
#' @param b_table
#' @param ug_table
#' @param g_table
#' @param algo
#'
#' @description
#'
#' @return
#' @export
#'
#' @examples
compare_peaks <- function(b_table, ug_table, g_table, algo){

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
  info_list <- append(info_list, list(nr_of_b_peaks = length(unique(b_table$comp_id_b)),
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
    print(paste0('Removed ',total_ug_peaks - nrow(ug_table), ' duplicate non-aligned peaks to prevent errors'))
  #}
  #if (any(duplicated(g_table, by = c('rt_g', 'mz_g', 'peak_area_g')))){
  #  print('Duplicate peaks present in grouped. This can lead to further errors during analysis.')
  #}

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
  c_table <- b_table[ug_table, on=.(sample_id_b_temp == sample_id_ug_temp,
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
  split_table <- rbindlist(list('split_left_table' = split_left_table, 'split_right_table' = split_right_table, 'split_middle_table' = split_middle_table), fill=TRUE, use.names = TRUE, idcol='file')



  print(paste('Before Main Peak Check: ', nrow(c_table)))
  c_table <- pick_main_peak(c_table)
  print(paste('After Main Peak Check: ', nrow(c_table[main_peak == TRUE])))


  c_table <- c_table[main_peak == TRUE]

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


  c_table[, N_fid := .N, by = .(molecule_b, adduct_b, isoab_b, feature_id_g)]
  c_table <-  c_table[order(-rank(N_fid))]
  c_table <- unique(c_table, by = c("molecule_b", "adduct_b", "isoab_b", "sample_id_b"))

  #Replace 0 in peak_area_g with NA (no idea why they appear in the first place)(maybe int64?)
  #c_table <- c_table[, peak_area_g := ifelse(peak_area_g == 0, NA, peak_area_g)]

  split_table <- g_table[split_table, on=.(peak_area_g_temp == peak_area_ug_temp),
                     allow.cartesian = TRUE, nomatch=NA, mult='all']

  #Remove _temp Columns
  c_table[,grep('_temp$', colnames(c_table)):=NULL]
  b_table[,grep('_temp$', colnames(b_table)):=NULL]


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



  ###################################################################################################################
  #feature_feature comparison
  ###################################################################################################################

  #Generate alignment error table
  if(nrow(g_table) > 0){


  ali_error_table <-
    rbindlist(list(c_table, nf_b_table), fill = TRUE)

  if('peak_area_rounded_ug' %in% colnames(ali_error_table)){
    ali_error_table <- ali_error_table[, 'peak_area_ug' := peak_area_rounded_ug]
  }

  ali_error_table <- ali_error_table[, as.list(count_errors_max(.SD)), .SDcols=c('molecule_b',
                                                    'adduct_b',
                                                    'main_peak',
                                                    'sample_id_b',
                                                    'isoab_b',
                                                    'feature_id_g',
                                                    'peak_area_g',
                                                    'peak_area_ug'),
                 by=.(molecule_b, adduct_b)]

  ali_error_table <- setnames(ali_error_table, c('errors', 'Lost_b.A', 'diff_BM', 'molecule_b', 'adduct_b'), c('Min.errors', 'Lost_b.A', 'BM.div', 'Molecule', 'Adduct'))

  } else {

    ali_error_table <- setNames(data.table(matrix(nrow = 0, ncol = 5)), c("Molecule", "Adduct", "Min.errors", "Lost_b.A", "BM.div"))

  }


  #Generate feature table
    print("feature table start")

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

  tmp <- unique(data.table(sample_id_b = as.factor(c_table[["sample_id_b"]]),
                           sample_name_b = c_table[["sample_name_b"]]))
  feature_table <- dt_n[tmp, on = .(sample_id_b)]

  ug_info <- rbindlist(list(c_table, nf_b_table), fill = TRUE, use.names = TRUE)

  feature_table <-
  feature_table[!is.na(area_b)][ug_info[, c("molecule_b",
                                            "adduct_b",
                                            "isoab_b",
                                            "sample_name_b",
                                            "peak_area_b",
                                            "peak_area_ug")],
                                on = .(molecule_b, adduct_b, isoab_b, sample_name_b)]

    } else {

      feature_table <- setNames(data.table(matrix(nrow = 0, ncol = 15)), c("feature_id_b", "feature_id_g", "molecule_b", "isoab_b", "adduct_b",
                                                                          "total_area_b", "min_mz_start", "max_mz_end", "min_rt_start",
                                                                          "max_rt_end", "main_feature", "sample_id_b", "area_g", "area_b",
                                                                          "sample_name_b"))
      ff_table_dt <- data.table(NULL)
    }



  #Generate Random and systematic error DT
  rs_table <- rbindlist(list(c_table, nf_b_table), fill = TRUE)

  rs_table <- rs_table[, c("molecule_b", "adduct_b", "isoab_b", "sample_name_b", "peak_area_b", "peak_height_b",
                           "peak_area_ug", "peak_area_g", "feature_id_g", "sample_id_b"
  )]

  print(nrow(rs_table))

  if(nrow(g_table) > 0){

 # feat_t <- melt_fftable(ff_table_dt, c_table)
#tt <<- feat_t
  rs_table <- feature_table[main_feature == TRUE & !is.na(area_b), c("molecule_b",
                                                                     "adduct_b",
                                                                     "isoab_b",
                                                                     "sample_name_b",
                                                                     "area_g")][rs_table,
                       on =.(molecule_b, adduct_b, isoab_b, sample_name_b)]

#tt <<- rs_table



  rs_table[, peak_area_g := area_g]



  rs_table <- rs_table[!is.na(peak_area_b)]
  rs_table <- rs_table[order(feature_id_g)]

   rs_table <-
    rs_table[, Connected := File_con_test(
      sample_name_b,
      feature_id_g),
      by = .(molecule_b, adduct_b)]

   colnames(rs_table) <- replace(colnames(rs_table), colnames(rs_table) == "area_g", "peak_area_g")

  } else {

    rs_table[, Connected := TRUE]
  }

  rs_table <-
    rs_table[, c("missing_peaks_ug", "missing_peaks_g") := .(find_r_s_error(
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



  rs_table <- rs_table[!is.na(peak_area_b)]



  #rs_table[, missing_peaks := find_r_s_error(
  #  peak_area_b,
  #  peak_area_ug,
  #  peak_height_b
  #), by = .(molecule_b, adduct_b, isoab_b)]


#Generate Isotopologe error dt
c_table_t <- c_table
if(nrow(g_table) > 0){

  c_table_t$sample_id_b <- as.factor(c_table_t$sample_id_b)
  iso_err_dt <- c_table_t[main_peak == TRUE,c("molecule_b",
                                   "adduct_b",
                                   "isoab_b",
                                   "sample_id_b",
                                   "peak_area_ug",
                                   "peak_area_b",
                                   "Grp_b",
                                   "sample_name_b")][feature_table[main_feature == TRUE &
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

  iso_err_dt[is.na(peak_area_b)]$peak_area_b <- iso_err_dt[is.na(peak_area_b)]$area_b
  iso_err_dt[is.na(sample_name_b)]$sample_name_b <- iso_err_dt[is.na(sample_name_b)]$i.sample_name_b


} else {
  c_table_t$sample_id_b <- as.factor(c_table_t$sample_id_b)
  c_table_t[, area_g := as.numeric(NA)]

  iso_err_dt <- c_table_t


}

  iso_err_dt <- iso_err_dt[isoab_b != 100][iso_err_dt[isoab_b == 100,
                                     c("sample_id_b", "sample_name_b", "molecule_b", "adduct_b", "area_g", "peak_area_b", "peak_area_ug")],
                                on=.(sample_id_b, molecule_b, adduct_b),
                                nomatch = NA, allow.cartesian=TRUE][,c("benchmark",
                                                                       "NPP_peak picking",
                                                                       "NPP_features") := .((peak_area_b / ((i.peak_area_b * isoab_b) / 100) - 1) * 100,
                                                                                            (peak_area_ug / ((i.peak_area_ug * isoab_b) / 100) - 1) * 100,
                                                                                            (area_g / ((i.area_g * isoab_b) / 100) - 1) * 100)]


  iso_err_dt[, diffH20PP_pp := as.character(abs(abs(benchmark) - abs(`NPP_peak picking`)) > 10 &
                                        abs(`NPP_peak picking` - benchmark) > 20 &
                                        abs(`NPP_peak picking`) > 30)]

  iso_err_dt[, diffH20PP_ft := as.character(abs(abs(benchmark) - abs(NPP_features)) > 10 &
                                        abs(NPP_features - benchmark) > 20 &
                                        abs(NPP_features) > 30)]


  iso_err_dt[diffH20PP_pp == "TRUE"]$diffH20PP_pp <- "Inc. > 20%p"
  iso_err_dt[diffH20PP_pp == "FALSE"]$diffH20PP_pp <- "Inc. < 20%p"

  iso_err_dt[diffH20PP_ft == "TRUE"]$diffH20PP_ft <- "Inc. > 20%p"
  iso_err_dt[diffH20PP_ft == "FALSE"]$diffH20PP_ft <- "Inc. < 20%p"

  iso_err_dt <- iso_err_dt[!is.na(peak_area_b)]


  ##############
  #Return the found and 3 notfoundtables in a list
  ##############

  return_list <- list('c_table' = c_table, 'nf_b_table' = nf_b_table, 'nf_ug_table' = nf_ug_table, 'nf_g_table' = nf_g_table, 'info_list' = info_list,
                      'split_table' = split_table, 'ff_table' = ff_table_dt, 'rs_table'= rs_table, 'iso_err_dt' = iso_err_dt, 'ali_error_table' = ali_error_table,
                      'feature_table' = feature_table)
  print('Compare Succesfull')


  return(return_list)
}
