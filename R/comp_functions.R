#' import_options
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
import_options <- function (file_path) {


  ####ADD CHECK FUNCTIONS
  options_table = fread(file_path, na.strings = c(""))
  return(options_table)
}


#Renames the columns of dt by replacing all names defined in vector old_columns by vector new_columns
#' rename_columns_from_options
#'
#' @param dt
#' @param options_table
#' @param old_columns
#' @param new_columns
#'
#' @return
#' @export
#'
#' @examples
rename_columns_from_options <- function(dt, options_dt, old_columns, new_columns) {

  rename_table = na.omit(options_dt[, c(old_columns, new_columns), with=FALSE], old_columns)
  return(setnames(dt, rename_table[[old_columns]], rename_table[[new_columns]]))
}


#' remove_identical_peaks
#'
#' @param dt
#' @param incl_height
#'
#' @return
#' @export
#'
#' @examples
remove_identical_peaks <- function(dt, grouped = FALSE){
  peaks_before <- nrow(dt)
  if (grouped == FALSE){
    dt <- dt[!duplicated(dt, by=c('peak_area', 'peak_height', 'mz', 'mz_start', 'mz_end', 'rt', 'rt_start', 'rt_end'))]
  } else {
    print( dt[duplicated(dt, by=c('peak_area', 'mz', 'rt')) | duplicated(dt, by=c('peak_area', 'mz', 'rt'), fromLast = TRUE)])
    dt <- dt[!duplicated(dt, by=c('peak_area', 'mz', 'rt'))]
  }
  peaks_removed <- peaks_before-nrow(dt)
  print(paste0('Removed ', peaks_removed,' identical peaks'))
  return(dt)
}

##-----------------------

#' find_main_feature
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
find_main_feature <- function(dt) {

  #Find most occuring Feature and place in main_feature column, place list ofother features into side features
  #Propably a more efficent way exists

  t <- table(dt$feature_id_g)
  if (length(t) > 0){
    main_feature <- names(t)[which.max(t)]
  } else {
    #requierd, other wise NULL and error
    main_feature <- character()
  }
  if (length(t) > 1){
    print(t)
    t <- as.data.table(t)
    side_feature <- t[V1 != main_feature][,V1]
    print(str(side_feature))

  } else {
    #requierd, other wise NULL and error
    side_feature <- character()
  }

  side_feature <- list(side_feature)
  print(list(main_feature, side_feature))
  return(list(main_feature, side_feature))
}

#' find_false_positive_feature
#'
#' @param found_table
#' @param not_found_table
#'
#' @return
#' @export
#'
#' @examples
find_false_positive_feature <- function (found_table, not_found_table){

}

calculate_cov <- ?function (dt, calc_column, group_column) {
  new_col_name <- paste0(calc_column, '_cov')
  cov_col <- dt[, .(temp_col = sd(get(calc_column))/mean(get(calc_column))*100), by=group_column]
  cov_col <- setnames(cov_col, c('temp_col'), c(new_col_name))
  print(cov_col)
  return(cov_col)
  }


find_r_s_error2 <- function(dt) {

  all_tables <- split(dt, by=c('molecule_b', 'adduct_b', 'grp'))
  for (i in seq_along(all_tables)) {
    sub_table <- all_tables[[i]]
    print(unique(sub_table$group_id_b))
    print(sub_table$peak_area_ug)
    sub_table <- sub_table[order(peak_area_b)]
    sub_table$order <- seq.int(nrow(sub_table))
    print(is.unsorted(sub_table$peak_area_ug))
    print(sub_table$peak_area_ug)
    print('--------')
    all_tables[[i]] <- sub_table
  }
  return(rbindlist(all_tables, fill=TRUE))
}




#' compare_peak_groups
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
compare_peak_groups <- function(dt) {

  #order DT by Iso abb, 100 als erstes
  dt <- dt[order(-isoabb_b)]

  #Create Named column for Molecule_Adduct_File
  dt <- dt[, iso_groups := paste(molecule_b, adduct_b, sample_id_b, sep="_")]
  dt <- dt[, features_in_iso_group := paste(isoabb_b, '-', feature_id_g, collapse = "; "), by=.(iso_groups, peak_group_b)]

  #dt <- dt[!duplicated(dt, by=c('iso_groups', 'peak_group_b', 'features_in_iso_group'))]
  #print(anyDuplicated(dt, by=c('molecule_b', 'adduct_b', 'sample_id_b', 'isoabb_b', 'peak_group_b')))

  dt <- dt[!duplicated(dt, by=c('molecule_b', 'adduct_b', 'sample_id_b', 'isoabb_b', 'peak_group_b'))]

  #fwrite(dt[molecule_b == 'Glutamate' & adduct_b == 'M-H'], file="peak_group_debug.csv")
  ##Long to Wide Transormation, filtering represents each ne dt passed inside function later
  #dt <- dt[molecule_b == 'Glutamate' & adduct_b == 'M-H']
  ##Only one row per peak_group - file pair
  #dt<- dt[!duplicated(dt, by=c('sample_name_b', 'peak_group_b'))]
  #dt <- dcast(dt, peak_group_b ~ sample_name_b, value.var='features_in_iso_group')
  ##dt <- dt[order(-isoabb_b)]
  #fwrite(dt, file="wide_peak_group_debug.csv")

  ##filtering represents each ne dt passed inside function later
  dt <- dt[molecule_b == 'Glutamate' & adduct_b == 'M-H']
  fwrite(dt, file="loop_debug_start.csv")

  #Only one row per peak_group - file pair
  dt<- dt[!duplicated(dt, by=c('sample_name_b', 'peak_group_b'))]
  #Reduce to File - Group - Vector
  dt <- dt[, c('sample_id_b', 'peak_group_b', 'features_in_iso_group')]
  #Turn iso group column into list
  dt <- dt[, features_in_iso_group := strsplit(features_in_iso_group, '; ')]

  #Start for loop. Take each Vector, compare to others and create a table
  print('start loop')
  dt_list <- list()

  for (row in 1:nrow(dt)){
    file <- dt[row, sample_id_b]
    group <- dt[row, peak_group_b]
    testVec <- dt[row, features_in_iso_group][[1]]
    #remove all NA
    testVec <- testVec[!grepl("NA", testVec)]


    #print(str(testVec))

    #Apply Comparison, eigentlich müsste ich nur über ein col loopen, sapply?
    if (!all(grepl("NA", testVec))){
      tempDT<- dt
      for (temp_row in 1:nrow(tempDT)) {
        indi_vec <- tempDT[temp_row, features_in_iso_group][[1]]
        ##remove all NA
        #indi_vec <- indi_vec[!indi_vec=='NA']

        #Find intersection without conting NA
        intersection <- intersect(testVec, indi_vec)
        number_intersection <- length(intersection)

        tempDT[temp_row, 'test_vec_col'] <- list(list(testVec))

        tempDT[temp_row, 'test_col'] <- list(list(intersection))

        tempDT[temp_row, 'test_col_number'] <- number_intersection

        max_intersect_DT <- tempDT[, .SD[test_col_number == max(test_col_number)], by=sample_id_b]
        max_intersect_DT_dup <- max_intersect_DT[duplicated(max_intersect_DT, by=c('sample_id_b')) | duplicated(max_intersect_DT, by=c('sample_id_b'), fromLast = TRUE)]
        max_intersect_DT_dup <- max_intersect_DT_dup[, test_col_number_comb := list(list(peak_group_b)), by=c('sample_id_b')]

        #max_intersect_DT <- max_intersect_DT[test_col_number > 1]
        max_intersect_DT <- max_intersect_DT[, file := file]
        max_intersect_DT <- max_intersect_DT[, group := group]
        #comb_list <- list(max_intersect_DT[!duplicated(max_intersect_DT, by=c('sample_id_b'))], max_intersect_DT_dup[!duplicated(max_intersect_DT_dup, by=c('sample_id_b'))])
        #combined_output <- rbindlist(comb_list, use.names = TRUE, fill=TRUE)
        #combined_output <- combined_output[,test_col_number_comb := ifelse(is.na(test_col_number_comb), test_col_number, test_col_number_comb)]
        #print(combined_output)
      }

      #For Debug

      if (row == 67){
        #print(tempDT)
        #print(str(combined_output$test_col_number_comb))
        #print(is.na(combined_output$test_col_number_comb))
        #max_intersect_DT <- dcast(max_intersect_DT, file + group ~ sample_id_b, fun=function(x) paste(x, collapse="_"), value.var='peak_group_b')
        View(tempDT)
        fwrite(tempDT, file=paste0('loop_debug_',row,'.csv'))
        fwrite(max_intersect_DT, file=paste0('max_intersect_DT_',row,'.csv'))
        fwrite(max_intersect_DT_dup, file=paste0('max_intersect_DT_dup_',row,'.csv'))
        #fwrite(combined_output, file='test.csv')#paste0('indersect_combined_',row,'.csv'))
      }
      dt_list <- append(dt_list, list(max_intersect_DT))
    }
  }
  #print(dt_list)
  final_dt <- rbindlist(dt_list, use.names=TRUE, fill=TRUE)
  final_dt <- dcast(final_dt, file + group ~ sample_id_b, fun=function(x) paste(x, collapse="_"), value.var='peak_group_b')


  fwrite(final_dt, file="loop_debug.csv")

}


#' compare_each_row
#'
#' @param test_vec
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
compare_each_row <- function(test_vec, dt){

}

compare_peak_groups_new <- function(dt){
  ##filtering represents each ne dt passed inside function later
  dt <- dt[molecule_b == 'Glutamate' & adduct_b == 'M-H']
  dt <- dt[, c('sample_id_b', 'isoabb_b', 'feature_id_g', 'molecule_b', 'adduct_b', 'peak_group_b')]
  #Only use Peakgroup 1, will be filterd in benchmark later
  dt <- dt[peak_group_b == 2]

  #Converting columns to single string, than splitting to list, there must be a easier way
  #strsplit seperate to avoid grouping
  dt <- dt[, features_in_iso_group := paste(isoabb_b, '-', feature_id_g, collapse = "; "), by=.(molecule_b, adduct_b, sample_id_b, peak_group_b)]
  dt <- dt[, features_in_iso_group := strsplit(features_in_iso_group, '; ')]
  dt <- dt[, file_group := paste(sample_id_b)]
  #fwrite(dt, 'pre_loop_dt.csv')


  dt_list <- list()
  for (row in 1:nrow(dt)){
      temp_dt = NULL
      file <- dt[row, sample_id_b]
      group <- dt[row, peak_group_b]
      testVec <- dt[row, features_in_iso_group][[1]]
      #Remove NA
      testVec <- testVec[!grepl("NA", testVec)]
      if (!all(grepl("NA", testVec))){
        temp_dt <- dt
        temp_dt <- temp_dt[, sample_id_view := file]
        temp_dt <- temp_dt[, test_iso_cols := list(list(testVec))]
        temp_dt <- temp_dt[, overlap_iso := lapply(temp_dt$features_in_iso_group, function(x) intersect(testVec, x))]
        #fwrite(temp_dt, file=paste0('temp_dt', row, '.csv'))
        dt_list <- append(dt_list, list(copy(temp_dt)))
      }
  }
  #print(dt_list)
  compared_dt <- rbindlist(dt_list, use.names = TRUE, fill =TRUE)
  #filter out duplicates by only file id and sample_id_view
  compared_dt <- compared_dt[!duplicated(compared_dt, by=c('sample_id_b', 'sample_id_view'))]
  compared_dt <- dcast(compared_dt, sample_id_b ~ sample_id_view, value.var='overlap_iso')
  #fwrite(compared_dt, file='DEBUG.csv')
}


#' count_errors_max
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
count_errors_max <- function(dt){

  dt <- dt[main_peak == "TRUE" | is.na(main_peak), c('sample_id_b', 'isoabb_b', 'feature_id_g', 'molecule_b', 'adduct_b', 'peak_group_b', 'peak_area_g', 'peak_area_ug')]

  dt <- dt[, peak_status := ifelse(is.na(peak_area_g) & is.na(peak_area_ug), "Lost_b.PP",
                                   ifelse(is.na(peak_area_g) & !is.na(peak_area_ug), 'Lost_b.A',
                                          ifelse(!is.na(peak_area_g) & !is.na(peak_area_ug) & peak_area_g != peak_area_ug, -3, feature_id_g)))]

  if(nrow(dt) == 0){return(NA_integer_)}
  #Reformat Table (CHECK FOR DUPLICATES)

  dt <- dcast(dt, sample_id_b ~ isoabb_b, value.var='peak_status', fun.aggregate = function(x) paste(x, collapse = ""))

  theReturn <- count_alignment_errors(dt, get_main_UT_groups(dt))

  return(theReturn)
}



