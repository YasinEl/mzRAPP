#' import_ungrouped_xcms
#'
#' @param file
#' @param options_dt
#'
#' @return
#' @export
#'
#' @examples
import_ungrouped_xcms <- function(file, options_dt){

  print('start xcms ug import')

  #Check if filetype is csv
  if(file_ext(file) != 'csv'){
    stop('ungrouped dataset is not a valid csv file')
  }

  #Import csv file
  ug_table <- fread(file)

  #Check if all columns defined in optionsframe are present
  ug_req_cols <- na.omit(options_dt$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to optiosn frame
  ug_table <- rename_columns_from_options(ug_table, options_dt, 'ug_columns', 'internal_columns')

  #Add a sample_id and grp_id column based on the sample_names in options_dt
  ug_table <- ug_table[options_dt, ':=' (sample_id = i.sample_id, grp_id = i.grp_id), on=c(sample_name = 'ug_samples')]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Remove identical peaks
  ug_table <- remove_identical_peaks(ug_table)
  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  print(paste0('Successful xcms ug import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}


#' import_grouped_xcms
#'
#' @param file
#' @param options_dt
#'
#' @return
#' @export
#'
#' @examples
import_grouped_xcms <- function (file, options_dt) {

  print('start xcms g import')

  #Check if filetype is csv
  if(file_ext(file) != 'csv'){
    stop('ungrouped dataset is not a valid csv file')
  }

  #Import csv file
  g_table <- fread(file)

  #Check if all columns defined in optionsframe are present
  g_req_cols <- na.omit(options_dt$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #Add feature_id for each row
  g_table$feature_id <- seq.int(nrow(g_table))

  #Transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(na.omit(options_dt[['g_columns']]), 'feature_id')
  measure_vars = na.omit(options_dt[, g_samples])
  g_table <- melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_dt, 'g_columns', 'internal_columns')

  #Add a sample_id and grp_id column based on the sample_names in options_dt
  g_table <- g_table[options_dt, ':=' (sample_id = i.sample_id, grp_id = i.grp_id), on=c(sample_name = 'g_samples')]

  #Remove peaks where area is below 0
  g_table <- g_table[peak_area > 0]

  #Remove identical peaks
  g_table <- remove_identical_peaks(g_table, grouped = TRUE)

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  print(paste0('Successful xcms g import. No. of peaks imported: ', nrow(g_table)))

  return(g_table)
}
