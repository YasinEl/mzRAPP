#' import_ungrouped_xcms
#'
#' @param file_path
#' @param options_table
#'
#' @return
#' @export
#'
#' @examples
import_ungrouped_xcms <- function(file_path, options_table){

  print('start xcms import')

  #Check if filetype is csv
  print(file_ext(file_path))
  if(file_ext(file_path) != 'csv'){
    stop('ungrouped dataset is not a valid csv file')
  }

  #Import csv file
  ug_table <- fread(file_path)

  #Make sure options_table is valid
  if (!is.data.table(options_table)){
    stop('Options is not type DataTable')
  }

  #Check if all columns defined in optionsframe are present
  ug_req_cols <- na.omit(options_table$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to optiosn frame
  ug_table <- rename_columns_from_options(ug_table, options_table, 'ug_columns', 'internal_columns')

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(ug_table, by=c('peak_area', 'mz', 'rt')))){
    ug_table <- ug_table[!duplicated(ug_table, by='peak_area')]
    warning('Duplicate peaks present in raw ut file')
  }

  #Filter out samples not present in ug_samples
  ug_table <- filter_by_vector(ug_table, 'sample_name', options_table[,ug_samples])

  #Add a sample_id column based on the sample_ids in options_table
  ug_table <- dt_map(ug_table, options_table, 'sample_name', 'ug_samples', 'sample_id', 'sample_id')

  #Generate id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')


  return(ug_table)
}


#' import_grouped_xcms
#'
#' @param file_path
#' @param options_table
#'
#' @return
#' @export
#'
#' @examples
import_grouped_xcms <- function (file_path, options_table) {

  #Check if filetype is csv
  if(file_ext(file_path) != 'csv'){
    stop('ungrouped dataset is not a valid csv file')
  }

  #Import csv file
  g_table <- fread(file_path)

  #Make sure options_table is valid
  if (!is.data.table(options_table)){
    stop('Options is not type DataTable')
  }

  #Check if all columns defined in optionsframe are present
  g_req_cols <- na.omit(options_table$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #Add feature id for each row
  g_table$feature_id <- seq.int(nrow(g_table))

  #########
  #Transforming table from wide to long format
  #Creating 1 peak-per-row format
  #########
  id_vars <- append(na.omit(options_table[['g_columns']]), 'feature_id')
  measure_vars = na.omit(options_table[, g_samples])
  g_table <- melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_table, 'g_columns', 'internal_columns')

  #Remove peaks where area is below 0
  g_table <- g_table[peak_area > 0]

  #needs to be investigated further, no duplicates should be present
  #removing for now to make statistics work
  fwrite(g_table[duplicated(g_table, by=c('peak_area', 'sample_name'))|duplicated(g_table, by=c('peak_area', 'sample_name'), fromLast = TRUE)], file = 'gdup.csv')
  print(nrow(g_table))
  #g_table <- g_table[!duplicated(g_table, by=c('peak_area', 'sample_name'))]
  print(nrow(g_table[!duplicated(g_table, by=c('peak_area', 'sample_name'))]))
  print('KEEPING DUPLICATE ROWS')



  g_table <- filter_by_vector(g_table, 'sample_name', options_table[,g_samples])
  g_table <- dt_map(g_table, options_table, 'sample_name', 'g_samples', 'sample_id', 'sample_id')

  #Add a grp column based on the grp in options_table
  g_table <- dt_map(g_table, options_table, 'sample_name', 'g_samples', 'grp_id', 'grp')

  #Add ID field

  g_table$comp_id <- seq.int(nrow(g_table))

  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  return(g_table)
}
