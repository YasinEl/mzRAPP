#' import_benchmark
#'
#' @param file_path
#' @param options_table
#' @param from_csv
#'
#' @return
#' @export
#'
#' @examples
import_benchmark <- function (file_path, options_table, from_csv = TRUE) {

  if(from_csv){
    #Make sure file_path points to a csv file
    if(file_ext(file_path) != 'csv'){
      stop('benchmark is not a valid csv file')
    }
    #Import csv file
    b_table = fread(file_path)
  }#######IMPLEMENT DIRECT USAGE OF GENERATED BENCHMARK!!!!!!!!!!!!!!!!!!!!

  #Make sure options_table is valid
  if (!is.data.table(options_table)){
    stop('Options is not type DataTable')
  }

  #Check if all columns defined in optionsframe are present
  b_req_cols <- na.omit(options_table$b_columns)
  if(!all(b_req_cols %in% colnames(b_table))){
    cols_not_found <- setdiff(b_req_cols, colnames(b_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to optiosn frame
  b_table <- rename_columns_from_options(b_table, options_table, 'b_columns', 'internal_columns')

  #Remove peaks where height and area are below 0
  b_table <- b_table[peak_area > 0 & peak_height > 0]

  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(b_table, by=c('peak_area', 'mz', 'rt')))){
    b_table <- b_table[!duplicated(b_table, by='peak_area')]
    warning('Duplicate peaks present in raw benchmark file')
  }

  #Filter out samples not present in ug_samples
  b_table <- filter_by_vector(b_table, 'sample_name', options_table[,b_samples])

  #Add a sample_id column based on the sample_ids in options_table
  b_table <- dt_map(b_table, options_table, 'sample_name', 'b_samples', 'sample_id', 'sample_id')
  #Add a grp column based on the grp in options_table
  b_table <- dt_map(b_table, options_table, 'sample_name', 'b_samples', 'grp_id', 'grp')


  #Generate feature ID to quickly detect features later
  b_table <- assign_groupID_column(b_table, 'feature_id', c('molecule', 'adduct', 'isoabb'))

  #Generate id for each peak
  b_table$comp_id <- seq.int(nrow(b_table))

  #Add "_b" as suffix to each column name
  colnames(b_table) <- paste(colnames(b_table), 'b', sep = '_')


  ##############################
  #b_table <- b_table[isoabb_b == 100]
  #print(nrow(b_table))
  ##############################

  return(b_table)
}
