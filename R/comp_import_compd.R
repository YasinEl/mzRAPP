#' import_ungrouped_cd
#'
#' @param file_path
#' @param options_table
#'
#' @return
#' @export
#'
#' @examples
import_ungrouped_cd <- function(file_path, options_table){

  print('Start import ungrouped compund discoverer')

  if(is.null(file_path)){
    stop('No ungrouped file selected')
  }
  #Check if filetype is csv
  if(file_ext(file_path) != 'csv'){impo
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
    stop('Columns defined in options but not present in Compound Discoverer ungrouped dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to optiosn frame
  ug_table <- rename_columns_from_options(ug_table, options_table, 'ug_columns', 'internal_columns')

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(ug_table, by=c('peak_area', 'mz', 'rt')))){
    ug_table <- ug_table[!duplicated(ug_table, by='peak_area')]
    warning('Duplicate peaks present in raw benchmark file')
  }

  #Filter out samples not present in ug_samples
  ug_table <- filter_by_vector(ug_table, 'sample_name', options_table[,ug_samples])

  #Add a sample_id column based on the sample_ids in options_table
  ug_table <- dt_map(ug_table, options_table, 'sample_name', 'ug_samples', 'sample_id', 'sample_id')

  #Generate id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  ug_table <- ug_table[, rt_start := rt_start*60]
  ug_table <- ug_table[, rt_end := rt_end*60]
  ug_table <- ug_table[, rt := rt*60]

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  return(ug_table)
}


