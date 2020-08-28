#' import_ungrouped_cd
#'
#' @param file_path file_path
#' @param options_table options_table
#'
#' @export
#'
#' @noRd
import_ungrouped_cd <- function(file_path, options_table){

  message('Start import unaligned compound discoverer')

  if(is.null(file_path)){
    stop('No ungrouped file selected')
  }
  #Check if filetype is csv
  if(tools::file_ext(file_path) != 'csv'){#impo
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

	#Add a sample_id column based on the sample_names in options_dt
  ug_table <- ug_table[options_table, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

	#Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

	#Multiply rt by 60 to convert min to seconds ##MAKE OPTIONAL LATER
  ug_table[, ':=' (rt = rt*60, rt_start = rt_start*60, rt_end = rt_end*60)]

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

	message(paste0('Successful import of unaligned Compound Discoverer output. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}


