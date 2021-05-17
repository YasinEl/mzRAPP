#' import_ungrouped_Metaboanalyst
#'
#' @param file file
#' @param options_dt options_dt
#'
#'
#' @keywords internal
import_ungrouped_Metaboanalyst <- function(file, options_dt){



  message('Starting Metaboanalyst unaligned import')

  if(is.null(file)){
    stop('No ungrouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file) != 'csv'){
    stop('ungrouped dataset is not a valid csv file')
  }


  if(length(file) != 1){
    stop('There should only be 1 file for the unaligned Metaboanalyst output!')
  }

  #Import csv file
  ug_table <- data.table::fread(file)


  #Check if all columns defined in optionsframe are present
  ug_req_cols <- stats::na.omit(options_dt$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in unaligned Metaboanalyst output: ', paste0(cols_not_found, sep = " - "))
  }


  #rename all columns for internal use according to optiosn frame
  ug_table <- rename_columns_from_options(ug_table, options_dt, 'ug_columns', 'internal_columns')

  #correct ug_sample_IDs
  options_dt[, ug_samples := ug_samples + max(ug_table$sample_name, na.rm = TRUE)]

  #Add a sample_id column based on the sample_names in options_dt
  ug_table <- ug_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  message(paste0('Successful Metaboanalyst unaligned import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}


#' import_grouped_Metaboanalyst
#'
#' @param file file
#' @param options_dt options_dt
#'
#'
#' @keywords internal
import_grouped_Metaboanalyst <- function (file, options_dt) {

  message('Starting Metaboanalyst aligned import')

  if(is.null(file)){
    return(NULL)
    stop('No grouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file) != 'csv'){
    stop('ungrouped dataset is not a valid csv file')
  }




  #Import csv file
  g_table <- data.table::fread(file)

  if(!"Sample" %in% colnames(g_table)){
    stop('Column "Sample" is missing in aligned output.')
  }

  g_table <- g_table[Sample != "Label"]
  g_table <- g_table[, c("mz", "rt") := data.table::tstrsplit(Sample, "@", fixed=TRUE)]
  data.table::setcolorder(g_table, c("mz", "rt", "Sample", colnames(g_table)[! colnames(g_table) %in% c("mz", "rt", "Sample")]))

  #Check if all columns defined in optionsframe are present
  g_req_cols <- stats::na.omit(options_dt$g_columns)
  colnames(g_table) <- tools::file_path_sans_ext(colnames(g_table))
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in aligned Metaboanalyst output: ', paste0(cols_not_found, sep = " - "))
  }

  #Compare order of present samples in options_dt to grouped output and update options_dt
  nt_samples <- colnames(g_table)
  #nt_ids <- match(na.omit(options_dt$g_samples), nt_samples)
  options_dt[!is.na(g_samples), ug_samples := match(stats::na.omit(options_dt$g_samples), nt_samples) - length(g_table)] #columns not containing samples have to be substracted in ug_import

  #Add feature_id for each row
  g_table$feature_id <- seq.int(nrow(g_table))


  #Transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(stats::na.omit(options_dt[['g_columns']]), 'feature_id')
  measure_vars = stats::na.omit(options_dt[!is.na(ug_samples), g_samples])
  g_table <- data.table::melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_dt, 'g_columns', 'internal_columns')

  #Add a sample_id column based on the sample_names in options_dt
  g_table <- g_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'g_samples')]

  #Remove peaks where area is below 0
  g_table[, peak_area := as.numeric(peak_area)]
  g_table[, rt := as.numeric(rt)]
  g_table[, mz := as.numeric(mz)]
  g_table <- g_table[peak_area > 0]

  #Remove identical peaks
  #g_table <- remove_identical_peaks(g_table, grouped = TRUE)

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  message(paste0('Successful Metaboanalyst aligned import. No. of peaks imported: ', nrow(g_table)))

  return(list(g_table = g_table, options_dt = options_dt))
}
