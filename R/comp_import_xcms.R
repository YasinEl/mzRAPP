#' import_ungrouped_xcms
#'
#' @param file file
#' @param options_dt options_dt
#'
#'
#' @keywords internal
import_ungrouped_xcms <- function(file, options_dt){

  message('Starting xcms unaligned import')

  if(is.null(file)){
    stop('No ungrouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file) != 'csv' & tools::file_ext(file) != "Rda"){
    stop('ungrouped dataset is not a valid csv (/Rda) file')
  }


  if(length(file) != 1){
    stop('There should only be 1 file for the unaligned XCMS output!')
  }

  if(tools::file_ext(file) == "Rda"){
    rda_file_v <- load(file = file, envir = environment())
    rda_file <- get(rda_file_v[1])
    ug_table <- as.data.table(xcms::peaks(rda_file))
  } else {
    #Import csv file
    ug_table <- fread(file)
  }



  #Check if all columns defined in optionsframe are present
  ug_req_cols <- na.omit(options_dt$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in unaligned XCMS output: ', paste0(cols_not_found, sep = " - "))
  }
  #rename all columns for internal use according to optiosn frame
  ug_table <- rename_columns_from_options(ug_table, options_dt, 'ug_columns', 'internal_columns')

  #Add a sample_id column based on the sample_names in options_dt
  ug_table <- ug_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]


  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  message(paste0('Successful xcms unaligned import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}


#' import_grouped_xcms
#'
#' @param file file
#' @param options_dt options_dt
#'
#'
#' @keywords internal
import_grouped_xcms <- function (file, options_dt) {

  message('Starting xcms aligned import')

  if(is.null(file)){
    return(NULL)
    stop('No grouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file) != 'csv' & tools::file_ext(file) != "Rda"){
    stop('ungrouped dataset is not a valid csv (/Rda) file')
  }
  if(tools::file_ext(file) == "Rda"){
    rda_file_v <- load(file = file, envir = environment())
    rda_file <- get(rda_file_v[1])
    g_table <- as.data.table(xcms::peakTable(rda_file))
  } else {
    #Import csv file
    g_table <- fread(file)
  }


  #Check if all columns defined in optionsframe are present
  g_req_cols <- na.omit(options_dt$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in aligned XCMS output: ', paste0(cols_not_found, sep = " - "))
  }

  #Add feature_id for each row
  g_table$feature_id <- seq.int(nrow(g_table))


  #Transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(na.omit(options_dt[['g_columns']]), 'feature_id')
  measure_vars = na.omit(options_dt[, g_samples])
  g_table <- melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_dt, 'g_columns', 'internal_columns')

  #Add a sample_id column based on the sample_names in options_dt
  g_table <- g_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'g_samples')]

  #Remove peaks where area is below 0
  g_table <- g_table[peak_area > 0]

  #Remove identical peaks
  #g_table <- remove_identical_peaks(g_table, grouped = TRUE)

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  message(paste0('Successful xcms aligned import. No. of peaks imported: ', nrow(g_table)))

  return(g_table)
}
