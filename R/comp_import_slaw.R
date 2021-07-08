#' import_ungrouped_slaw
#'
#' @param file_list file_list
#' @param options_dt options_dt
#'
#'
#' @keywords internal
import_ungrouped_slaw <- function(file_list, options_dt){



  message('Starting SLAW unaligned import')

  if(is.null(file_list)){
    stop('No ungrouped files selected')
  }

  if(length(file_list) < 2){
    stop('There should be multiple files for the unaligned SLAW output!')
  }

  ug_req_cols <- stats::na.omit(options_dt$ug_columns)


  for (i in 1:length(file_list)){
    file_path <- file_list[i]
    file_name <- tools::file_path_sans_ext(basename(file_path))

    #Check if ug_table exists, if not: create
    if(!exists("ug_table")){
      ug_table <- data.table::fread(file_path)
      if(!all(ug_req_cols %in% colnames(ug_table))) {stop(paste("There are SLAW specific columns missing in " , file_path))}
      ug_table <- ug_table[, sample_name := file_name]
    } else if (exists("ug_table")){
      temp_data <- data.table::fread(file_path)
      temp_data <- temp_data[, sample_name := file_name]
      ug_table <- rbind(ug_table, temp_data)
    }
  }




  #Check if all columns defined in optionsframe are present
  ug_req_cols <- stats::na.omit(options_dt$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in unaligned SLAW output: ', paste0(cols_not_found, sep = " - "))
  }


  #rename all columns for internal use according to options frame
  ug_table <- rename_columns_from_options(ug_table, options_dt, 'ug_columns', 'internal_columns')

  #Add a sample_id based on the sample_names in options_dt
  ug_table <- ug_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Multiply rt by 60 to convert min to seconds ##MAKE OPTIONAL LATER
  ug_table[, ':=' (rt = rt*60, rt_start = rt_start*60, rt_end = rt_end*60)]

  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  message(paste0('Successful SLAW unaligned import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}


#' import_grouped_slaw
#'
#' @param file file
#' @param options_dt options_dt
#'
#'
#' @keywords internal
import_grouped_slaw <- function (file, options_dt) {

  message('Starting SLAW aligned import')

  if(is.null(file)){
    return(NULL)
    stop('No grouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file) != 'csv'){
    stop('grouped dataset is not a valid csv file')
  }

  #Import csv file
  g_table <- data.table::fread(file)



  #Rename raw files by removing _intensity tag and file extension
  raw_files <- colnames(g_table)[grepl(".csv", colnames(g_table))]
  raw_files_stspl <- strsplit(raw_files, "_")
  raw_files <-
    lapply(raw_files_stspl, function(x){

      paste0(x[2:length(x)], collapse = "_")

    })
  colnames(g_table)[grepl(".csv", colnames(g_table))] <- tools::file_path_sans_ext(unlist(raw_files))


  #Check if all columns defined in optionsframe are present
  g_req_cols <- stats::na.omit(options_dt$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in aligned SLAW output: ', paste0(cols_not_found, sep = " - "))
  }


  #Add feature_id for each row
  g_table$feature_id <- seq.int(nrow(g_table))


  #Transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(stats::na.omit(options_dt[['g_columns']]), 'feature_id')
  measure_vars = stats::na.omit(options_dt[!is.na(ug_samples), g_samples])
  g_table <- data.table::melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')

  #rename all columns for internal use according to options frame
  g_table <- rename_columns_from_options(g_table, options_dt, 'g_columns', 'internal_columns')

  #Add a sample_id column based on the sample_names in options_dt
  g_table <- g_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'g_samples')]

  #Remove peaks where area is below 0
  g_table <- g_table[peak_area > 0]

  #Multiply rt by 60 to convert min to seconds MAKE OPTIONAL LATER
  g_table[, rt := rt*60]

  #Remove identical peaks
  #g_table <- remove_identical_peaks(g_table, grouped = TRUE)

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  message(paste0('Successful SLAW aligned import. No. of peaks imported: ', nrow(g_table)))

  return(g_table)
}
