#' import_ungrouped_msdial
#'
#' @param file_list
#' @param options_dt
#'
#' @return
#' @export
#'
#' @noRd
import_ungrouped_msdial <- function(file_list, options_dt){

  print('start msdial ug import')

  if(is.null(file_list)){
    stop('No ungrouped files selected')
  }

  if(length(file_list) < 2){
    stop('There should be multiple files for the unaligned MS-DIAL output!')
  }

  #Add files to ut dt if name is in options ug_samples
  for (i in 1:length(file_list)){
    file_path <- file_list[i]
    file_name <- tools::file_path_sans_ext(basename(file_path))

    #Check if ug_table exists, if not: create
    if(!exists("ug_table")){
      ug_table <- fread(file_path, integer64 = "numeric")
      if(!("Area" %in% names(ug_table))) {stop(paste("There are MS-DIAL specific columns missing in " , file_path))}
      ug_table <- ug_table[, sample_name := file_name]
    } else if (exists("ug_table")){
      temp_data <- fread(file_path, integer64 = "numeric")
      temp_data <- temp_data[, sample_name := file_name]
      ug_table <- rbind(ug_table, temp_data)
    }
  }


  #Check if all columns defined in optionsframe are present
  ug_req_cols <- na.omit(options_dt$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to option frame
  ug_table <- rename_columns_from_options(ug_table, options_dt, 'ug_columns', 'internal_columns')

  #Add a sample_id based on the sample_names in options_dt
  ug_table <- ug_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #make sure area is not bit64
  ug_table <- ug_table[, 'peak_area' := as.double(peak_area)]

  #Multiply rt by 60 to convert min to seconds ##MAKE OPTIONAL LATER
  ug_table[, ':=' (rt = rt*60, rt_start = rt_start*60, rt_end = rt_end*60)]

  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  # Add Rounding Column for later merge
  ug_table <- ug_table[, 'peak_area_rounded' := round(peak_area, 0)]

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  print(paste0('Successful msdial ug import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}

#' import_grouped_msdial
#'
#' @param file_path
#' @param options_dt
#'
#' @return
#' @export
#'
#' @noRd
import_grouped_msdial <- function(file_path, options_dt){

  print('start msdial g import')

  if(is.null(file_path)){
    return(NULL)
    stop('No grouped file selected')
  }

  #Check if filetype is text
  if(tools::file_ext(file_path) != 'txt'){
    stop('grouped dataset is not a valid txt file')
  }

  if(length(file) != 1){
    stop('There should only be only one file for the aligned MS-DIAL output!')
  }

  #Import text file
  #Make skip variable

  #options(datatable.verbose = TRU)

  g_table <- fread(file_path, skip=4, integer64 = "double", verbose = FALSE)


  #Check if all columns defined in optionsframe are present
  g_req_cols <- na.omit(options_dt$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #Add feature id for each row
  g_table$feature_id <- seq.int(nrow(g_table))

  #Transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(na.omit(options_dt[['g_columns']]), 'feature_id')
  measure_vars = na.omit(options_dt[, g_samples])
  g_table <- melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_dt, 'g_columns', 'internal_columns')


  #make sure area is not bit64
  g_table <- g_table[, 'peak_area' := as.double(peak_area)]


  #Add a sample_id column based on the sample_names in options_dt
  g_table <- g_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'g_samples')]

  #Remove peaks where area is below or equal 0
  g_table <- g_table[peak_area > 0]

  #Multiply rt by 60 to convert min to seconds MAKE OPTIONAL LATER
  g_table[, rt := rt*60]

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  print(paste0('Successful msdial g import. No. of peaks imported: ', nrow(g_table)))

  return(g_table)
}
