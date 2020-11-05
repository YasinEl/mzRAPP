#Using same file for grouped and ungrouped

#' import_ungrouped_mzmine
#'
#' @param folder_path folder_path
#' @param options_table options_table
#'
#'
#' @keywords internal
import_ungrouped_mzmine <- function(folder_path, options_table){
  message('Starting unaligned mzmine import')

  if(is.null(folder_path)){
    stop('No ungrouped files selected')
  }

  if(length(folder_path) < 2){
    stop('There should be multiple files for the unaligned mzMine output!')
  }

  for (i in 1:length(folder_path)){
    file_path <- folder_path[i]
    #Check if ug_table exists, if not: create
    if(!exists("temp_dt")){
      ug_table <- fread(file_path, integer64 = 'numeric')
      #get sample name from Peak Name column

      if(length(names(ug_table)[grep(' Peak name$', names(ug_table))]) < 1){
        stop(paste("It seems mzMine specific columns are missing in file ", file_path))
      }

      sample_name <- strsplit(names(ug_table)[grep(' Peak name$', names(ug_table))], ' Peak name')[[1]]

      ug_table <- ug_table[, 'sample_name' := sample_name]

      cols_to_keep <- names(ug_table)[grep(paste0('^', sample_name), names(ug_table))]
      cols_to_keep <- c(cols_to_keep, 'sample_name')
      temp_dt <- ug_table[,cols_to_keep, with=FALSE]
      old_names <- names(temp_dt)[names(temp_dt)!='sample_name']
      new_names <- sapply(old_names, function(x) strsplit(x, paste0(sample_name, ' '))[[1]], USE.NAMES = FALSE)[2,]
      temp_dt <- setnames(temp_dt, old_names, new_names)

    } else {
      ug_table <- fread(file_path, integer64 = 'numeric')
      #get sample name from Peak Name column
      sample_name <- strsplit(names(ug_table)[grep(' Peak name$', names(ug_table))], ' Peak name')[[1]]

      ug_table <- ug_table[, 'sample_name' := sample_name]

      cols_to_keep <- names(ug_table)[grep(paste0('^', sample_name), names(ug_table))]
      cols_to_keep <- c(cols_to_keep, 'sample_name')
      temp_data <- ug_table[,cols_to_keep, with=FALSE]
      old_names <- names(temp_data)[names(temp_data)!='sample_name']
      new_names <- sapply(old_names, function(x) strsplit(x, paste0(sample_name, ' '))[[1]], USE.NAMES = FALSE)[2,]
      temp_data <- setnames(temp_data, old_names, new_names)
      temp_dt <- rbind(temp_dt, temp_data)
    }
  }
  ug_table <- temp_dt

  #Check if all columns defined in optionsframe are present
  ug_req_cols <- na.omit(options_table$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in raw benchmark dataset: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to optiosn frame
  ug_table <- rename_columns_from_options(ug_table, options_table, 'ug_columns', 'internal_columns')

  #Rm file ext
  ug_table <- ug_table[, sample_name := tools::file_path_sans_ext(basename(sample_name))]

  #Add a sample_id column based on the sample_names in options_dt
  ug_table <- ug_table[options_table, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(ug_table, by=c('peak_area', 'mz', 'rt')))){
    ug_table <- ug_table[!duplicated(ug_table, by='peak_area')]
    warning('Duplicate peaks present in raw benchmark file')
  }

  #Multiply rt by 60 to convert min to seconds ##MAKE OPTIONAL LATER
  ug_table[, ':=' (rt = rt*60, rt_start = rt_start*60, rt_end = rt_end*60)]

  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  message(paste0('Successful mzMine unaligned import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)

}

#' import_grouped_mzmine
#'
#' @param file_path file_path
#' @param options_table options_table
#'
#' @return data.table
#'
#' @keywords internal
import_grouped_mzmine <- function(file_path, options_table){

  message('start mzMine aligned import')

  if(is.null(file_path)){
    return(NULL)
    stop('No grouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file_path) != 'csv'){
    stop('grouped dataset is not a valid csv file')
  }

  if(length(file_path) != 1){
    stop('There should only be one file for the aligned MS-DIAL output!')
  }

  #Import csv file
  g_table <- fread(file_path)

  #Check if all columns defined in optionsframe are present
  g_req_cols <- na.omit(options_table$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in grouped dataset: ', paste0(cols_not_found, sep = " - "))
  }


  #Add feature_id for each row
  g_table[, feature_id := seq.int(nrow(g_table))]


  #Removing file extensions from column names and transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(na.omit(options_table[['g_columns']]), 'feature_id')
  measure_vars <- paste0(na.omit(options_table[, g_samples]), ' Peak area')

  colnames(g_table) <-
    sapply(colnames(g_table), function(x){

      if(length(unlist(strsplit(x, " "))) > 1){
        unname(paste(tools::file_path_sans_ext(unlist(strsplit(x, " "))[1]),
                     paste(unlist(strsplit(x, " "))[2:length(unlist(strsplit(x, " ")))],
                           collapse = " ")))
      } else x
    },
    USE.NAMES = FALSE)

  g_table <- melt(g_table, id.vars = id_vars, measure.vars = measure_vars, variable.name = 'sample_name', value.name = 'peak_area')
  g_table <- g_table[, sample_name := tstrsplit(sample_name, ' Peak area')]

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_table, 'g_columns', 'internal_columns')

  #Add a sample_id column based on the sample_names in options_dt
  g_table <- g_table[options_table, ':=' (sample_id = i.sample_id), on=c(sample_name = 'g_samples')]

  #Remove peaks where area is below or equal 0
  g_table <- g_table[peak_area > 0]

  #Multiply rt by 60 to convert min to seconds MAKE OPTIONAL LATER
  g_table[, rt := rt*60]

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  message(paste0('Successful mzMine aligned import. No. of peaks imported: ', nrow(g_table)))

  return(g_table)
}
