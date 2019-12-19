#Using same file for grouped and ungrouped

#' import_ungrouped_mzmine
#'
#' @param folder_path
#' @param options_table
#'
#' @return
#' @export
#'
#' @examples
import_ungrouped_mzmine <- function(folder_path, options_table){
  print('start ungroupd mzmine import')
  print(folder_path)

  for (i in rownames(folder_path)){
    row = folder_path[i, ]
    print(row)
    #Check if ug_table exists, if not: create
    if(!exists("temp_dt")){
      ug_table <- fread(row$datapath, integer64 = 'numeric')
      #get sample name from Peak Name column
      sample_name <- strsplit(names(ug_table)[grep(' Peak name$', names(ug_table))], ' Peak name')[[1]]
      print(sample_name)

      ug_table <- ug_table[, 'sample_name' := sample_name]

      cols_to_keep <- names(ug_table)[grep(paste0('^', sample_name), names(ug_table))]
      cols_to_keep <- c(cols_to_keep, 'sample_name')
      temp_dt <- ug_table[,cols_to_keep, with=FALSE]
      old_names <- names(temp_dt)[names(temp_dt)!='sample_name']
      new_names <- sapply(old_names, function(x) strsplit(x, paste0(sample_name, ' '))[[1]], USE.NAMES = FALSE)[2,]
      temp_dt <- setnames(temp_dt, old_names, new_names)

    } else {
      ug_table <- fread(row$datapath, integer64 = 'numeric')
      #get sample name from Peak Name column
      sample_name <- strsplit(names(ug_table)[grep(' Peak name$', names(ug_table))], ' Peak name')[[1]]
      print(sample_name)

      ug_table <- ug_table[, 'sample_name' := sample_name]

      cols_to_keep <- names(ug_table)[grep(paste0('^', sample_name), names(ug_table))]
      cols_to_keep <- c(cols_to_keep, 'sample_name')
      temp_data <- ug_table[,cols_to_keep, with=FALSE]
      old_names <- names(temp_data)[names(temp_data)!='sample_name']
      new_names <- sapply(old_names, function(x) strsplit(x, paste0(sample_name, ' '))[[1]], USE.NAMES = FALSE)[2,]
      temp_data <- setnames(temp_data, old_names, new_names)
      temp_dt <- rbind(temp_dt, temp_data)
      rm(temp_data)
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

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(ug_table, by=c('peak_area', 'mz', 'rt')))){
    ug_table <- ug_table[!duplicated(ug_table, by='peak_area')]
    warning('Duplicate peaks present in raw benchmark file')
  }

  #Add a sample_id column based on the sample_ids in options_table
  ug_table <- dt_map(ug_table, options_table, 'sample_name', 'ug_samples', 'sample_id', 'sample_id')

  #Generate id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  ug_table <- ug_table[, rt_start := rt_start*60]
  ug_table <- ug_table[, rt_end := rt_end*60]
  ug_table <- ug_table[, rt := rt*60]

  #copy ug_table as g_table
  g_table <- ug_table

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  #Generate sample/column combinations

  #Eliminate all columns not in options
  print(ug_table)

  return(ug_table)

}

#' import_grouped_mzmine
#'
#' @param file_path
#' @param options_table
#'
#' @return
#' @export
#'
#' @examples
import_grouped_mzmine <- function(file_path, options_table){

  #Check if filetype is csv
  print(file_ext(file_path))
  if(file_ext(file_path) != 'csv'){
    stop('ugrouped dataset is not a valid csv file')
  }
  ug_table <- fread(file_path)

  #Make sure options_table is valid
  if (!is.data.table(options_table)){
    stop('Options is not type DataTable')
  }

  #Generate sample/column combinations
  #columns_to_keep <- c(sapply(na.omit(options_table$ug_samples), paste, na.omit(options_table$ug_columns)))

  #Eliminate all columns not in options
  #ug_table <- ug_table[, columns_to_keep, with = FALSE]

  ##Generate melt pattern
  ##melt_pattern <- c(sapply('^', paste0, na.omit(options_table$ug_samples)))
  ##print(melt_pattern)

  ##Melt According to pattern

  ##ug_table <- melt(ug_table, measure=patterns(melt_pattern))

  #Add feature id for each row
  ug_table$feature_id <- seq.int(nrow(ug_table))

  #Split datatable into seperate tables for each file, rename, combine
  for (sample in na.omit(options_table$ug_samples)) {
    #check if temp dt exists, if not create, if yes append
    if (!exists('temp_dt')){
      #Adding feature_id to cols to keep
      cols_to_keep <- names(ug_table)[grep(paste0('^', sample), names(ug_table))]
      cols_to_keep <- c(cols_to_keep, 'feature_id')
      temp_dt <- ug_table[,cols_to_keep, with=FALSE]
      old_names <- names(temp_dt)[names(temp_dt)!='feature_id']
      new_names <- sapply(old_names, function(x) strsplit(x, paste0(sample, ' '))[[1]], USE.NAMES = FALSE)[2,]
      temp_dt <- setnames(temp_dt, old_names, new_names)
      temp_dt <- temp_dt[, sample_name := sample]
    } else {
      cols_to_keep <- names(ug_table)[grep(paste0('^', sample), names(ug_table))]
      cols_to_keep <- c(cols_to_keep, 'feature_id')
      temp_data <- ug_table[,cols_to_keep, with=FALSE]
      old_names <- names(temp_data)[names(temp_data)!='feature_id']
      new_names <- sapply(old_names, function(x) strsplit(x, paste0(sample, ' '))[[1]], USE.NAMES = FALSE)[2,]
      temp_data <- setnames(temp_data, old_names, new_names)
      temp_data <- temp_data[, sample_name := sample]
      temp_dt <- rbind(temp_dt, temp_data)
      rm(temp_data)
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

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0 & peak_height > 0]

  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(ug_table, by=c('peak_area', 'mz', 'rt')))){
    ug_table <- ug_table[!duplicated(ug_table, by='peak_area')]
    warning('Duplicate peaks present in raw benchmark file')
  }

  #Add a sample_id column based on the sample_ids in options_table
  ug_table <- dt_map(ug_table, options_table, 'sample_name', 'ug_samples', 'sample_id', 'sample_id')

  #Generate id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  ug_table <- ug_table[, rt_start := rt_start*60]
  ug_table <- ug_table[, rt_end := rt_end*60]
  ug_table <- ug_table[, rt := rt*60]

  #copy ug_table as g_table
  g_table <- ug_table

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  #Generate sample/column combinations

  #Eliminate all columns not in options
  return(g_table)
}

#import_mzmine('Y:/Max/TestFiles_Compare/mzMine/pHILIC_XCMS_JoinAligner.csv', import_options('Y:/Max/TestFiles_Compare/mzMine/options mzmine phlic.csv'))
