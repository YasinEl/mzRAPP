#' import_options
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @noRd
import_options <- function (file_path) {
  print(file_path)
  if(is.null(file_path)){
    stop('No options file selected')
  }
  if(tools::file_ext(file_path) != 'csv'){
    stop('options file is not a valid csv file')
  }
  options_table = fread(file_path, na.strings = c(""))

  if (!is.data.table(options_table)){
    stop('Options is not type DataTable')
  }
  return(options_table)
}


#Renames the columns of dt by replacing all names defined in vector old_columns by vector new_columns
#' rename_columns_from_options
#'
#' @param dt
#' @param options_table
#' @param old_columns
#' @param new_columns
#'
#' @return
#' @export
#'
#' @noRd
rename_columns_from_options <- function(dt, options_dt, old_columns, new_columns) {

  rename_table = na.omit(options_dt[, c(old_columns, new_columns), with=FALSE], old_columns)
  return(setnames(dt, rename_table[[old_columns]], rename_table[[new_columns]]))
}


#' remove_identical_peaks
#'
#' @param dt
#' @param incl_height
#'
#' @return
#' @export
#'
#' @noRd
remove_identical_peaks <- function(dt, grouped = FALSE){
  peaks_before <- nrow(dt)
  if (grouped == FALSE){
    dt <- dt[!duplicated(dt, by=c('peak_area', 'peak_height', 'mz', 'mz_start', 'mz_end', 'rt', 'rt_start', 'rt_end'))]
  } else {
    print( dt[duplicated(dt, by=c('peak_area', 'mz', 'rt')) | duplicated(dt, by=c('peak_area', 'mz', 'rt'), fromLast = TRUE)])
    dt <- dt[!duplicated(dt, by=c('peak_area', 'mz', 'rt'))]
  }
  peaks_removed <- peaks_before-nrow(dt)
  print(paste0('Removed ', peaks_removed,' identical peaks'))
  return(dt)
}

