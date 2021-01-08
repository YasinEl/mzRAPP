#' check_benchmark_input
#'
#' Checks the benchmark dataset and brings it into a format readable by \code{\link{compare_peaks}}. All molecules for which the most abundant isotopolgue is not present,
#' or less than 2 isotopologues are present are deleted. Moreover, isotopologues which appear in only one file are deleted.
#'
#' @param file output of \code{\link{find_bench_peaks}}. Can be path to csv file or a data table  object (meaning that is.data.table(file) returns TRUE).
#' @param options_path can be a string "generate" in order to use default column names for chosen algo. In the future we might include a possibility to allow the user to choose column names.
#' @param from_csv TRUE or FALSE depending on file being a data.table object or a path to a csv
#' @param algo tool output format to compare the benchmark against. can be XCMS, XCMS3, El-Maven, OpenMS, msDial, CompoundDiscoverer or mzMine. Outputs from different tools can also be used as long as they are reformatted to one of those types.
#'
#' @return returns a list including the benchmark in a format readable by \code{\link{compare_peaks}}.
#' @export
#'
check_benchmark_input <- function (file, options_path = "generate", from_csv = TRUE, algo) {

  if(from_csv){
    if(is.null(file)){
      stop('No benchmark file selected')
    }
    #Make sure file points to a csv file
    if(tools::file_ext(file) != 'csv'){
      stop('benchmark is not a valid csv file')
    }

    #Import csv file
    b_table <- fread(file)
  } else {
    if (!is.data.table(file)){
      stop('Generated benchmark is not a datatable')
    } else {
      b_table <- copy(file)
    }
  }

  if(options_path == 'generate'){
    options_table <- generate_options(b_table, algo)
  } else {
    options_table <- import_options(options_path)
  }

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

  #rename all columns for internal use according to options frame
  b_table <- rename_columns_from_options(b_table, options_table, 'b_columns', 'internal_columns')

  #Remove peaks where height and area are below 0
  b_table <- b_table[peak_area > 0 & peak_height > 0]

  #Add a sample_id and grp_id column based on the sample_names in options_table
  b_table <- b_table[options_table, ':=' (sample_id = i.sample_id), on=c(sample_name = 'b_samples')]


  #Check for duplicate peaks, should not be present so warning, removing them if there
  if (any(duplicated(b_table, by=c('peak_area')))){
    b_table <- b_table[!duplicated(b_table, by=c('peak_area', 'rt'))]
    warning('Duplicate peaks removed from raw benchmark file')
  }

  #Generate feature ID to quickly detect features later
  b_table <- b_table[, feature_id := .GRP, by = c('molecule', 'adduct', 'isoab')]

  #Remove Features if only observed in one sample (if more than one sample is present in benchmark)
  if(length(unique(b_table$sample_name)) > 1){
    b_table[, ft_count := .N, by = .(feature_id)]
    b_table <- b_table[ft_count > 1, !c("ft_count")]
  }

  #Remove molecules for which only one isotopologue is present
  b_table[, iso_count2 := .N, by =.(molecule, adduct, sample_name)]
  b_table <- b_table[iso_count2 > 1, !c("iso_count2")]

  #Remove molecules if most abundant isotopologue is not present
  b_table[, maIso := any(isoab == "100"), by =.(molecule, adduct, sample_name)]
  b_table <- b_table[maIso == TRUE, !c("maIso")]

  #Generate id for each peak
  b_table$comp_id <- seq.int(nrow(b_table))

  #Add "_b" as suffix to each column name
  colnames(b_table) <- paste(colnames(b_table), 'b', sep = '_')

  return(list('b_table' = b_table, 'options_table' = options_table))
}
