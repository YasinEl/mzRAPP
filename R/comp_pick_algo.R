#' check_nonTargeted_input
#'
#' @param ug_table_path path to unaligned table(s)
#' @param g_table_path path to aligned table
#' @param options_table output from \code{\link{check_benchmark_input}}
#' @param algo tool output format of ug_table and g_table. can be XCMS, XCMS3, El-Maven, OpenMS, MS-DIAL, CompoundDiscoverer or MZmine 2. Outputs from different tools can also be used as long as they are reformatted to one of those types.
#'
#' @return returns unaligned and aligned outputs from non-targeted tool in a format readable via \code{\link{compare_peaks}}
#' @export
#'
#'
check_nonTargeted_input <- function(ug_table_path, g_table_path, options_table = 'generate', algo){

  if(is.list(options_table) == TRUE && data.table::is.data.table(options_table) == FALSE){

    options_table <- options_table$options_table

  }

  switch(algo,
    'XCMS' = {
      g_table <- import_grouped_xcms(g_table_path, options_table)
      options_table <- g_table[["options_dt"]]
      g_table <- g_table[["g_table"]]
      ug_table <- import_ungrouped_xcms(ug_table_path, options_table)
    },
    'XCMS3' = {
      g_table <- import_grouped_xcms(g_table_path, options_table)
      ug_table <- import_ungrouped_xcms(ug_table_path, options_table)
    },
    'El-MAVEN' = {
      ug_table <- import_ungrouped_elmaven(ug_table_path, options_table)
      g_table <- import_grouped_elmaven(g_table_path, options_table)
    },
    'OpenMS' = {
      ug_table <- import_ungrouped_openms(ug_table_path, options_table)
      g_table <- import_grouped_openms(g_table_path, options_table)
    },
    'MS-DIAL' = {
      ug_table <- import_ungrouped_msdial(ug_table_path, options_table)
      g_table <- import_grouped_msdial(g_table_path, options_table)
    },
    'CompoundDiscoverer' = {
      ug_table <- import_ungrouped_cd(ug_table_path, options_table)
      g_table = NULL
    },
    'MZmine 2' = {
      ug_table <-import_ungrouped_mzmine(ug_table_path, options_table)
      g_table <- import_grouped_mzmine(g_table_path, options_table)
    },
    {return (NULL)})
  return(list('ug_table' = ug_table, 'g_table' = g_table))
}
