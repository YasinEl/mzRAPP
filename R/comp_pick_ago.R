#' pick_algorithm
#'
#' @param ug_table_path
#' @param g_table_path
#' @param options_table
#' @param algo
#'
#' @return
#' @export
#'
#' @examples
pick_algorithm <- function(ug_table_path, g_table_path, options_table, algo){
  switch(algo,
    'XCMS' = {
    ug_table <- import_ungrouped_xcms(ug_table_path$datapath, options_table)
    if (is.null(g_table_path)){
      g_table = NULL
    } else {
      g_table <- import_grouped_xcms(g_table_path$datapath, options_table)
    }
    },
    'msDial' = {
      #####Pick the folder where file is located
      #####Remove if switch to text based path input
      ug_table <- import_ungrouped_msdial(ug_table_path, options_table)
      g_table <- import_grouped_msdial(g_table_path$datapath, options_table)
    },
    'CompoundDiscoverer' = {
      ug_table <- import_ungrouped_cd(ug_table_path$datapath, options_table)
      g_table = NULL
    },
    'mzMine' = {
      #import_tables <- import_ungrouped_mzmine(ug_table_path, options_table)
      #print(import_tables)
      ug_table <-import_ungrouped_mzmine(ug_table_path, options_table)
      print(class(ug_table))
      g_table <- import_grouped_mzmine(g_table_path$datapath, options_table)
      print(class(g_table))
    },
    {return (NULL)})
  return(list('ug_table' = ug_table, 'g_table' = g_table))
}


#options_table <- import_options('Y:/Max/TestFiles_Compare/msdial/optionsframe msdialHILIC.csv')
#pick_algorithm('Y:/Max/TestFiles_Compare/msdial/ungrouped/0_C13_1.txt', 'Y:/Max/TestFiles_Compare/msdial/Area_0_20188301418.txt', options_table, 'msDial')
