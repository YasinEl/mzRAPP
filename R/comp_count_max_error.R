#' count_errors_max
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
count_errors_max <- function(dt){
  theMolecule <- unique(dt$molecule_b)
  theAdduct  <- unique(dt$adduct_b)
  #dt <- dt[molecule_b == 'cis-Aconitate' & adduct_b == 'M-H']

  dt <- dt[main_peak=='TRUE' | is.na(main_peak), c('sample_id_b', 'isoabb_b', 'feature_id_g', 'molecule_b', 'adduct_b', 'peak_group_b', 'peak_area_g', 'peak_area_ug')]

  #Generate peak status Column
  ## -1 = Peak not found in g or ug (peakpicking error)
  ## -2 = Peak not found in g (grouping error)
  ## -3 = Different Peak in g and ug
  dt <- dt[, peak_status := ifelse((is.na(peak_area_g)) & (is.na(peak_area_ug)), "Lost_b.PP",
                                   ifelse((is.na(peak_area_g)) & (!is.na(peak_area_ug)), 'Lost_b.A',
                                          ifelse((!is.na(peak_area_g)) & (!is.na(peak_area_ug)) & (peak_area_g != peak_area_ug), -3, feature_id_g)))] # put -3 here

  #if(theMolecule == 'cis-Aconitate'){View(dt)}
  #Reformat Table (CHECK FOR DUPLICATES)
  dt <- dcast(dt, sample_id_b ~ isoabb_b, value.var='peak_status', fun.aggregate = function(x) paste(x, collapse = ""))
  #fwrite(dt, 'pre_loop_dt.csv')
  #if(theMolecule == 'cis-Aconitate'){View(dt)}

  theReturn <- count_alignment_errors(dt, get_main_UT_groups(dt))
  if(theReturn > 0){print(paste(theMolecule, theAdduct, theReturn, sep = ' | '))
    #fwrite(dt, paste0(theMolecule, theAdduct, theReturn, '.csv'))
    print('----')}

  return(theReturn)
}
