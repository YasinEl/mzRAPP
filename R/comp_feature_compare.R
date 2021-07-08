#' feature_compare
#'
#' @param b_table b_table
#' @param g_table g_table
#' @param areaMatch_table areaMatch_table
#'
#'
#' @keywords internal
#'
feature_compare <- function(b_table, g_table, areaMatch_table = NA){

  #Find smallest and largest mz and rt and area per BM feature to fine NPP candidates
  b_table <- b_table[, ':=' (min_mz_start = min(mz_start_b),
                             max_mz_end = max(mz_end_b),
                             min_rt_start = min(peak_core_rt_range_start_b),# - if(is.na(sd(rt_b))){ 0} else stats::sd(rt_b),
                             max_rt_end = max(peak_core_rt_range_end_b),# + if(is.na(sd(rt_b))){ 0} else stats::sd(rt_b),
                             total_area_b = sum(peak_area_b),
                             present_samples_b = paste(.SD$sample_id_b, collapse = ','),
                             sample_id_b_suf = paste0('sample_',sample_id_b, '_b')), by=c('molecule_b', 'isoab_b', 'adduct_b')]


  #Bring b_table into wide format
  b_table <- data.table::dcast(b_table, feature_id_b + molecule_b + isoab_b + adduct_b + total_area_b + min_mz_start + max_mz_end + min_rt_start + max_rt_end + present_samples_b ~ sample_id_b_suf, value.var=c('peak_area_b'))

  #add NPP feature candidates if areas matched from NPP peaks
  if(length(areaMatch_table) > 1){
    areaMatch_table <- areaMatch_table[b_table, on = .(feature_id_b)]
  }



  b_table[, min_mz_start_temp := min_mz_start]
  b_table[, max_mz_end_temp := max_mz_end]
  b_table[, min_rt_start_temp := min_rt_start]
  b_table[, max_rt_end_temp := max_rt_end]


  #Calculate total area of g feature
  g_table <- g_table[, ':=' (total_area_g= sum(peak_area_g),
                             present_samples_g = paste(.SD$sample_id_g, collapse = ','),
                             sample_id_g_suf = paste0('sample_',sample_id_g, '_g')), by=c('feature_id_g')]

  #Bring g_table into wide format
  g_table <- data.table::dcast(g_table, feature_id_g + total_area_g + rt_g + mz_g + present_samples_g ~ sample_id_g_suf, value.var = c('peak_area_g'))


  if(length(areaMatch_table) > 1){
    areaMatch_table <- g_table[areaMatch_table, on = .(feature_id_g)][!is.na(feature_id_g) & !is.na(feature_id_b)]
  }


  #Merge
  cf_table <- b_table[g_table, on=.(min_mz_start_temp <= mz_g,
                                    max_mz_end_temp >= mz_g,
                                    min_rt_start_temp <= rt_g,
                                    max_rt_end_temp >= rt_g), allow.cartesian=TRUE, nomatch=NULL, mult='all']

  cf_table <- cf_table[, !c("min_mz_start_temp", "max_mz_end_temp", "min_rt_start_temp", "max_rt_end_temp")]


  if(length(areaMatch_table) > 1){
    cf_table <- data.table::rbindlist(list(cf_table, areaMatch_table[, !c("rt_g", "mz_g")]), use.names = TRUE)
    cf_table <- unique(cf_table)
  }


  if(nrow(cf_table) == 1){

    cf_table$samples_to_compare <- paste0(apply(cf_table,1,function(x){paste(intersect(unlist(strsplit(x['present_samples_g'], ',')), unlist(strsplit(x['present_samples_b'], ','))))}), collapse = ",")

  }else{

    cf_table$samples_to_compare <- apply(cf_table,1,function(x){paste(intersect(unlist(strsplit(x['present_samples_g'], ',')), unlist(strsplit(x['present_samples_b'], ','))))})

  }


  return(cf_table)
}
