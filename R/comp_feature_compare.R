#' feature_compare
#'
#' @param b_table
#' @param g_table
#'
#' @return
#' @export
#'
#' @examples
feature_compare <- function(b_table, g_table){

  #Find smallest and largest mz and rt and area per feature
  b_table <- b_table[, ':=' (min_mz_start = min(mz_start_b),
                             max_mz_end = max(mz_end_b),
                             min_rt_start = min(new_rt_start_b),
                             max_rt_end = max(new_rt_end_b),
                             total_area_b = sum(peak_area_b),
                             present_samples_b = paste(.SD$sample_id_b, collapse = ','),
                             sample_id_b_suf = paste0('sample_',sample_id_b, '_b')), by=c('molecule_b', 'isoabb_b', 'adduct_b')]


  #Bring b_table into wide format
  b_table <- dcast(b_table, feature_id_b + molecule_b + isoabb_b + adduct_b + total_area_b + min_mz_start + max_mz_end + min_rt_start + max_rt_end + present_samples_b ~ sample_id_b_suf, value.var=c('peak_area_b'))

  #Calculate total area of g feature
  g_table <- g_table[, ':=' (total_area_g= sum(peak_area_g),
                             present_samples_g = paste(.SD$sample_id_g, collapse = ','),
                             sample_id_g_suf = paste0('sample_',sample_id_g, '_g')), by=c('feature_id_g')]

  #Bring g_table into wide format
  g_table <- dcast(g_table, feature_id_g + total_area_g + rt_g + mz_g + present_samples_g ~ sample_id_g_suf, value.var = c('peak_area_g'))


  #Merge
  cf_table <- b_table[g_table, on=.(min_mz_start < mz_g,
                                    max_mz_end > mz_g,
                                    min_rt_start < rt_g,
                                    max_rt_end > rt_g), allow.cartesian=TRUE, nomatch=NULL, mult='all']


  cf_table$samples_to_compare <- apply(cf_table,1,function(x){paste(intersect(unlist(strsplit(x['present_samples_g'], ',')), unlist(strsplit(x['present_samples_b'], ','))))})
  #cf_table$samples_to_compare <- sapply(split(cf_table, row(cf_table)),function(x){paste(intersect(unlist(strsplit(x['present_samples_g'], ',')), unlist(strsplit(x['present_samples_b'], ','))))}, simplify = FALSE)

  return(cf_table)
}
