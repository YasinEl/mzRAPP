#' Alignment_error_plot
#'
#' @param dt
#' @param mol
#' @param add
#'
#' @return
#' @export
#'
#' @examples
Alignment_error_plot <- function(comparison_data, mol, add){

  dt <- rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)

  if('peak_area_rounded_ug' %in% colnames(dt)){
    dt <- dt[, 'peak_area_ug' := peak_area_rounded_ug]
  }


  dt <- dt[(main_peak == "TRUE" | is.na(main_peak)) &
             molecule_b == mol &
             adduct_b == add, c('sample_id_b',
                                'main_peak',
                                'isoabb_b',
                                'feature_id_g',
                                'molecule_b',
                                'adduct_b',
                                'peak_group_b',
                                'peak_area_g',
                                'peak_area_ug',
                                'sample_name_b')]


  dt <- dt[, peak_status := ifelse(is.na(peak_area_g) & is.na(peak_area_ug), "Lost_b.PP",
                                   ifelse(is.na(peak_area_g) & !is.na(peak_area_ug), 'Lost_b.A',
                                          ifelse(!is.na(peak_area_g) & !is.na(peak_area_ug) & peak_area_g != peak_area_ug, -3, feature_id_g)))]

  fwrite(dt, 'align_debug.csv')


  if(nrow(dt) == 0){return(NA_integer_)}

  dt_for_error_count <- dcast(dt, sample_id_b ~ isoabb_b, value.var='peak_status', fun.aggregate = function(x) paste(x, collapse = ""))

  warumDAS <<- dt_for_error_count

  error_count <- count_alignment_errors(dt_for_error_count, get_main_UT_groups(dt_for_error_count))



  p <- ggplot(dt, aes(x = as.character(sample_name_b),
                      y = as.factor(round(isoabb_b, 2)),
                      peak_area_ug = peak_area_ug)) +
    geom_tile(aes(fill = as.character(peak_status), width = 0.5, height = 0.5), color = "white") +
    coord_equal() +
    ggtitle(paste0("Alignment of ", mol, " ", add, " | Min. errors: ",ifelse(is.na(error_count), 0, error_count))) +
    labs(x = "Samples", y = "Isotopologues") +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


  return(plotly::ggplotly(p))


}




















