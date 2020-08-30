#' Alignment_error_plot
#'
#' @param mol Name of molecule
#' @param add Name of adduct
#' @param comparison_data output of compare_peaks
#'
#' @return plotly object
#' @export
#'
Alignment_error_plot <- function(comparison_data, mol, add){
  if(missing(mol) | missing(add) | missing(comparison_data) | length(mol) < 1 | length(add) < 1) return(plotly::ggplotly(ggplot() +
                                                                                                                           ggtitle("Missing arguments")))

  dt <- rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)

  if('peak_area_rounded_ug' %in% colnames(dt)){
    dt <- dt[, 'peak_area_ug' := peak_area_rounded_ug]
  }
  if(nrow(dt[(main_peak == "TRUE" | is.na(main_peak)) &
                                       molecule_b == mol &
                                       adduct_b == add]) == 0) return(plotly::ggplotly(ggplot() +
                                                                                         ggtitle("No peaks")))
suppressWarnings(
  dt <- dt[(main_peak == "TRUE" | is.na(main_peak)) &
             molecule_b == mol &
             adduct_b == add, c('sample_id_b',
                                'main_peak',
                                'isoab_b',
                                'feature_id_g',
                                'molecule_b',
                                'adduct_b',
                                'peak_area_g',
                                'peak_area_ug',
                                'sample_name_b')]

)

  dt <- dt[, peak_status := ifelse(is.na(peak_area_g) & is.na(peak_area_ug), "Lost_b.PP",
                                   ifelse(is.na(peak_area_g) & !is.na(peak_area_ug), 'Lost_b.A',
                                          ifelse(!is.na(peak_area_g) & !is.na(peak_area_ug) & peak_area_g != peak_area_ug, -3, feature_id_g)))]

  dt_for_error_count <- dcast(dt, sample_id_b ~ isoab_b, value.var='peak_status', fun.aggregate = function(x) paste(x, collapse = ""))

  error_count <- count_alignment_errors(dt_for_error_count, get_main_UT_groups(dt_for_error_count))[1]

  p <- ggplot(dt, aes(x = as.character(sample_name_b),
                      y = as.factor(round(isoab_b, 2)),
                      peak_area_ug = peak_area_ug)) +
    geom_tile(aes(fill = as.character(peak_status), width = 0.5, height = 0.5), color = "white") +
    coord_equal() +
    ggtitle(paste0("Alignment of ", mol, " ", add, " | Min. errors: ",ifelse(is.na(error_count), 0, error_count))) +
    labs(x = "Samples", y = "Isotopologues") +
    #scale_fill_manual(values=c(`Lost_b.A` = "firebrick", `Lost_b.A` = "orange")) +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(plotly::ggplotly(p))


}




















