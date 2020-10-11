#' plot_comp_peak_overview
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#' @param mol_c molecule
#' @param add_c adduct
#' @param ia_c isotopic abundance rounded to 2 digits
#'
#' @return plotly object
#' @export
#'
plot_comp_peak_overview <- function(comparison_data, mol_c, add_c, ia_c) {

  plot_dt <-  rbindlist(list(comparison_data$Matches_BM_NPPpeaks, comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)

  if(nrow(plot_dt[molecule_b == mol_c & adduct_b == add_c & round(isoab_b, 2) == ia_c]) > 0){

    p <- plot_Peak_per_mol(
          plot_dt,
          mol = mol_c,
          add = add_c,
          ia = ia_c
        )

    return(p)
  } else {
      return(NULL)
  }
}
