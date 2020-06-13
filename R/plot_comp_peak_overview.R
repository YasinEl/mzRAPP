#' plot_comp_peak_overview
#'
#' @param comparison_data
#' @param mol_c
#' @param add_c
#' @param ia_c
#'
#' @return
#' @export
#'
#' @examples
plot_comp_peak_overview <- function(comparison_data, mol_c, add_c, ia_c) {

  plot_dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)

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
