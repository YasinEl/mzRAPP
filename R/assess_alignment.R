#' assess_alignment
#'
#' @param Matches_BM_NPPpeaks Matches_BM_NPPpeaks
#' @param Unmatched_BM_NPPpeaks Unmatched_BM_NPPpeaks
#' @param g_table g_table
#'
#' @keywords internal
#'
#'

assess_alignment <- function(Matches_BM_NPPpeaks,
                             Unmatched_BM_NPPpeaks,
                             g_table){


  if(nrow(g_table) > 0){


    AlignmentErrors_per_moleculeAndAdduct <-
      data.table::rbindlist(list(Matches_BM_NPPpeaks, Unmatched_BM_NPPpeaks), fill = TRUE)

    if('peak_area_rounded_ug' %in% colnames(AlignmentErrors_per_moleculeAndAdduct)){
      AlignmentErrors_per_moleculeAndAdduct <- AlignmentErrors_per_moleculeAndAdduct[, 'peak_area_ug' := peak_area_rounded_ug]
    }

    AlignmentErrors_per_moleculeAndAdduct <- AlignmentErrors_per_moleculeAndAdduct[, as.list(count_errors_max(.SD)), .SDcols=c('molecule_b',
                                                                                                                               'adduct_b',
                                                                                                                               'main_peak',
                                                                                                                               'sample_id_b',
                                                                                                                               'isoab_b',
                                                                                                                               'feature_id_g',
                                                                                                                               'peak_area_g',
                                                                                                                               'peak_area_ug'),
                                                                                   by=.(molecule_b, adduct_b)]

    AlignmentErrors_per_moleculeAndAdduct <- data.table::setnames(AlignmentErrors_per_moleculeAndAdduct, c('errors', 'Lost_b.A', 'diff_BM', 'molecule_b', 'adduct_b'), c('Min.errors', 'Lost_b.A', 'BM.div', 'Molecule', 'Adduct'))

  } else {

    AlignmentErrors_per_moleculeAndAdduct <- stats::setNames(data.table(matrix(nrow = 0, ncol = 5)), c("Molecule", "Adduct", "Min.errors", "Lost_b.A", "BM.div"))

  }

  return(AlignmentErrors_per_moleculeAndAdduct)

}
