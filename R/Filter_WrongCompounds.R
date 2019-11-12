#' Filter_WrongCompounds
#'
#' @param DTT
#' @param SampleIdentifier_col
#' @param Molecule_Adduct_col
#' @param IsoAbb_col
#'
#' @return
#' @export
#'
#' @examples
Filter_WrongCompounds <- function(PeakCandidates, ErrorRel_H_Mp1_tol = 10)
{

  #PeakCandidates$Index <- seq.int(nrow(PeakCandidates))

  DT_tmp <- setorder(PeakCandidates[isoabb < 100, .(fileIdx, molecule, adduct, isoabb, ErrorRel_H)],
  -isoabb)


  Mp1 <- unique(DT_tmp,
                by = c("molecule", "adduct", "fileIdx"))


  Mp1_valid <- Mp1[abs(ErrorRel_H) > ErrorRel_H_Mp1_tol, .(fileIdx, molecule, adduct, ErrorRel_H)]


  PeakCandidates_f <- unique(PeakCandidates[,.(fileIdx, molecule, adduct)])[Mp1_valid[,!c("ErrorRel_H")], on=.(fileIdx, molecule, adduct)][, WrongCompound := TRUE]


  Output <- PeakCandidates_f[PeakCandidates, on=.(fileIdx, molecule, adduct), allow.cartesian = TRUE]

  Output[is.na(WrongCompound)]$WrongCompound <- FALSE


  Output
}
