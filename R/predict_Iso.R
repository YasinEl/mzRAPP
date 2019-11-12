#' predict_Iso
#'
#' @param DTT
#' @param SampleIdentifier_col
#' @param Molecule_Adduct_col
#' @param IsoAbb_col
#' @param flag_extremes
#'
#' @return
#' @export
#'
#' @examples
predict_Iso <- function(DTT, SampleIdentifier_col, Molecule_Adduct_col, IsoAbb_col, flag_extremes = FALSE)
{

  DTT <- DTT[, eval(substitute(IsoAbb_col)) := as.numeric(get(IsoAbb_col))]


  newcols <- c("ExpectedArea", "ErrorRel_A", "ErrorAbs_A", "ExpectedHeight", "ErrorRel_H", "ErrorAbs_H")




  DT_tmp <- DTT[get(IsoAbb_col) != 100][DTT[get(IsoAbb_col) == 100],
                                        on=c(SampleIdentifier_col, Molecule_Adduct_col),
                                        nomatch = 0L, allow.cartesian=TRUE][,(newcols) := .((i.peaks.area * get(IsoAbb_col)) / 100,
                                                                                            (peaks.area / ((i.peaks.area * get(IsoAbb_col)) / 100) - 1) * 100,
                                                                                            peaks.area - ((i.peaks.area * get(IsoAbb_col)) / 100),
                                                                                            i.peaks.height * get(IsoAbb_col) / 100,
                                                                                            (peaks.height / ((i.peaks.height * get(IsoAbb_col)) / 100) - 1) * 100,
                                                                                            peaks.height - ((i.peaks.height * get(IsoAbb_col)) / 100))]


  Output <- merge(DTT, DT_tmp[,.(IDX, ExpectedArea, ErrorRel_A, ErrorAbs_A, ExpectedHeight, ErrorRel_H, ErrorAbs_H)], by = 'IDX', all.x = TRUE, allow.cartesian = TRUE)


  if(flag_extremes == TRUE){

    Output$isoabb_ol <- TRUE
    Output[abs(ErrorRel_A) < 20 | (abs(ErrorRel_A) < 30 & abs(ErrorRel_H - ErrorRel_A) < 20) | isoabb == 100]$isoabb_ol <- FALSE

  }
  Output
}
