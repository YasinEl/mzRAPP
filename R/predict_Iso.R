#' predict_Iso
#'
#' @description Takes the output of \code{\link{find_bench_peaks}} predicts peak areas as well as peak heights for lower abundant isotopologues
#' from the most abundant isotopologue
#'
#' @param DTT output of \code{\link{find_bench_peaks}}
#' @param SampleIdentifier_col name of column(s) with file names
#' @param Molecule_Adduct_col name of column(s) with molecule and adduct identifiers
#' @param isoab_col name of column with isotopic abundance information
#' @param flag_extremes whether outliers should be flagged (more than 30\% of in area or more than 30\% of in area but with area and height being in agreement within 30\%)
#'
#' @keywords internal
#'
predict_Iso <- function(DTT, SampleIdentifier_col, Molecule_Adduct_col, isoab_col, flag_extremes = FALSE, max_bias_area = 35, max_bias_height = 30, area_height_bias_diff = 30)
{

  DTT <- DTT[, eval(substitute(isoab_col)) := as.numeric(get(isoab_col))]


  newcols <- c("ExpectedArea", "ErrorRel_A", "ErrorAbs_A", "ExpectedHeight", "ErrorRel_H", "ErrorAbs_H")




  DT_tmp <- DTT[get(isoab_col) != 100][DTT[get(isoab_col) == 100],
                                        on=c(SampleIdentifier_col, Molecule_Adduct_col),
                                        nomatch = 0L, allow.cartesian=TRUE][,(newcols) := .((i.peaks.area * get(isoab_col)) / 100,
                                                                                            (peaks.area / ((i.peaks.area * get(isoab_col)) / 100) - 1) * 100,
                                                                                            peaks.area - ((i.peaks.area * get(isoab_col)) / 100),
                                                                                            i.peaks.height * get(isoab_col) / 100,
                                                                                            (peaks.height / ((i.peaks.height * get(isoab_col)) / 100) - 1) * 100,
                                                                                            peaks.height - ((i.peaks.height * get(isoab_col)) / 100))]


  Output <- merge(DTT, DT_tmp[,.(IDX, ExpectedArea, ErrorRel_A, ErrorAbs_A, ExpectedHeight, ErrorRel_H, ErrorAbs_H)], by = 'IDX', all.x = TRUE, allow.cartesian = TRUE)


  if(flag_extremes == TRUE){

    Output$isoab_ol <- TRUE
    Output[(abs(ErrorRel_A) < max_bias_area & abs(ErrorRel_H) < max_bias_height & abs(ErrorRel_H - ErrorRel_A) < area_height_bias_diff) | isoab == 100]$isoab_ol <- FALSE

  }
  Output
}
