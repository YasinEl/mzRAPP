#' @title Filter_WrongIsoOrder
#'
#' @description
#' Takes a PeakCandidates table and lables peaks if their abundance or elution profile does not correspond to the order indicated by the theoretical isotopologue pattern.
#'
#' @param PeakCandidates x
#' @param SampleIdentifier column name or vector of column names which uniquely identify measured samples
#' @param MoleculeIdentifier column name or vector of column names which uniquely identify sample molecules
#' @param AdductIdentifier column name or vector of column names which uniquely identify molecule adducts
#' @param IsotoplogeIdentifier column name with numeric representation of theoretical isotopologue abundance
#'
#' @return PeakCandidates table with additional logical column: WrongIsoOrder (TRUE/FALSE)
#' @export
#'
#' @examples


Filter_WrongIsoOrder <- function(PeakCandidates, SampleIdentifier, MoleculeIdentifier, AdductIdentifier, IsotoplogeIdentifier)
{

  ##################################
  #label isotopologues which have a higher area than than another isotopologue of the same molecule and adduct which should theoretically be the higher one
  ##################################
  DT_tmp <- PeakCandidates[PeakCandidates,
                on = c(SampleIdentifier, MoleculeIdentifier, AdductIdentifier, 'isoab < isoab'),
                nomatch = 0L, mult = "all", allow.cartesian = TRUE][, WrongIsoOrder_tmp :=
                                                                      .(peaks.area/i.peaks.area > 1 |
                                                                          peaks.area/i.peaks.area == Inf |
                                                                          (is.na(i.peaks.area) & !is.na(peaks.area)) |
                                                                          #(Feature==TRUE & i.Feature==FALSE | Feature == TRUE & is.na(Feature) == TRUE) |
                                                                          (peaks.rt_raw < i.peaks.rtmin | peaks.rt_raw > i.peaks.rtmax))]# |
                                                                          #StartTime/i.StartTime < 1 |
                                                                          #EndTime/ i.EndTime > 1)]

  DT_tmp <- DT_tmp[order(DT_tmp$WrongIsoOrder_tmp, decreasing=TRUE),]
  DT_tmp <- DT_tmp[!duplicated(DT_tmp$IDX),]
  DT_tmp <- merge(PeakCandidates, DT_tmp[,.(IDX, WrongIsoOrder_tmp)], by = "IDX", all.x = TRUE)
  DT_tmp$WrongIsoOrder_tmp[is.na(DT_tmp$WrongIsoOrder_tmp)] <- FALSE



  ##################################
  #also label isotopologues which obey the theoretical abundance order if a higher isotopologue of the same molecule and adduct does not
  ##################################
  DT_tmp <- DT_tmp[DT_tmp,
                   on = c(SampleIdentifier, MoleculeIdentifier, AdductIdentifier, 'isoab < isoab'),
                   nomatch = 0L, mult = "all", allow.cartesian = TRUE][, WrongIsoOrder :=
                                                                         .(i.WrongIsoOrder_tmp == TRUE |
                                                                             WrongIsoOrder_tmp == TRUE)]

  DT_tmp <- DT_tmp[order(DT_tmp$WrongIsoOrder, decreasing=TRUE),]
  DT_tmp <- DT_tmp[!duplicated(DT_tmp$IDX),]
  Output <- merge(PeakCandidates, DT_tmp[,.(IDX, WrongIsoOrder)], by = "IDX", all.x = TRUE)
  Output$WrongIsoOrder[is.na(Output$WrongIsoOrder)] <- FALSE



  return(Output)

}
