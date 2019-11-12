#' @title Filter_Background
#'
#' @description
#' Takes a PeakCandidates table and lables peaks in non-blank-samples if they do not outreach peaks found in blanks by a certain factor (in area as well as in height).
#'
#' @param PeakCandidates x
#' @param IonSpeciesIdentifier Should uniquly identify the ion species (one column name or a vector of column names)
#' @param SampleIdentifier column name or vector of column names which uniquely identify measured samples
#' @param Blank_label string by which blanks are labeled within the SampleIdentifier
#' @param Factor factor by which sample peaks have to outreach blank peaks (mean of blank peaks over all replicates is used if multiple replicates are available)
#'
#' @import data.table
#' @return PeakCandidates table with additional logical column: Background (TRUE/FALSE)
#' @export
#'
#' @examples
#'
#'


Filter_Background <- function(PeakCandidates, IonSpeciesIdentifier, SampleIdentifier, Blank_label, Factor = 5)
{
  PeakCandidates_tmp <- PeakCandidates[PeakCandidates[[SampleIdentifier]] != Blank_label][PeakCandidates[PeakCandidates[[SampleIdentifier]] == Blank_label][,.(Blank_Area_Mean = mean(peaks.area),
                                                                                                                                                                   Blank_Height_Mean = mean(peaks.height)),
                                                                                                                                                                keyby = c(IonSpeciesIdentifier, SampleIdentifier)],
                                                                                            on = IonSpeciesIdentifier, mult="all"][,Background := .(peaks.area < Factor * Blank_Area_Mean |
                                                                                                                                                      peaks.height < Factor * Blank_Height_Mean)]

  Output <- merge(PeakCandidates, PeakCandidates_tmp[,.(IDX, Background)], by = 'IDX', all.x = TRUE, allow.cartesian=TRUE)
  Output$Background[is.na(Output$Background)] <- FALSE

  return(Output)
}
