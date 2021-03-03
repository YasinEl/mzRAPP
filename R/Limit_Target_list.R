#' Limit_Target_list
#'
#' @param CompCol CompCol
#' @param CondPeaks CondPeaks
#' @param iso.run iso.run
#' @param file file
#' @param adduct.run adduct.run
#' @param Min.PointsperPeak PpP
#'
#' 
#' @keywords internal
#'
Limit_Target_list <- function(CompCol, CondPeaks, iso.run, adduct.run, file, Min.PointsperPeak){

  if (iso.run == "MAiso") {
    if (adduct.run == "main_adduct") {
      CompCol_xic <- CompCol[isoab == 100 &
                                FileName == sub(pattern = "(.*)\\..*$",
                                                replacement = "\\1",
                                                basename(file)) &
                                adduct == main_adduct]
    } else if (adduct.run == "screen_adducts") {
        CompCol_xic <- CompCol[isoab == 100 &
                                  FileName == sub(pattern = "(.*)\\..*$",
                                                  replacement = "\\1",
                                                  basename(file)) &
                                  adduct != main_adduct]

        CompCol_xic <-
          stats::na.omit(CompCol_xic[unique(CondPeaks[peaks.PpP > Min.PointsperPeak &
                                                !is.na(peaks.PpP), c("molecule", "FileName")]),
                               on = .(molecule, FileName),
                               allow.cartesian = TRUE], cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))
    }

  } else if (iso.run == "LAisos") {
      if (adduct.run == "main_adduct") {
        CompCol_xic <- stats::na.omit(CompCol[isoab < 100 &
                                          FileName == sub(pattern = "(.*)\\..*$",
                                                          replacement = "\\1",
                                                          basename(file)) &
                                          adduct == main_adduct], cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))
      } else {
        CompCol_xic <- CompCol[isoab < 100 &
                                  FileName == sub(pattern = "(.*)\\..*$",
                                                  replacement = "\\1",
                                                  basename(file)) &
                                  adduct != main_adduct]
      }

      CompCol_xic <-
        stats::na.omit(CompCol_xic[unique(CondPeaks[peaks.PpP > Min.PointsperPeak &
                                              !is.na(peaks.PpP), c("molecule", "adduct", "FileName")]),
                             on = .(molecule, adduct, FileName),
                             allow.cartesian = TRUE], cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))
      }

  return(CompCol_xic)

}
