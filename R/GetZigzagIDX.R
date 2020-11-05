#' GetZigzagIDX
#'
#'
#'As described in:
#' Zhang,W. and Zhao,P.X. (2014) Quality evaluation of extracted ion chromatograms and chromatographic peaks in
#' liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics, 15, 1–13.
#'
#'
#' @param int int
#' @param height height
#'
#'
#' @keywords internal
GetZigzagIDX <- function(int, height){

  Zaeler_zzi = 0
  for(n in c(2:(length(int)-1))){
    Zaeler_zzi <- Zaeler_zzi + (2 * int[n] - int[n - 1] - int[n + 1])^2
  }

  zigzag_idx <- Zaeler_zzi / (length(int) * height^2)

  return(zigzag_idx)

}
