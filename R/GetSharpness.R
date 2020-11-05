#' GetSharpness
#'
#' As described in:
#' Zhang,W. and Zhao,P.X. (2014) Quality evaluation of extracted ion chromatograms and chromatographic peaks in
#' liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics, 15, 1–13.
#'
#' @param int int
#'
#'
#' @keywords internal
GetSharpness <- function(int){


  int <- int[int>0]
  befAp = 0
  aftAp = 0
  ApPos <- which.max(int)
  for(n in c(2:(length(int)-1))){
    if(n <= ApPos){
      befAp <- befAp + (int[n] - int[n - 1]) / int[n - 1]
    }

    if(n >= ApPos){
      aftAp <- aftAp + (int[n] - int[n + 1]) / int[n + 1]
    }
  }

  sharpness <- aftAp + befAp

  return(sharpness)

}
