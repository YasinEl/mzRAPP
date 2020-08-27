#' GetZigzagIDX
#'
#' @param int
#' @param height
#'
#' @return
#' @export
#'
#' @noRd
GetZigzagIDX <- function(int, height){

  Zaeler_zzi = 0
  for(n in c(2:(length(int)-1))){
    Zaeler_zzi <- Zaeler_zzi + (2 * int[n] - int[n - 1] - int[n + 1])^2
  }

  zigzag_idx <- Zaeler_zzi / (length(int) * height^2)

  return(zigzag_idx)

}
