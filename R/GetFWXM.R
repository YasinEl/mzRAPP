#' @title GetFWXM
#'
#' @description Returns the width of a peak at Full Width at X Maximum, with X being the factor by which the maximum of the peak is multiplied.
#'
#' @param RT_vect vector with retention times
#' @param Int_vect vector with intensities
#' @param baseL height of baseline
#' @param X peak height at which width should be measured (e.g. 0.5 means width at halfe maximum)
#' @param return_diff TRUE/FALSE; should the output be a difference of end point and start point, or both points as a vector
#'
#' @return Width of the peak (numeric)
#'
#' @export
#' @importFrom retistruct line.line.intersection
#'


GetFWXM <- function(RT_vect, Int_vect, baseL, X, peak_borders = FALSE, return_diff = FALSE)
{



  tl <- rle(Int_vect > (baseL + (max(Int_vect) - baseL) * X))
  l <- tl[["lengths"]]
  v <- tl[["values"]]
  IntSec1 <- NA
  IntSec2 <- NA


  if(v[1] == FALSE & v[length(v)] == FALSE & length(v) > 1 | peak_borders == TRUE){


    if(peak_borders == FALSE | peak_borders == TRUE & v[1] == FALSE){

    P1 <- c(RT_vect[l[which(!v)][1]], Int_vect[l[which(!v)][1]])
    P2 <- c(RT_vect[l[which(!v)][1] + 1], Int_vect[l[which(!v)][1] + 1])
    P3 <- c(RT_vect[l[which(!v)][1]], (baseL + (max(Int_vect) - baseL) * X))
    P4 <- c(RT_vect[l[which(!v)][1] + 1], (baseL + (max(Int_vect) - baseL) * X))

    IntSec1 <- retistruct::line.line.intersection(P1, P2, P3, P4, interior.only = TRUE)
    } else {IntSec1 <- c(min(RT_vect))}


    if(peak_borders == FALSE | peak_borders == TRUE & v[length(v)] == FALSE){
      u <- sum(l) - l[length(l)] + 1

      P1 <- c(RT_vect[u], Int_vect[u])
      P2 <- c(RT_vect[u - 1], Int_vect[u - 1])
      P3 <- c(RT_vect[u], (baseL + (max(Int_vect) - baseL) * X))
      P4 <- c(RT_vect[u - 1], (baseL + (max(Int_vect) - baseL) * X))

      IntSec2 <- retistruct::line.line.intersection(P1, P2, P3, P4, interior.only = TRUE)
      } else {IntSec2 <- c(max(RT_vect))}

    #IntSec2[1] - IntSec1[1]

    if(return_diff == FALSE) { return(c(IntSec1[1], IntSec2[1])) } else {

      if(is.na(IntSec2[1]) | is.na(IntSec1[1])) return(NA)
      return(IntSec2[1] - IntSec1[1])

      }


  } else {NA}

}
