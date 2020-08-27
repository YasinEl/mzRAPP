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

#' @export
#' @return Width of the peak (numeric)
#'
#' @importFrom retistruct line.line.intersection
#'
#' @noRd


GetFWXM <- function(RT_vect, Int_vect, baseL, X, peak_borders = FALSE, return_diff = FALSE)
{



  gw <- baseL + (max(Int_vect) - baseL) * X
  tl <- rle(Int_vect > gw)
  l <- tl[["lengths"]]
  v <- tl[["values"]]

  dt <- data.table(idx = seq(1:length(l)),
                   l = l,
                   v = v)

  if(nrow(dt[v == TRUE]) == 0 || max(dt[v == TRUE]$l) < 4){return(NA_real_)}

  main_peak <- dt[v == TRUE & l == max(dt[v == TRUE]$l)]$idx[1]
  IntSec1 <- NA
  IntSec2 <- NA
  #print(dt)
  if(v[1] == FALSE & v[length(v)] == FALSE & length(v) > 1 | peak_borders == TRUE){


    if(peak_borders == FALSE | peak_borders == TRUE & v[1] == FALSE){

      fs <- 1
      if(length(l) > 4 & peak_borders == TRUE){

        #main_peak <- dt[v == TRUE & l == max(dt[v == TRUE]$l)]$idx[1]


        dt_before <- dt[idx < main_peak]

        if(nrow(dt_before[v == FALSE & l > 2]) > 0){
          fs <- suppressWarnings(max(dt_before[v == FALSE & l > 2]$idx[length(dt_before[v == FALSE & l > 2]$idx)]))
        }

      }

      fs <- sum(l[1:fs])



      P1 <- c(RT_vect[fs], Int_vect[fs])
      P2 <- c(RT_vect[fs + 1], Int_vect[fs + 1])
      P3 <- c(RT_vect[fs], (baseL + (max(Int_vect) - baseL) * X))
      P4 <- c(RT_vect[fs + 1], (baseL + (max(Int_vect) - baseL) * X))


      IntSec1 <- retistruct::line.line.intersection(P1, P2, P3, P4, interior.only = TRUE)
    } else {IntSec1 <- c(min(RT_vect))}

    #print(paste0("IntSec1: ", IntSec1))

    if(peak_borders == FALSE | peak_borders == TRUE & v[length(v)] == FALSE){
      #u <- sum(l) - l[length(l)] + 1


      fs <- length(l)
      if(length(l) > 4 & peak_borders == TRUE){

        #main_peak <- dt[v == TRUE & l == max(dt[v == TRUE]$l)]$idx[1]

        dt_after <- dt[idx > main_peak]

        if(nrow(dt_after[v == FALSE & l > 2]) > 0){
          fs <- suppressWarnings(min(dt_after[v == FALSE & l > 2]$idx))
        }


      }

            u <- sum(l) - sum(l[fs: length(l)]) + 1

      P1 <- c(RT_vect[u], Int_vect[u])
      P2 <- c(RT_vect[u - 1], Int_vect[u - 1])
      P3 <- c(RT_vect[u], (baseL + (max(Int_vect) - baseL) * X))
      P4 <- c(RT_vect[u - 1], (baseL + (max(Int_vect) - baseL) * X))

      IntSec2 <- retistruct::line.line.intersection(P1, P2, P3, P4, interior.only = TRUE)
    } else {IntSec2 <- c(max(RT_vect))}

    #IntSec2[1] - IntSec1[1]

    if(return_diff == FALSE) { return(as.double(c(IntSec1[1], IntSec2[1]))) } else {

      if(is.na(IntSec2[1]) | is.na(IntSec1[1])) return(NA_real_)
      return(as.double(IntSec2[1] - IntSec1[1]))

    }


  } else {NA}

}
