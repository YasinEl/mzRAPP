#' get_pot_peak_ranges2
#'
#' @param int
#' @param Min.PpP
#' @param peak.spotting.factor
#'
#' @return
#' @export
#'
#' @examples
get_pot_peak_ranges2 <- function(int, Min.PpP = 10, peak.spotting.factor = 0.01){



  ##################################
  #generate table with more than Min.PpP consecutive points above baseline
  ##################################
  baseL <- min(int) + (max(int) - min(int)) * peak.spotting.factor
  rle.obj <- S4Vectors::Rle(int > baseL)
  rle.dt <- data.table(
    v = rle.obj@values,
    l = rle.obj@lengths,
    s = S4Vectors::start(rle.obj),
    e = S4Vectors::end(rle.obj)
  )
  consec.points <- rle.dt[v==TRUE & l >= Min.PpP]



  ##################################
  #extend potential peak ranges by one scan into each both directs (if they are not already reaching until the first/last scan)
  ##################################
  if(nrow(consec.points) == 0) { return(NULL) }
  consec.points[s>1]$s <- as.integer(consec.points[s>1]$s - 1)
  consec.points[e<length(int)]$e <- as.integer(consec.points[e<length(int)]$e + 1)
  consec.points$idx <- seq(nrow(consec.points))
  consec.points <- consec.points[consec.points[, .(height = max(int[s:e])), by = .(idx)], on = .(idx)]
  consec.points <- consec.points[height > 0.03 * max(consec.points$height)]


  return(consec.points[,!c("v", "l", "height")])
}


