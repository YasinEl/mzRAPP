#' detect_double_peaks2
#'
#' @param pot.doubleP.v
#' @param Min.PpP
#' @param l
#' @param r
#' @param Min.Res
#'
#' @return
#' @export
#'
#' @examples
detect_double_peaks2 <- function(pot.doubleP.v, Min.PpP = 10, l = 1, r = length(pot.doubleP.v), Min.Res = 70) {

  pot.doubleP.v <- pot.doubleP.v[l:r]

  noise.over.peak.v <- get_avg_noise(pot.doubleP.v)

  susp.noise <- median(noise.over.peak.v) + sd(noise.over.peak.v)

  peak.dt.ini <-
    data.table(
      breakP = as.integer(NA),
      breakH = as.double(NA),
      valley_diff = NA,
      resolved = NA
    )

  peak.dt. <- peak.dt.ini

  peak.dt_list <-
lapply((pot.doubleP.v[pot.doubleP.v > susp.noise &
                               pot.doubleP.v < (Min.Res/100) * max(pot.doubleP.v)] + 1), function(point, peak.dt = peak.dt.) {

    doublePeak.rle <- S4Vectors::Rle(pot.doubleP.v > point)

    doublePeak.rle.dt <-
      data.table(
        idx = as.numeric(seq.int(length(
          doublePeak.rle@values
        ))),
        values = doublePeak.rle@values,
        lengths = doublePeak.rle@lengths,
        start = S4Vectors::start(doublePeak.rle),
        end = S4Vectors::end(doublePeak.rle)
      )

    doublePeak.rle.dt_p <-
      doublePeak.rle.dt[values == TRUE & lengths >= Min.PpP * 0.8]

    if (nrow(doublePeak.rle.dt_p) > 1) {


      for(peak in 1:nrow(doublePeak.rle.dt_p) - 1){

        #valley between two peaks
        doublePeak.rle.dt_v <- doublePeak.rle.dt[idx > doublePeak.rle.dt_p[peak]$idx & idx < doublePeak.rle.dt_p[peak + 1]$idx]


        #if(suppressWarnings( ifelse( max(doublePeak.rle.dt_v[values == FALSE]$lengths) < 1, FALSE, max(doublePeak.rle.dt_v[values == FALSE]$lengths))) >= max(max(doublePeak.rle.dt_p[peak]$lengths, doublePeak.rle.dt_p[peak+1]$lengths) * 0.5, Min.PpP * 0.8)){
        if(suppressWarnings( ifelse(nrow(doublePeak.rle.dt_v[values == FALSE]) == 0,
                                    FALSE,
                                    max(doublePeak.rle.dt_v[values == FALSE]$lengths)
        )
        ) >= max(max(doublePeak.rle.dt_p[peak]$lengths,
                     doublePeak.rle.dt_p[peak+1]$lengths) * 0.3,
                 Min.PpP * 0.5) &
        sum(doublePeak.rle.dt_v[values == TRUE]$lengths) < 0.5 *  max(max(doublePeak.rle.dt_p[peak]$lengths,
                                                                          doublePeak.rle.dt_p[peak+1]$lengths) * 0.7,
                                                                      Min.PpP * 0.8)){


          dp.p_height_h <-
            max(max(pot.doubleP.v[(doublePeak.rle.dt_p[peak]$start):(doublePeak.rle.dt_p[peak]$end)]),
                max(pot.doubleP.v[(doublePeak.rle.dt_p[peak + 1]$start):(doublePeak.rle.dt_p[peak + 1]$end)]))# - baseL

          dp.p_height_l <-
            min(max(pot.doubleP.v[(doublePeak.rle.dt_p[peak]$start):(doublePeak.rle.dt_p[peak]$end)]),
                max(pot.doubleP.v[(doublePeak.rle.dt_p[peak + 1]$start):(doublePeak.rle.dt_p[peak + 1]$end)]))# - baseL

          break.H <-
            as.double(min(pot.doubleP.v[min(doublePeak.rle.dt[idx > doublePeak.rle.dt_p[peak]$idx]$start):max(doublePeak.rle.dt[idx < doublePeak.rle.dt_p[peak+1]$idx]$end)]))# - baseL

          resolved. <-
            break.H / dp.p_height_h * 100

          if(resolved. < Min.Res){

            break.P <-
              which.min(pot.doubleP.v[min(doublePeak.rle.dt[idx > doublePeak.rle.dt_p[peak]$idx]$start):max(doublePeak.rle.dt[idx < doublePeak.rle.dt_p[peak+1]$idx]$end)]) +
              doublePeak.rle.dt_p[peak]$end
            peak.dt <- rbind(peak.dt,
                             data.table(breakP = as.integer(break.P + l - 1),
                                        breakH = as.double(break.H),
                                        valley_diff = dp.p_height_l - break.H,
                                        resolved = resolved.))

            return(peak.dt)
          }
        }
      }
    }
  })
peak.dt <- rbindlist(peak.dt_list)
peak.dt$breakP <- as.integer(peak.dt$breakP)
  if(nrow(peak.dt) == 0) return(NULL) else{

    noise_valleys <- get_avg_noise(pot.doubleP.v)

    if(!is.null(noise_valleys)){
      noise_valleys <- noise_valleys[!noise_valleys %in% na.omit(peak.dt$valley_diff)]
    }

    if(!is.null(noise_valleys)){
      if(length(noise_valleys) > 0){
        valley_tsh <- 3 * median(noise_valleys) #+ ifelse( length(noise_valleys) > 1, sd(noise_valleys), 0 )
        peak.dt <- peak.dt[valley_diff > valley_tsh]
      }

    }


    if(nrow(na.omit(peak.dt)) == 0){return(NULL)}
    return(na.omit(unique(peak.dt, by="breakP")))
  }
}






#' get_avg_noise
#'
#' @param int
#'
#' @return
#' @export
#'
#' @examples
get_avg_noise <- function(int){

  int <- unique(int)

  local_max <- which(diff(sign(diff(int)))==-2)+1

  local_min <- sort(c(which(diff(sign(diff(int)))==2)+1, which(int == 0)))

  if(length(local_min) == 0) return(c())


  global_max <- which.max(int)

  if(length(local_max[local_max < global_max]) > 0) {

    noise_steps_bm <- unlist(
      lapply(local_max[local_max < global_max], function(l.max){
        if(local_min[local_min > l.max][1] - l.max < 4 & int[local_min[local_min > l.max][1]] > 0){
          return(int[l.max] - int[local_min[local_min > l.max][1]])
        }

      }))
  } else noise_steps_bm <- c()


  if(length(local_max[local_max > global_max]) == 0) return(noise_steps_bm)

  noise_steps_am <- unlist(
    lapply(local_max[local_max > global_max], function(l.max){
      if(l.max - rev(local_min[local_min < l.max])[1] < 4 & int[rev(local_min[local_min < l.max])[1]] > 0){
        return(int[l.max] - int[rev(local_min[local_min < l.max])[1]])
      }
    }))

  return(c(noise_steps_bm, noise_steps_am))
}


















Log_op_fkt <- function(a, op, b, int.v) {
  foo <- match.fun(FUN = op)

  rle.v <- Rle(foo(a, b))


  rle.dt <-
    data.table(
      idx = as.numeric(seq.int(length(
        rle.v@values
      ))),
      values = rle.v@values,
      lengths = rle.v@lengths,
      start = start(rle.v),
      end = end(rle.v)
    )

  rle.dt <- rle.dt[rle.dt[, .(means = mean(int.v[start:end]),
                              maxs = max(int.v[start:end])
                              ),
                          by = .(idx)], on =.(idx)]


  return(rle.dt)
}




t <- function(){

t <- mapply(Log_op_fkt,
            b = sort(seq(min(tv),
                         max(tv),
                         0.0005 * max(tv)),
                     decreasing = FALSE),
            MoreArgs = list(a = tv,
                            op = ">",
                            int.v = tv),
            SIMPLIFY = FALSE)

names(t) <- sort(seq(min(tv), max(tv), 0.0005 * max(tv)), decreasing = FALSE) /max(tv) * 100

t <- t[!duplicated(t)]


peak.slices <- lapply(t, function(x) {setDT(x)[values == TRUE & lengths > 5]})
peak.slices <- peak.slices[!duplicated(peak.slices)]

double.peak.slices <- peak.slices[sapply(peak.slices, function(x) dim(x)[1]) >= 2]
double.peak.slices <- double.peak.slices[!duplicated(double.peak.slices)]


tt <- lapply(t, function(i) setDT(i)[, z := sum(x/y), by=.(a,b)])

scenarios <- lapply(scenarios, function(i) setDT(i)[, z := sum(x/y), by=.(a,b)])




tt <- data.table::rbindlist(t, fill = TRUE)


}

