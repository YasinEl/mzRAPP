#' Get_peak_vars
#'
#' @param l.peaks l.peaks
#' @param EIC.dt EIC.dt
#' @param CompCol_xic CompCol_xic
#' @param iso.run iso.run
#' @param adduct.run adduct.run
#' @param manual_bound manual_bound
#' @param l.peaks.mz_list l.peaks.mz_list
#'
#'
#'
#' @keywords internal
Get_peak_vars <- function(l.peaks, EIC.dt, CompCol_xic, l.peaks.mz_list, iso.run, adduct.run, manual_bound){

  li_te <- list(l.peaks, EIC.dt, CompCol_xic, l.peaks.mz_list, iso.run, adduct.run, manual_bound)

  if(length(l.peaks) == 1){return(NULL)}

  l.peaks <-
  l.peaks[l.peaks[, .(pnts = length(EIC.dt[!is.na(int_wo_spikes) & rt >= StartTime & rt <= EndTime & int > 0]$int)), by = .(idx)]$pnts > 5]

  if(nrow(l.peaks) < 1){return(NULL)}

  l.peaks[, idx := seq(1:nrow(l.peaks))]


  suppressWarnings(
    l.peaks <- l.peaks[l.peaks[, .(
      PpP = sum(EIC.dt[!is.na(int_wo_spikes) &
                         rt >= StartTime &
                         rt <= EndTime]$int > 0.1 * max(EIC.dt[!is.na(int_wo_spikes) &
                                                                 rt >= StartTime &
                                                                 rt <= EndTime]$int)),

      mz_accurate = stats::weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])),

      mz_accuracy_abs = abs(stats::weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])) - CompCol_xic$mz_ex),

      mz_accuracy_ppm = 1e6*abs(stats::weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])) - CompCol_xic$mz_ex) / CompCol_xic$mz_ex,

      mz_span_abs = max(unlist(l.peaks.mz_list[[idx]][["mz"]])) - min(unlist(l.peaks.mz_list[[idx]][["mz"]])),

      mz_span_ppm = 1e6*(max(unlist(l.peaks.mz_list[[idx]][["mz"]])) - min(unlist(l.peaks.mz_list[[idx]][["mz"]]))) / mean(unlist(l.peaks.mz_list[[idx]][["mz"]])),

      mz_min = min(unlist(l.peaks.mz_list[[idx]][["mz"]]), na.rm = TRUE),

      mz_max = max(unlist(l.peaks.mz_list[[idx]][["mz"]]), na.rm = TRUE),

      FW25M = as.double(
        GetFWXM(
          EIC.dt[rt >= StartTime - 2 &
                   rt <= EndTime + 2 &
                   !is.na(int_wo_spikes)]$rt,
          EIC.dt[rt >= StartTime - 2 &
                   rt <= EndTime + 2 &
                   !is.na(int_wo_spikes)]$int,
          0,
          0.25,
          return_diff = TRUE
        )
      ),

      FW50M = as.double(
        GetFWXM(
          EIC.dt[rt >= StartTime - 2 &
                   rt <= EndTime + 2 &
                   !is.na(int_wo_spikes)]$rt,
          EIC.dt[rt >= StartTime - 2 &
                   rt <= EndTime + 2 &
                   !is.na(int_wo_spikes)]$int,
          0,
          0.50,
          return_diff = TRUE
        )
      ),

      FW75M = as.double(
        GetFWXM(
          EIC.dt[rt >= StartTime - 2 &
                   rt <= EndTime + 2 &
                   !is.na(int_wo_spikes)]$rt,
          EIC.dt[rt >= StartTime - 2 &
                   rt <= EndTime + 2 &
                   !is.na(int_wo_spikes)]$int,
          0,
          0.75,
          return_diff = TRUE
        )
      ),

      data_rate = mean(diff(EIC.dt[rt >= StartTime &
                                     rt <= EndTime]$rt)),

      rt_raw = EIC.dt[rt >= StartTime &
                        rt <= EndTime &
                        int == max(EIC.dt[rt >= StartTime &
                                            rt <= EndTime]$int)]$rt,

      rt_weig = stats::weighted.mean(EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             int > stats::median(EIC.dt[rt >= StartTime &
                                                                          rt <= EndTime]$int)]$rt,
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             int > stats::median(EIC.dt[rt >= StartTime &
                                                                          rt <= EndTime]$int)]$int),



      #zigZag_IDX = as.double(GetZigzagIDX(
      #  EIC.dt[rt >= StartTime &
      #           rt <= EndTime]$int,
      #  max(EIC.dt[rt >= StartTime &
      #               rt <= EndTime]$int)
      #)),

      zigZag_IDX = {

        pd <- c(rtmin = StartTime, rtmax = EndTime)

        pts <- as.matrix(EIC.dt[, c("rt", "int")])

       as.double(MetaClean::calculateZigZagIndex(peakData = pd, pts = pts))
      },

      #GaussianSimilarity = {

      #  pd <- c(rtmin = StartTime, rtmax = EndTime)

      #  pts <- as.matrix(EIC.dt[, c("rt", "int")])

      #  as.double(MetaClean::calculateGaussianSimilarity(peakData = pd, pts = pts))


      #},

      Jaggedness = {

        pd <- c(rtmin = StartTime, rtmax = EndTime)

        pts <- as.matrix(EIC.dt[, c("rt", "int")])

        as.double(MetaClean::calculateJaggedness(peakData = pd, pts = pts))


      },

      Modality = {

        pd <- c(rtmin = StartTime, rtmax = EndTime)

        pts <- as.matrix(EIC.dt[, c("rt", "int")])

        as.double(MetaClean::calculateModality(peakData = pd, pts = pts, flatness.factor = 0.05))


      },

      Sharpness = {

        pd <- c(rtmin = StartTime, rtmax = EndTime)

        pts <- as.matrix(EIC.dt[, c("rt", "int")])

        as.double(MetaClean::calculateSharpness(peakData = pd, pts = pts))


      },

      Symmetry = {

        pd <- c(rtmin = StartTime, rtmax = EndTime)

        pts <- as.matrix(EIC.dt[, c("rt", "int")])

        as.double(MetaClean::calculateSymmetry(peakData = pd, pts = pts))


      },

      TPASR = {

        pd <- c(rtmin = StartTime, rtmax = EndTime)

        pts <- as.matrix(EIC.dt[, c("rt", "int")])

        as.double(MetaClean::calculateTPASR(peakData = pd, pts = pts))


      },


      MinDivMax = {

        min(EIC.dt[rt >= StartTime &
                     rt <= EndTime &
                     !is.na(int_wo_spikes) & int > 0]$int)/max(EIC.dt[rt >= StartTime &
                                                              rt <= EndTime &
                                                              !is.na(int_wo_spikes)]$int)


      },



      #sharpness = as.double(GetSharpness(EIC.dt[rt >= StartTime &
      #                                            rt <= EndTime]$int)),


      #symmetry = {
#
#        sig <- EIC.dt[rt >= StartTime &
#                        rt <= EndTime]$int
#
#        left <- sig[1:floor(length(sig)/2)]
#
#        right <- sig[length(sig):ceiling(length(sig)/2) + 1]
#
#        if(abs(length(left) - length(right)) > max(c(length(left), length(right))) / 4) NA
#
#        left <- left[1:min(length(left), length(right))]
#
#        right <- right[1:min(length(left), length(right))]
#
#        if(abs(length(left) - length(right)) > max(c(length(left), length(right))) / 4 | length(left) < 2) NA
#
#        #right <- sig[seq(length(sig),length(sig) + 1 - floor(length(sig)/2),by = -1)]
#
#        r.symmetry <- suppressWarnings(cor(left,right,method = "pearson",
#                                           use = "complete.obs"))
#
#        #r.symmetry[is.na(r.symmetry)] <- 1
#
#        peak.symmetry <- round(mean(r.symmetry),digits = 4)
#
#        peak.symmetry
#
#      },
#
      height = max(EIC.dt[rt >= StartTime &
                            rt <= EndTime]$int),

      area = DescTools::AUC(
        c(StartTime,
          EIC.dt[rt > StartTime &
                   rt < EndTime]$rt,
          EndTime),
        c(0,
          EIC.dt[rt > StartTime &
                   rt < EndTime]$int,
          0),
        method = "trapezoid"
      ),

      rt_neighbors = {

        if((length(EIC.dt[(rt > (StartTime - (EndTime - StartTime)) &
                          rt < StartTime) &
                          int > 0]$int) > 3)){

          n_area_left <- DescTools::AUC(
            EIC.dt[(rt > (StartTime - (EndTime - StartTime)) &
                      rt < StartTime)]$rt,
            EIC.dt[(rt > (StartTime - (EndTime - StartTime)) &
                      rt < StartTime)]$int,
            method = "trapezoid"
          )

          n_height_left <- max(EIC.dt[(rt > (StartTime - (EndTime - StartTime)) &
                                       rt < StartTime)]$int)

        } else {

          n_area_left <- 0
          n_height_left <- 0

        }

        if((length(EIC.dt[(rt > EndTime &
                           rt < EndTime + (EndTime - StartTime)) &
                          int > 0]$int) > 3)){

          n_area_right <- DescTools::AUC(
            EIC.dt[(rt > EndTime &
                      rt < EndTime + (EndTime - StartTime))]$rt,
            EIC.dt[(rt > EndTime &
                      rt < EndTime + (EndTime - StartTime))]$int,
            method = "trapezoid"
          )

          n_height_right <- max(EIC.dt[(rt > EndTime &
                                          rt < EndTime + (EndTime - StartTime))]$int)

        } else {

          n_area_right <- 0
          n_height_right <- 0

        }

        peak_height <- max(EIC.dt[rt >= StartTime &
                                    rt <= EndTime]$int)

        peak_area <- DescTools::AUC(
          c(StartTime,
            EIC.dt[rt > StartTime &
                     rt < EndTime]$rt,
            EndTime),
          c(0,
            EIC.dt[rt > StartTime &
                     rt < EndTime]$int,
            0),
          method = "trapezoid"
        )

        neighbor_left <- FALSE
        neighbor_right <- FALSE

        #neighbor_left <- if(n_height_left > 0.2 * peak_height & n_area_left > 0.2 * peak_area) {TRUE} else {FALSE}
        #neighbor_right <- if(n_height_right > 0.2 * peak_height & n_area_right > 0.2 * peak_area) {TRUE} else {FALSE}

        neighbor_left <- if(n_area_left > 0.2 * peak_area) {TRUE} else {FALSE}
        neighbor_right <- if(n_area_right > 0.2 * peak_area) {TRUE} else {FALSE}


        if(neighbor_left == TRUE & neighbor_right == FALSE) {
          ret <- "left"
        }
        if(neighbor_left == FALSE & neighbor_right == TRUE) {
          ret <- "right"
        }
        if(neighbor_left == TRUE & neighbor_right == TRUE) {
          ret <- "both sides"
        }
        if(neighbor_left == FALSE & neighbor_right == FALSE) {
          ret <- "none"
        }

        if(is.null(ret)){character()} else {ret}
      },

      mz_neighbors = round(l.peaks.mz_list[[idx]][["EXTvsORIG"]][1] / l.peaks.mz_list[[idx]][["EXTvsORIG"]][2],1),

      cor_w_M0 = ifelse(iso.run == "LAisos", suppressWarnings(
        stats::cor(
          EIC.dt[rt >= StartTime &
                   rt <= EndTime]$int,
          EIC.dt[rt >= StartTime &
                   rt <= EndTime]$M0_int,
          method = "pearson",
          use = "complete.obs"
        )
      ), NA),

      cor_w_main_add = ifelse(iso.run == "MAiso" & adduct.run == "screen_adducts", suppressWarnings(
        stats::cor(
          EIC.dt[rt >= StartTime &
                   rt <= EndTime]$int,
          EIC.dt[rt >= StartTime &
                   rt <= EndTime]$M0_int,
          method = "pearson",
          use = "complete.obs"
        )
      ), NA),

      manual_int = manual_bound


    ), by = .(idx)], on = .(idx)]

  )
  return(l.peaks)

}
