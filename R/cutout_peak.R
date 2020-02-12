#' cutout_peaks
#'
#' @description detects peaks
#'
#' @param int
#' @param rt
#' @param Min.PpP
#' @param peak.spotting.factor.
#' @param Integration_baseL_factor.
#' @param l
#' @param r
#' @param M0.grp
#' @param main_adduct.grp
#' @param Min.Res.
#'
#' @keyword Internal
#' @export
#' @return
#'
cutout_peaks <-
  function(int,
           rt,
           Min.PpP = 10,
           peak.spotting.factor. = 0.01,
           Integration_baseL_factor. = 0.1,
           l = 1,
           r = length(int),
           M0.grp = NA,
           main_adduct.grp = NA,
           Min.Res. = 70) {


    ##################################
    #limit peak-detection to specific rt-region
    ##################################
    #if(is.na(l) | is.null(l)) l = 1
    #if(is.na(r) | is.null(l)) l = length(int)

    vl <- length(int)
    int <- int[l:r]
    rt <- rt[l:r]
    if (sum(int) == 0) {
      return(NULL)
    }


    ##################################
    #pickup potential peaks by counting consectuive points above the base line
    ##################################
    pot.peak.ranges <- mzRAPP::get_pot_peak_ranges2(int,
                                                      Min.PpP = Min.PpP,
                                                      peak.spotting.factor = peak.spotting.factor.)
    if (is.null(pot.peak.ranges)) {
      return(NULL)
    }
    l.peaks <-
      pot.peak.ranges[, c("idx", "s", "e")][, c("unres.s", "unres.e", "peak.grp") := .(rep(FALSE, nrow(pot.peak.ranges)),
                                                                                       rep(FALSE, nrow(pot.peak.ranges)),
                                                                                       idx)][]


    ##################################
    #check each potential peak on whether multiple peaks can be resolved from it
    ##################################
    double.peak.borders <- mapply(
      detect_double_peaks2,
      l = pot.peak.ranges$s,
      r = pot.peak.ranges$e,
      MoreArgs = list(
        pot.doubleP.v = as.numeric(int),
        Min.PpP = Min.PpP,
        Min.Res = Min.Res.
      ),
      SIMPLIFY = FALSE
    )
    double.peak.borders <-
      data.table::rbindlist(double.peak.borders, use.names = TRUE)
    if (nrow(double.peak.borders) > 0) {
      ##################################
      #insert double peak borders into potential peak ranges
      ##################################
      l.peaks <- double.peak.borders[pot.peak.ranges,
                                     assemble_peaks(idx, s, e, x.breakP),
                                     on = .(breakP > s, breakP < e),
                                     by = .EACHI][,-(1:2)][, c("unres.s", "unres.e", "idx") := .(!is.na(match(s, double.peak.borders$breakP)),!is.na(match(e, double.peak.borders$breakP)),
                                                                                                 seq(1:length(s)))][]
    }


    ##################################
    #add different variables per peak
    ##################################
    l.peaks$idx <- seq_len(nrow(l.peaks))
    l.peaks <-
      l.peaks[l.peaks[, .(
        res.s = as.double(ifelse(unres.s == TRUE, 100 * int[s] / max(int[s:e]), NA)),
        res.e = as.double(ifelse(unres.e == TRUE, 100 * int[e] / max(int[s:e]), NA)),
        rt = rt[s + which.max(int[s:e]) - 1],
        rt.w = weighted.mean(rt[s:e], int[s:e]),
        rtmin = rt[s],
        rtmax = rt[e],
        baseL = min(int[s:e]) + (max(int[s:e]) - min(int[s:e])) * Integration_baseL_factor.
      ), by = .(idx)],
      on = .(idx)]
    l.peaks$s <- l + l.peaks$s - 1
    l.peaks$e <- l + l.peaks$e - 1


    ##################################
    #add indicators for lower abundant isotopologues and screened adducts
    ##################################
    if (!is.na(M0.grp)) {
      suppressWarnings(l.peaks[, M0.grp := rep(M0.grp, nrow(l.peaks))][])
    } else
      l.peaks[, M0.grp := idx][]
    if (!is.na(main_adduct.grp)) {
      suppressWarnings(l.peaks[, main_adduct.grp := rep(main_adduct.grp, nrow(l.peaks))][])
    } else
      l.peaks[, main_adduct.grp := idx][]

    ifelse(nrow(l.peaks) > 0, return(l.peaks),return(NULL))
  }






