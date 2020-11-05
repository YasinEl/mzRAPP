#' Get_MZ_list
#'
#' @param l.peaks l.peaks
#' @param raw_data raw_data
#' @param CompCol_xic CompCol_xic
#'
#'
#' @keywords internal
Get_MZ_list <- function(l.peaks, raw_data, CompCol_xic, EIC.dt){


  l.peaks <-
    l.peaks[l.peaks[, .(pnts = length(EIC.dt[!is.na(int_wo_spikes) & rt >= StartTime & rt <= EndTime & int > 0]$int)), by = .(idx)]$pnts > 5]

  if(nrow(l.peaks) < 1){return(NULL)}

  suppressWarnings(
    raw_data_lim <- raw_data %>%
      xcms::filterRt(rt = c(min(l.peaks$StartTime), max(l.peaks$EndTime))) %>%
      xcms::filterMz(mz = c(CompCol_xic$eic_mzmin - 0.0001, CompCol_xic$eic_mzmax + 0.0001))
  )

  l.peaks.mz_list <- list()
  length(l.peaks.mz_list) <- nrow(l.peaks)
  nc <- 1

  while(nc <= nrow(l.peaks)){

    suppressWarnings(
      raw_data_lim1 <- raw_data_lim %>%
        xcms::filterRt(rt = unlist(unname(l.peaks[nc, c("StartTime", "EndTime")]))) #%>%
    )

    suppressWarnings(
      l.peaks.mz_list[[nc]] <- list(mz = xcms::mz(raw_data_lim1),
                                    int = xcms::intensity(raw_data_lim1))

    )

    if(length(l.peaks.mz_list[[nc]][["mz"]]) > 1){
      highstInt_idx <-
        mapply(which.max,
               l.peaks.mz_list[[1]][["int"]],
               SIMPLIFY = FALSE
        )


      l.peaks.mz_list[[nc]][["int"]] <-
        lapply(1:length(highstInt_idx),
               function(x,
                        li = l.peaks.mz_list[[1]][["int"]],
                        idx = highstInt_idx){
                 return(li[[x]][idx[[x]]])
               })
      l.peaks.mz_list[[nc]][["mz"]] <-
        lapply(1:length(highstInt_idx),
               function(x,
                        li = l.peaks.mz_list[[1]][["mz"]],
                        idx = highstInt_idx){
                 return(li[[x]][idx[[x]]])
               })
    }
    nc <- nc + 1
  }
  return(l.peaks.mz_list)
}


