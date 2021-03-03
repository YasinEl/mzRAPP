#' Get_MZ_list
#'
#' @param l.peaks l.peaks
#' @param raw_data raw_data
#' @param CompCol_xic CompCol_xic
#'
#'
#'
#' @importFrom xcms filterRt filterMz
#'
#' @keywords internal
Get_MZ_list <- function(l.peaks, raw_data, CompCol_xic, EIC.dt, max.mz.diff_ppm){


  l.peaks <-
    l.peaks[l.peaks[, .(pnts = length(EIC.dt[!is.na(int_wo_spikes) & rt >= StartTime & rt <= EndTime & int > 0]$int)), by = .(idx)]$pnts >= 5]

  if(nrow(l.peaks) < 1){return(NULL)}

  suppressWarnings(
    raw_data_lim <- raw_data %>%
      xcms::filterRt(rt = c(min(l.peaks$StartTime), max(l.peaks$EndTime))) %>%
      xcms::filterMz(mz = c(CompCol_xic$eic_mzmin - max.mz.diff_ppm * 4 * 1e-6 * CompCol_xic$eic_mzmin, CompCol_xic$eic_mzmax + max.mz.diff_ppm * 4 * 1e-6 * CompCol_xic$eic_mzmin))
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


    #get area of extended mz
    intensity <- lapply(l.peaks.mz_list[[nc]][["int"]], function(x){if(is.null(x)){NULL} else sum(x)})

    mean_int <- mean(unlist(intensity))


    if(length(l.peaks.mz_list[[nc]][["mz"]]) > 1){

      #get original peak

      orig_idx <-
        mapply(function(x, lower = CompCol_xic$eic_mzmin - 0.001, upper = CompCol_xic$eic_mzmax + 0.001){which(data.table::between(x, lower, upper) == TRUE)},
               l.peaks.mz_list[[1]][["mz"]],
               SIMPLIFY = FALSE
        )


      l.peaks.mz_list[[nc]][["int"]] <-
        lapply(1:length(orig_idx),
               function(x,
                        li = l.peaks.mz_list[[1]][["int"]],
                        idx = orig_idx){
                 return(li[[x]][idx[[x]]])
               })


      l.peaks.mz_list[[nc]][["mz"]] <-
        lapply(1:length(orig_idx),
               function(x,
                        li = l.peaks.mz_list[[1]][["mz"]],
                        idx = orig_idx){
                 return(li[[x]][idx[[x]]])
               })


      #take highest mass peak

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


    #get area of original mz
    intensity <- lapply(l.peaks.mz_list[[nc]][["int"]], function(x){if(is.null(x)){NULL} else sum(x)})

    mean_orig_int <- mean(unlist(intensity))

    l.peaks.mz_list[[nc]][["EXTvsORIG"]] <- c(mean_int, mean_orig_int)

    nc <- nc + 1
  }



  return(l.peaks.mz_list)
}


