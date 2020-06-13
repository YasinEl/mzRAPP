#' Get_peak_vars
#'
#' @param l.peaks
#' @param EIC.dt
#' @param CompCol_xic
#' @param iso.run
#' @param adduct.run
#' @param manual_bound
#' @param l.peaks.mz_list
#'
#' @return
#' @export
#'
#' @examples
Get_peak_vars <- function(l.peaks, EIC.dt, CompCol_xic, l.peaks.mz_list, iso.run, adduct.run, manual_bound){

  suppressWarnings(
    l.peaks <- l.peaks[l.peaks[, .(
      PpP = sum(EIC.dt[!is.na(int_wo_spikes) &
                         rt >= StartTime &
                         rt <= EndTime]$int > 0),

      mz_accurate = weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])),

      mz_accuracy_abs = abs(weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])) - CompCol_xic$mz_ex),

      mz_accuracy_ppm = 1e6*abs(weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])) - CompCol_xic$mz_ex) / CompCol_xic$mz_ex,

      mz_span_abs = max(unlist(l.peaks.mz_list[[idx]][["mz"]])) - min(unlist(l.peaks.mz_list[[idx]][["mz"]])),

      mz_span_ppm = 1e6*(max(unlist(l.peaks.mz_list[[idx]][["mz"]])) - min(unlist(l.peaks.mz_list[[idx]][["mz"]]))) / mean(unlist(l.peaks.mz_list[[idx]][["mz"]])),

      mz_min = min(unlist(l.peaks.mz_list[[idx]][["mz"]]), na.rm = TRUE),

      mz_max = max(unlist(l.peaks.mz_list[[idx]][["mz"]]), na.rm = TRUE),

      #mz_interference = {

      # #mz_min_t <- CompCol_xic$mz_acc - (CompCol_xic$mz_acc - CompCol_xic$eic_mzmin) * 3
      #  #mz_max_t <- CompCol_xic$mz_acc + abs(CompCol_xic$mz_acc - CompCol_xic$eic_mzmax) * 3

      # rdl_extended1 <- rdl_extended %>%
      #    filterRt(rt = c(StartTime, EndTime)) #%>%
      #filterMz(mz = c(mz_min_t, mz_max_t))


      # suppressWarnings(
      #    raw_data_lim1 <- raw_data_lim %>%
      ##      filterRt(rt = c(StartTime, EndTime)) #%>%
      #filterMz(mz = c(CompCol_xic$eic_mzmin - 0.0001, CompCol_xic$eic_mzmax + 0.0001))
      #    )



      #highest_mp <- sum(unlist(lapply(intensity(raw_data_lim), max)))
      #summed_mp <- sum(unlist(lapply(intensity(rdl_extended), sum)))

      #summed_mp > 2 * highest_mp


      #  suppressWarnings(
      #    EIC.spec_targets <- lapply(seq(length(raw_data_lim1)), function(x,
      #                                                                   mz_lim = mz(raw_data_lim1),
      #                                                                   int_lim = intensity(raw_data_lim1),
      #                                                                   mz_lim_ext = mz(rdl_extended1),
      #                                                                   int_lim_ext = intensity(rdl_extended1)){

      #      if(length(int_lim[[x]]) == 0){return(c(mz = 0, int = 0, mz_if = 0, int_if = 0))}

      #        wmi <- which.max(int_lim[[x]])
      #        mz_val <- mz_lim[[x]][wmi]
      #        int_val <- max(int_lim[[x]])#

      #      if(int_val >= max(int_lim_ext[[x]])){return(c(mz = mz_val,int = int_val, mz_if = 0, int_if = 0))}

      #     wmi_ext <- which.max(int_lim_ext[[x]])
      #      mz_val_ext <- mz_lim_ext[[x]][wmi_ext]
      #      int_val_ext <- max(int_lim_ext[[x]])

      #      return(c(mz = mz_val, int = int_val, mz_if = mz_val_ext, int_if = int_val_ext))

      #    })
      #  )

      #    interference_table <- as.data.table(do.call(rbind, EIC.spec_targets))

      #   if(sum(interference_table$int) * 2 < sum(interference_table$int_if)){
      #      interference_table$delta_mz <- abs(interference_table$mz - interference_table$mz_if)
      #      as.double(min(interference_table$delta_mz, na.rm = TRUE))
      #    } else as.double(NA)

      # },

      FW25M = as.double(
        GetFWXM(
          EIC.dt[rt >= StartTime &
                   rt <= EndTime &
                   !is.na(int_wo_spikes)]$rt,
          EIC.dt[rt >= StartTime &
                   rt <= EndTime &
                   !is.na(int_wo_spikes)]$int,
          0,
          0.25,
          return_diff = TRUE
        )
      ),

      FW50M = as.double(
        GetFWXM(
          EIC.dt[rt >= StartTime &
                   rt <= EndTime &
                   !is.na(int_wo_spikes)]$rt,
          EIC.dt[rt >= StartTime &
                   rt <= EndTime &
                   !is.na(int_wo_spikes)]$int,
          0,
          0.50,
          return_diff = TRUE
        )
      ),

      FW75M = as.double(
        GetFWXM(
          EIC.dt[rt >= StartTime &
                   rt <= EndTime &
                   !is.na(int_wo_spikes)]$rt,
          EIC.dt[rt >= StartTime &
                   rt <= EndTime &
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

      zigZag_IDX = as.double(GetZigzagIDX(
        EIC.dt[rt >= StartTime &
                 rt <= EndTime]$int,
        max(EIC.dt[rt >= StartTime &
                     rt <= EndTime]$int)
      )),

      sharpness = as.double(GetSharpness(EIC.dt[rt >= StartTime &
                                                  rt <= EndTime]$int)),

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

      cor_w_M0 = ifelse(iso.run == "LAisos", suppressWarnings(
        cor(
          EIC.dt[rt >= StartTime &
                   rt <= EndTime]$int,
          EIC.dt[rt >= StartTime &
                   rt <= EndTime]$M0_int,
          method = "pearson",
          use = "complete.obs"
        )
      ), NA),

      cor_w_main_add = ifelse(iso.run == "MAiso" & adduct.run == "screen_adducts", suppressWarnings(
        cor(
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
