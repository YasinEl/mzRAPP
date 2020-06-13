#' align_PC
#'
#' @param PC
#' @param ia
#' @param add
#' @param isocount
#' @param plan
#' @param pick_best
#'
#' @return
#' @export
#' @import foreach
#'
#'
#'
#' @examples
align_PC <- function(PC,
                     ia = 100,
                     add = "main_adduct",
                     isocount = 2,
                     plan = "multiprocess",
                     pick_best = "rt_match"){

  PC_start <- copy(PC)

  #Bureau <- as.list(seq_len(nrow(unique(PC, by = c("molecule", "adduct", "isoab")))))
  if(ia == 100){PC <- PC[isoab == 100]}

  if(add == "main_adduct"){PC <- PC[adduct == main_adduct]}

  PC <- PC[Iso_count >= isocount]

  checkl <- unique(PC[, c("molecule", "adduct", "mz_ex")])
  #df <- na.omit(PC_momix_philic_13c[molecule == "Uridine" & adduct == "M-H" & isoab == 100,
  #                                  c("molecule", "adduct", "isoab", "peaks.M0.grp", "FileName", "peaks.StartTime", "peaks.EndTime")])


  #for(mai.c in seq(nrow(checkl))){

  PC <- PC[, c("molecule", "adduct", "isoab", "peaks.M0.grp", "FileName", "peaks.StartTime", "peaks.EndTime", "peaks.rt_raw", "mz_ex")]


  Bureau <- list()

  doFuture::registerDoFuture()
  future::plan(plan)


  Bureau <- foreach(mai.c = seq(nrow(checkl)), .packages = c("mzRAPP"), .inorder = FALSE) %dopar%{

    df <- na.omit(PC[molecule == checkl[mai.c]$molecule & adduct == checkl[mai.c]$adduct & mz_ex == checkl[mai.c]$mz_ex])



    if(nrow(df) > 0){

      df$id <- seq(nrow(df))


      ol_matrix <- matrix(nrow = nrow(df), ncol = nrow(df))

      idf <- intervals::Intervals(df[,c("peaks.StartTime", "peaks.EndTime")])
      as.data.frame(intervals::interval_union(idf))

      idl <- lapply(unique(df$id),function(x){
        var <- as(intervals::Intervals(df[df$id==x,c("peaks.StartTime", "peaks.EndTime")]),"Intervals_full")#;
        intervals::closed(var)[,1]<- FALSE#;
        return(var)
        })

      for(ii in rev(seq(length(idl)))[1:(length(idl)-1)]){
        idt_p <- idl[[ii]]
        n = 1
        for(i in idl[1:(ii-1)]){
          idt <- intervals::interval_intersection(idt_p,i)
          res <- as.data.frame(idt)
          if(nrow(res) > 0){

            ol <- (100 * (res$V2 - res$V1) / min( i[[2]] - i[[1]], idt_p[[2]] - idt_p[[1]]  ))

            if(ol > 5){# & abs(res$V2 - res$V1) >= abs(df[id == n]$peaks.rt_raw - df[id == ii]$peaks.rt_raw) ){

              #ol_matrix[ii, n] <- abs(df[id == n]$peaks.rt_raw - df[id == ii]$peaks.rt_raw)
              ol_matrix[ii, n] <- TRUE#if(df[id == n]$peaks.rt_raw > df[id == ii]$peaks.StartTime & df[id == n]$peaks.rt_raw < df[id == ii]$peaks.EndTime) {TRUE} else {NA}
            }

          }
          n = n + 1
        }
      }





      df[, aligned.grp := as.integer( rep(NA, nrow(df)))]
      df[, aligned.count := rep(0, nrow(df))]
      df[, aligned.flag := rep(FALSE, nrow(df))]

      align.grps = 1



      for(i in seq(nrow(ol_matrix))){

        pot.align.grp <- c(which(!is.na(ol_matrix[,i])), which(!is.na(ol_matrix[i,])), i)

        #if(!length(unique(df[pot.align.grp]$aligned.grp)) == 1){ df[pot.align.grp]$aligned.flag <- TRUE }

        if(any(is.na(unique(df[pot.align.grp]$aligned.grp))) | !length(unique(na.omit(df[pot.align.grp]$aligned.grp))) == 1){


          #what to do if any filenames duplicated??

          if(length(unique(na.omit(df[pot.align.grp]$aligned.grp))) > 0){

            old.aligned.grp <- unique(na.omit(df[pot.align.grp]$aligned.grp))
            df[aligned.grp %in% old.aligned.grp]$aligned.grp <- as.numeric(align.grps)
            df[pot.align.grp]$aligned.flag <- TRUE
            df[aligned.grp %in% old.aligned.grp]$aligned.flag <- TRUE

          }

          df[pot.align.grp]$aligned.grp <- as.numeric(align.grps)
          df[aligned.grp == align.grps]$aligned.count <- length(pot.align.grp)

          align.grps = align.grps + 1

        }
      }
    }


    return(df)


  }

  future::plan("sequential")

  outputT <- data.table::rbindlist(Bureau, fill = TRUE)
  outputT <- outputT[, c("molecule", "peaks.M0.grp", "FileName", "aligned.grp", "aligned.count", "aligned.flag")][PC_start,
                                                                                                                     on = .(molecule, peaks.M0.grp, FileName)]
  #rm aligned.grps with dupl isos in one sample
  dplT <- outputT[, .(dpl = anyDuplicated(mz_ex), aligned.grp = aligned.grp), by = .(molecule, adduct, FileName)]
  dplT <- unique(dplT[dpl > 0], by = c("molecule", "adduct", "aligned.grp"))

  setkeyv(dplT, c("molecule", "adduct", "aligned.grp"))
  setkeyv(outputT, c("molecule", "adduct", "aligned.grp"))

  outputT <- outputT[!dplT]
  outputT <- outputT[!is.na(aligned.grp)]

#print(outputT[molecule == "m1" & isoab == 100, c("FileName", "molecule", "peaks.StartTime", "peaks.EndTime", "aligned.grp")])
  #decide for best aligned.grp
  if(pick_best == "rt_match"){
    tmp <- outputT[isoab == 100, .(rt.diff = abs(mean(peaks.rt_raw) - mean(user.rt)), peaks.manual_int = any(peaks.manual_int)), by = .(molecule, aligned.grp) ]
    tmp <- tmp[, .(aligned.grp = aligned.grp, rt.diff = rt.diff, peaks.manual_int = peaks.manual_int, MINrt.diff = min(rt.diff)), by = .(molecule) ]
    tmp <- tmp[rt.diff == MINrt.diff | peaks.manual_int == TRUE]
    outputT <- outputT[tmp, on = .(molecule, aligned.grp), nomatch = NULL, allow.cartesian = TRUE][, !c("MINrt.diff", "rt.diff", "i.peaks.manual_int")]
  } else if(pick_best == "highest_mean_area"){
    tmp <- outputT[isoab == 100, .(mean_area = mean(peaks.area), peaks.manual_int = any(peaks.manual_int)), by = .(molecule, aligned.grp) ]
    tmp <- tmp[, .(aligned.grp = aligned.grp, mean_area = mean_area, peaks.manual_int = peaks.manual_int, Max_mean_area = max(mean_area)), by = .(molecule) ]
    tmp <- tmp[mean_area == Max_mean_area | peaks.manual_int == TRUE]
    outputT <- outputT[tmp, on = .(molecule, aligned.grp), nomatch = NULL, allow.cartesian = TRUE][, !"i.peaks.manual_int"]
  }

  #filter out isotopologues which do not have at least in one file a prediction error < 30
  Isoab_summary_table <- outputT[isoab < 100,.(lowest_IsoabError = min(abs(ErrorRel_A))), by = .(molecule, adduct, mz_ex)]
  Isoab_summary_table <- Isoab_summary_table[lowest_IsoabError >= 30]
  if(nrow(Isoab_summary_table) > 1){
    outputT <- Isoab_summary_table[outputT, on = .(molecule, adduct, mz_ex), nomatch = NA]
    outputT_f <- outputT[is.na(lowest_IsoabError), !"lowest_IsoabError"]
    isoCountT <- outputT[,.(Iso_count = .N), by = .(molecule, adduct, FileName)]
    outputT <- isoCountT[outputT[,!"Iso_count"], on = .(molecule, adduct, FileName), nomatch = NA]
    outputT <- outputT[Iso_count > 1]
  }

  #tmp <- outputT[isoab == 100, .(peaks.StartTime_range = max(peaks.StartTime) - min(peaks.StartTime),
  #                                peaks.EndTime_range = max(peaks.EndTime) - min(peaks.EndTime)),
  #               by = .(molecule, adduct)]

  #tmp$start_end_med <- abs(peaks.StartTime_range - peaks.EndTime_range)
  #tmp$start_end_diff <- abs(peaks.StartTime_range - peaks.EndTime_range)



  outputT <- outputT[, !c("Iso_count", "lowest_IsoabError", "peaks.M0.grp", "aligned.grp", "aligned.count", "aligned.flag", "peaks.idx", "peaks.manual_int", "isoab_ol")]



  return(outputT)
}
