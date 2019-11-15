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
#'
#'
#'
#' @examples
align_PC <- function(PC,
                     ia = 100,
                     add = "all",
                     isocount = 2,
                     plan = "multiprocess",
                     pick_best = "rt_match"){

  PC_start <- PC

  #Bureau <- as.list(seq_len(nrow(unique(PC, by = c("molecule", "adduct", "isoabb")))))
  if(ia == 100){PC <- PC[isoabb == 100]}

  if(add == "main_adduct"){PC <- PC[adduct == main_adduct]}

  PC <- PC[Iso_count >= isocount]

  checkl <- unique(PC[, c("molecule", "adduct", "isoabb")])
  #df <- na.omit(PC_momix_philic_13c[molecule == "Uridine" & adduct == "M-H" & isoabb == 100,
  #                                  c("molecule", "adduct", "isoabb", "peaks.M0.grp", "FileName", "peaks.StartTime", "peaks.EndTime")])


  #for(mai.c in seq(nrow(checkl))){

  PC <- PC[, c("molecule", "adduct", "isoabb", "peaks.M0.grp", "FileName", "peaks.StartTime", "peaks.EndTime", "peaks.rt")]


  Bureau <- list()

  doFuture::registerDoFuture()
  future::plan(plan)


  Bureau <- foreach(mai.c = seq(nrow(checkl)), .packages = c("lazypeaks"), .inorder = FALSE) %dopar%{

    df <- na.omit(PC[molecule == checkl[mai.c]$molecule & adduct == checkl[mai.c]$adduct & isoabb == checkl[mai.c]$isoabb])



    if(nrow(df) > 0){

      df$id <- seq(nrow(df))


      ol_matrix <- matrix(nrow = nrow(df), ncol = nrow(df))

      idf <- intervals::Intervals(df[,c("peaks.StartTime", "peaks.EndTime")])
      as.data.frame(intervals::interval_union(idf))

      idl <- lapply(unique(df$id),function(x){var <- as(intervals::Intervals(df[df$id==x,c("peaks.StartTime", "peaks.EndTime")]),"Intervals_full");intervals::closed(var)[,1]<- FALSE;return(var)})

      for(ii in rev(seq(length(idl)))[1:(length(idl)-1)]){
        idt_p <- idl[[ii]]
        n = 1
        for(i in idl[1:(ii-1)]){
          idt <- intervals::interval_intersection(idt_p,i)
          res <- as.data.frame(idt)
          if(nrow(res) > 0){

            ol <- (100 * (res$V2 - res$V1) / min( i[[2]] - i[[1]], idt_p[[2]] - idt_p[[1]]  ))

            if(ol > 50 & abs(res$V2 - res$V1) >= abs(df[id == n]$peaks.rt - df[id == ii]$peaks.rt) ){

              ol_matrix[ii, n] <- abs(df[id == n]$peaks.rt - df[id == ii]$peaks.rt)
              #ol_matrix[ii, n] <- if(df[id == n]$peaks.rt > df[id == ii]$peaks.StartTime & df[id == n]$peaks.rt < df[id == ii]$peaks.EndTime) {TRUE} else {NA}
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
  dplT <- outputT[, .(dpl = anyDuplicated(isoabb), aligned.grp = aligned.grp), by = .(molecule, adduct, FileName)]
  dplT <- unique(dplT[dpl > 0], by = c("molecule", "adduct", "aligned.grp"))

  setkeyv(dplT, c("molecule", "adduct", "aligned.grp"))
  setkeyv(outputT, c("molecule", "adduct", "aligned.grp"))

  outputT <- outputT[!dplT]
  outputT <- outputT[!is.na(aligned.grp)]


  #decide for best aligned.grp
  if(pick_best == "rt_match"){
    tmp <- outputT[isoabb == 100, .(rt.diff = abs(mean(peaks.rt) - mean(user.rt))), by = .(molecule, aligned.grp) ]
    tmp <- tmp[, .(aligned.grp = aligned.grp, rt.diff = rt.diff, MINrt.diff = min(rt.diff)), by = .(molecule) ]
    tmp <- tmp[rt.diff == MINrt.diff]
    outputT <- outputT[tmp, on = .(molecule, aligned.grp), nomatch = NULL, allow.cartesian = TRUE][, !c("MINrt.diff", "rt.diff")]
  } else if(pick_best == "highest_mean_area"){
    tmp <- outputT[isoabb == 100, .(mean_area = abs(mean(peaks.rt) - mean(user.rt))), by = .(molecule, aligned.grp) ]
    tmp <- tmp[, .(aligned.grp = aligned.grp, mean_area = mean_area, Max_mean_area = min(mean_area)), by = .(molecule) ]
    tmp <- tmp[mean_area == Max_mean_area]
    outputT <- outputT[tmp, on = .(molecule, aligned.grp), nomatch = NULL, allow.cartesian = TRUE]
  }




  return(outputT)
}
