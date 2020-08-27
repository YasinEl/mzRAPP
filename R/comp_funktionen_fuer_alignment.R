#' @title get_main_UT_groups
#'
#' @description Find most occuring feature ID within matched peaks of a benchmark feature
#' @param DT
#'
#' @return
#' @export
#'
#' @noRd



get_main_UT_groups <- function(DT){

  DT[] <- lapply(DT, function(x) as.character(x))

  if(nrow(DT) < 2){return(NA)}

  checked_files <- c()
  checked_UTgrps <- c()
  checked_isoab <- c()
  used_UTgrps <- c()


  keep <- apply(DT[,!c("sample_id_b")], 1, function(x){any(!x %in% c("Lost_b.PP", "Lost_b.A", NA, ""))}) #find samples without any UT_groupes
  DT <- DT[keep,] #remove samples without any UN_groupes

  #maybe check if some ut_groupes apper in more then one isoab (mz error)

  repeat {

    bestIso <- suppressWarnings(names(which.max(apply(DT[,-1], 2, function(x) {max(table(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA, "", used_UTgrps) ]))}))))

    best_UTgrp <- names(which.max(table(na.omit(DT[!DT[[bestIso]] %in% c("Lost_b.PP", "Lost_b.A", NA, "", used_UTgrps), ..bestIso]))))

    used_UTgrps <- c(used_UTgrps, best_UTgrp)

    if(nrow(DT[DT[[bestIso]] == best_UTgrp]) < 2) {return(list(seq_along(checked_isoab), checked_isoab, checked_UTgrps))}

    if(length(setdiff(unique(DT[DT[[bestIso]] == best_UTgrp]$sample_id_b), checked_files)) >= 1) {
      checked_files <- unique(c(checked_files, DT[DT[[bestIso]] == best_UTgrp]$sample_id_b))
      checked_isoab <- c(checked_isoab, bestIso)
      checked_UTgrps <- c(checked_UTgrps, best_UTgrp)
    }

    if(length(checked_files) == nrow(DT)) return(list(seq_along(checked_isoab),checked_isoab, checked_UTgrps))

  }

}


#' @title count_alignment_errors
#'
#' @description count alignment errors occuring within a compound
#' @param DT
#' @param main_UTgroups
#' @param method
#'
#' @return
#' @export
#'
#' @noRd

count_alignment_errors <- function(DT, main_UTgroups, method = "both"){

  DT[] <- lapply(DT, function(x) as.character(x))


  if(method == "self-critical" | method == "both"){

    lba <- as.data.table(table(unlist(DT)))
    if(nrow(lba[V1 == "Lost_b.A"]) == 1) {
      lba_e <- as.integer(lba[V1 == "Lost_b.A"]$N)
    } else {lba_e <- 0L}

    if(length(DT) < 3 | is.na(main_UTgroups[[1]][1])){return(c(errors = 0L, Lost_b.A = lba_e, diff_BM = 0L))}

    #going through isotopologues which are necessary to cover all samples!
  error_list <- lapply(main_UTgroups[[1]], function(x) {

    entrustedGrp <- main_UTgroups[[2]][x] #isotopologue for this round
#print(paste0("entrustedGrp: ", entrustedGrp))
    DTsub <- DT[DT[[entrustedGrp]] == main_UTgroups[[3]][x]] #samples for which this isotopologues can be used
    #print(paste0("DT_sub: "))
    #print(DTsub)
    if(x>1){

      for(i in seq(x-1)){

        already_testedGrp <- main_UTgroups[[2]][i]
        #print(paste0("alreadyTestedGRP: ", already_testedGrp))

        DTsub <- DTsub[DTsub[[already_testedGrp]] != main_UTgroups[[3]][i]]

      }

    }
    #print(paste0("DT_sub: "))
    #print(DTsub)

    DT <- DT[DT[[entrustedGrp]] == main_UTgroups[[3]][x]]
    #print(paste0("DT: "))
    #print(DT)

    isos_to_test <- list(seq(length(DTsub[, !c("sample_id_b", ..entrustedGrp)])), #all isotopologues which should now be checked via the one used in this round
                         colnames(DTsub[, !c("sample_id_b", ..entrustedGrp)]))

    #print("Isos to test:")
#print(isos_to_test)

    errors <- lapply(isos_to_test[[1]], function(y){

      iso_to_test <- isos_to_test[[2]][y] #iso checked in this round


      #print("Iso to test:")
      #print(iso_to_test)


      yDTsub <- unname(unlist(DTsub[, ..iso_to_test]))
      yDTall <- unname(unlist(DT[, ..iso_to_test]))
      #print(paste0("ydtsub: "))
      #print(yDTsub)
      #print(paste0("ydtall: "))
      #print(yDTall)
      best_UTgrp <- names(which.max(table(yDTall[!yDTall %in% c("Lost_b.PP", "Lost_b.A", NA, "")])))

      #print("Best table: ")
      #print(table(yDTall[!yDTall %in% c("Lost_b.PP", "Lost_b.A", NA, "")]))
      #print(best_UTgrp)

      if(!is.null(best_UTgrp)){

        alignment_splits_vector.all <- !yDTall %in% best_UTgrp
        alignment_splits_vector.sub <- !yDTsub %in% best_UTgrp

        #print(alignment_splits_vector.all)
        #print(alignment_splits_vector.all)

        if(length(as.character(alignment_splits_vector.all)[as.character(alignment_splits_vector.all) == "FALSE"]) > 0 &
           length(as.character(alignment_splits_vector.all)[as.character(alignment_splits_vector.all) == "TRUE"]) > 0){

          problematic_joins <- yDTsub[alignment_splits_vector.sub][!yDTsub[alignment_splits_vector.sub] %in% c("Lost_b.A", "Lost_b.PP", NA, "")]
#print(paste0("problematic joins: "))
#print(problematic_joins)

#print("end of Iso to test succ:")
#print(iso_to_test)
          return(length(problematic_joins))


        }
      }

     # print("Iso to testto test unsuc:")
    #  print(iso_to_test)
    })

    errors <- unlist(errors)


  })

 # lba <- as.data.table(table(unlist(DT)))
 # if(nrow(lba[V1 == "Lost_b.A"]) == 1) {
 #   lba_e <- as.integer(lba[V1 == "Lost_b.A"]$N)
#  } else {lba_e <- 0L}
#
  if(method == "self-critical"){
    return(c(errors = as.integer(sum(unlist(error_list))), Lost_b.A = as.integer(sum(lba_e)), diff_BM = as.integer(NA)))
  }
  }



  if(method == "trustfull" | method == "both"){

    diff_BM_list <- apply(DT[, !c("sample_id_b")], 2, function(x) {

      if(length(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA)]) > 0) {
          best_UTgrp <- names(which.max(table(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA, "")])))
      } else best_UTgrp <- NULL

      diff_BM_c <- length(x[!x %in% c(best_UTgrp, "Lost_b.PP", NA, "", "Lost_b.A")])
      return(diff_BM_c)


    } )

    if(method == "trustfull"){
      return(c(errors = as.integer(NA), Lost_b.A = as.integer(sum(lba_e)), diff_BM = as.integer(sum(unlist(diff_BM_list)))))
    }

    return(c(errors = as.integer(sum(unlist(error_list))), Lost_b.A = as.integer(sum(lba_e)), diff_BM = as.integer(sum(unlist(diff_BM_list)))))
  }
}




