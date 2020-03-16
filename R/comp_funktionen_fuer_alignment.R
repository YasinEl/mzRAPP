#' @title get_main_UT_groups
#'
#' @description Find most occuring feature ID within matched peaks of a benchmark feature
#' @param DT
#'
#' @return
#' @export
#'
#' @examples



get_main_UT_groups <- function(DT){

  DT[] <- lapply(DT, function(x) as.character(x))

  if(nrow(DT) < 2){return(NA)}

  checked_files <- c()
  checked_UTgrps <- c()
  checked_isoabb <- c()
  used_UTgrps <- c()


  keep <- apply(DT[,!c("sample_id_b")], 1, function(x){any(!x %in% c("Lost_b.PP", "Lost_b.A", NA, ""))}) #find samples without any UT_groupes
  DT <- DT[keep,] #remove samples without any UN_groupes

  #maybe check if some ut_groupes apper in more then one isoabb (mz error)

  repeat {

    bestIso <- suppressWarnings(names(which.max(apply(DT[,-1], 2, function(x) {max(table(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA, "", used_UTgrps) ]))}))))

    best_UTgrp <- names(which.max(table(na.omit(DT[!DT[[bestIso]] %in% c("Lost_b.PP", "Lost_b.A", NA, "", used_UTgrps), ..bestIso]))))

    used_UTgrps <- c(used_UTgrps, best_UTgrp)

    if(nrow(DT[DT[[bestIso]] == best_UTgrp]) < 2) {return(list(seq_along(checked_isoabb), checked_isoabb, checked_UTgrps))}

    if(length(setdiff(unique(DT[DT[[bestIso]] == best_UTgrp]$sample_id_b), checked_files)) >= 1) {
      checked_files <- unique(c(checked_files, DT[DT[[bestIso]] == best_UTgrp]$sample_id_b))
      checked_isoabb <- c(checked_isoabb, bestIso)
      checked_UTgrps <- c(checked_UTgrps, best_UTgrp)
    }

    if(length(checked_files) == nrow(DT)) return(list(seq_along(checked_isoabb),checked_isoabb, checked_UTgrps))

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
#' @examples

count_alignment_errors <- function(DT, main_UTgroups, method = "self-critical"){

  DT[] <- lapply(DT, function(x) as.character(x))


  if(method == "self-critical"){

    if(length(DT) < 3 | is.na(main_UTgroups[[1]][1])){return(0L)}

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

          problematic_joins <- yDTsub[alignment_splits_vector.sub][!yDTsub[alignment_splits_vector.sub] %in% c("Lost_b.PP", NA, "")]
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


  empty_samples <- apply(DT[, !"sample_id_b"], 1, function(x) {

    if(length(x[x != "" & x != "Lost_b.A" & x != "Lost_b.PP"]) == 0){
      count.table <- data.table(table(x))
      if("Lost_b.A" %in% count.table$x){return(count.table[x == "Lost_b.A"]$N)}
    }
  })



  DT <-
    transform(DT, lost = apply(DT, 1, function(x) {
      if(length(x[-1][x[-1] != "" & x[-1] != "Lost_b.A" & x[-1] != "Lost_b.PP"]) == 0){
        return(TRUE)
      }
      return(FALSE)
    }))


  DT[DT == "Lost_b.A" & lost == TRUE] <- ""

  DT <- DT[, !"lost"]



empty_isos <- apply(DT[, !"sample_id_b"], 2, function(x) {

    if(length(x[x != "" & x != "Lost_b.A" & x != "Lost_b.PP"]) == 0){
      count.table <- data.table(table(x))
      if("Lost_b.A" %in% count.table$x){return(count.table[x == "Lost_b.A"]$N)}
    }
  })

  return(sum(unlist(error_list), unlist(empty_samples), unlist(empty_isos)))
  }



  if(method == "trustfull"){

    error_list <- apply(DT[, !c("sample_id_b")], 2, function(x) {

      if(length(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA)]) > 0) {
          best_UTgrp <- names(which.max(table(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA, "")])))
      } else best_UTgrp <- NULL

      errors <- length(x[!x %in% c(best_UTgrp, "Lost_b.PP", NA, "")])
      return(errors)


    } )

    return(sum(unlist(error_list)))

  }
}




