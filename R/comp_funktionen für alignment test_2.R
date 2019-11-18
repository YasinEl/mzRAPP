#' get_main_UT_groups
#'
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


  keep <- apply(DT[,!c("sample_id_b")], 1, function(x){any(!x %in% c("Lost_b.PP", "Lost_b.A", NA))}) #find samples without any UT_groupes
  DT <- DT[keep,] #remove samples without any UN_groupes

  #maybe check if some ut_groupes apper in more then one isoabb (mz error)

  repeat {

    bestIso <- suppressWarnings(names(which.max(apply(DT[,-1], 2, function(x) {max(table(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA, used_UTgrps) ]))}))))

    best_UTgrp <- names(which.max(table(na.omit(DT[!DT[[bestIso]] %in% c("Lost_b.PP", "Lost_b.A", NA, used_UTgrps), ..bestIso]))))

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


#' count_alignment_errors
#'
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

  error_list <- lapply(main_UTgroups[[1]], function(x) {

    entrustedGrp <- main_UTgroups[[2]][x]

    DTsub <- DT[DT[[entrustedGrp]] == main_UTgroups[[3]][x]]

    if(x>1){

      for(i in seq(x-1)){

        already_testedGrp <- main_UTgroups[[2]][i]

        DTsub <- DTsub[DTsub[[already_testedGrp]] != main_UTgroups[[3]][i]]

      }

    }


    DT <- DT[DT[[entrustedGrp]] == main_UTgroups[[3]][x]]


    isos_to_test <- list(seq(length(DTsub[, !c("sample_id_b", ..entrustedGrp)])),
                         colnames(DTsub[, !c("sample_id_b", ..entrustedGrp)]))



    errors <- lapply(isos_to_test[[1]], function(y){

      iso_to_test <- isos_to_test[[2]][y]

      yDTsub <- unname(unlist(DTsub[, ..iso_to_test]))
      yDTall <- unname(unlist(DT[, ..iso_to_test]))

      best_UTgrp <- names(which.max(table(yDTall[!yDTall %in% c("Lost_b.PP", "Lost_b.A", NA)])))



      if(!is.null(best_UTgrp)){

        alignment_splits_vector.all <- !yDTall %in% best_UTgrp
        alignment_splits_vector.sub <- !yDTsub %in% best_UTgrp


        if(length(as.character(alignment_splits_vector.all)[as.character(alignment_splits_vector.all) == "FALSE"]) > 1 &
           length(as.character(alignment_splits_vector.all)[as.character(alignment_splits_vector.all) == "TRUE"]) > 0){

          problematic_joins <- yDTsub[alignment_splits_vector.sub][!yDTsub[alignment_splits_vector.sub] %in% c("Lost_b.PP", NA)]

          return(length(problematic_joins))


        }
      }


    })

    errors <- unlist(errors)


  })
  return(sum(unlist(error_list)))
  }



  if(method == "trustfull"){

    error_list <- apply(DT[, !c("sample_id_b")], 2, function(x) {

      if(length(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA)]) > 0) {
          best_UTgrp <- names(which.max(table(x[!x %in% c("Lost_b.PP", "Lost_b.A", NA)])))
      } else best_UTgrp <- NULL

      errors <- length(x[!x %in% c(best_UTgrp, "Lost_b.PP", NA)])
      return(errors)


    } )

    return(sum(unlist(error_list)))

  }
}




