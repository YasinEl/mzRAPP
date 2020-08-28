#' plot_sunburst_alignment
#'
#' @param result_txt output from \code{\link{generate_results_text}}
#'
#' @return plotly object
#' @export
#'
plot_sunburst_alignment <- function(result_txt){




  dt <- data.table(BM = c("peaks<br>not found", "peaks<br>found", "peaks<br>found", "peaks<br>found", "peaks<br>found"),
                   peak_tarnsfer = c("NA", "peaks<br>lost", "peaks<br>retained", "peaks<br>retained", "peaks<br>retained"),
                   correct = c("NA", "NA", "correct", "incorrect", "incorrect"),
                   type = c("NA", "NA", "NA", "BM div", "min errors"))

  #print(dt)
  dt1 <- as.sunburstDF(dt)

  dt1 <- dt1[grepl("NA", ids) == FALSE]

  dt1[,values := c(result_txt[["Benchmark"]][["BM_peaks"]], #BM peaks
                   result_txt[["Benchmark"]][["BM_peaks"]] - #not found peaks
                     result_txt[["Before_alignment"]][["Found_peaks"]][["count"]],
                   result_txt[["Before_alignment"]][["Found_peaks"]][["count"]], #found peaks
                   result_txt[["Alignmnet"]][["Lost_b.A"]][["count"]], #lost
                   result_txt[["Before_alignment"]][["Found_peaks"]][["count"]] - #not lost
                     result_txt[["Alignmnet"]][["Lost_b.A"]][["count"]],
                   result_txt[["Before_alignment"]][["Found_peaks"]][["count"]] - #no errors
                     result_txt[["Alignmnet"]][["Lost_b.A"]][["count"]] -
                     result_txt[["Alignmnet"]][["BM_divergences"]][["count"]],
                   result_txt[["Alignmnet"]][["BM_divergences"]][["count"]], #errors
                   result_txt[["Alignmnet"]][["BM_divergences"]][["count"]] -
                     result_txt[["Alignmnet"]][["Min.Errors"]][["count"]], #BM div
                   result_txt[["Alignmnet"]][["Min.Errors"]][["count"]] #Min errors
  )
  ]

  dt1 <- dt1[values > 0]

  dt1[labels == "total", colors := "#FFFFFF"]
  dt1[labels == "peaks<br>not found", colors := "#ccd1d1"]
  dt1[labels == "peaks<br>found", colors := "#82e0aa"]
  dt1[labels == "peaks<br>lost", colors := "#ccd1d1"]
  dt1[labels == "peaks<br>retained", colors := "#85c1e9"]
  dt1[labels == "correct", colors := "#82e0aa"]
  dt1[labels == "incorrect", colors := "#f5b041"]
  dt1[labels == "BM div", colors := "#af7ac5"]
  dt1[labels == "min errors", colors := "#ec7063"]

  dt1[labels == "total", labels := "Benchmark<br>peaks"]
  dt1[labels == "peaks<br>retained", labels := "peaks<br>aligned"]
  dt1[labels == "correct", labels := "correct<br>alignment"]
  dt1[labels == "incorrect", labels := "incorrect<br>alignment"]
  dt1[labels == "BM div", labels := "Benchmark<br>divergencies"]
  dt1[labels == "min errors", labels := "Confirmable<br>errors"]

  p <-
    plotly::plot_ly(data = dt1,
                    ids = ~ids,
                    labels= ~labels,
                    parents = ~parents,
                    values= ~values,
                    type='sunburst',
                    branchvalues = 'total',
                    marker = list(colors = as.list(dt1$colors))
    )

  return(p)

}








#' plot_sunburst_peaks
#'
#' @param result_txt output from \code{\link{generate_results_text}}
#' @param comparison_object output from \code{\link{compare_peaks}}
#'
#' @return plotly object
#' @export
#'
plot_sunburst_peaks <- function(result_txt, comparison_object){

  summary_tab <- comparison_object[["feature_table"]]


  dt <- data.table(BM = c("peaks<br>not found", "peaks<br>not found", "peaks<br>found", "peaks<br>found"),
                   unaligned = c("peaks<br>found", "peaks<br>not found", "peaks<br>found", "peaks<br>not found"))

  dt1 <- as.sunburstDF(dt)
  dt1 <- dt1[grepl("NA", ids) == FALSE]

  dt1[,values := c(result_txt[["Benchmark"]][["BM_peaks"]], #BM peaks
                   result_txt[["Benchmark"]][["BM_peaks"]] - #not found peaks
                     result_txt[["Before_alignment"]][["Found_peaks"]][["count"]],
                   result_txt[["Before_alignment"]][["Found_peaks"]][["count"]], #found peaks
                   nrow(summary_tab[is.na(peak_area_ug) & !is.na(area_g)]), #bm-nf-f
                   nrow(summary_tab[is.na(peak_area_ug) & is.na(area_g)]), #bm-nf-nf
                   nrow(summary_tab[!is.na(peak_area_ug) & !is.na(area_g)]), #bm-f-f
                   nrow(summary_tab[!is.na(peak_area_ug) & is.na(area_g)]) #bm-f-nf
  )
  ]

  dt1 <- dt1[values > 0]

  dt1[labels == "total", colors := "#FFFFFF"]
  dt1[labels == "peaks<br>not found", colors := "#ccd1d1"]
  dt1[labels == "peaks<br>found", colors := "#82e0aa"]
  dt1[labels == "peaks<br>lost", colors := "#ccd1d1"]
  dt1[labels == "peaks<br>retained", colors := "#85c1e9"]

  dt1[labels == "total", labels := "Benchmark<br>peaks"]

  p <-
    plotly::plot_ly(data = dt1,
                    ids = ~ids,
                    labels= ~labels,
                    parents = ~parents,
                    values= ~values,
                    type='sunburst',
                    branchvalues = 'total',
                    marker = list(colors = as.list(dt1$colors))
    )

  return(p)

}





#' plot_sunburst_peakQuality
#'
#' @param result_txt output from \code{\link{generate_results_text}}
#' @param comparison_object output from \code{\link{compare_peaks}}
#'
#' @return plotly object
#' @export
#'
plot_sunburst_peakQuality <- function(result_txt, comparison_object){

  bm <- rbindlist(list(comparison_object$c_table,
                       comparison_object$nf_b_table),
                  fill = TRUE,
                  use.names = TRUE)

  ratios <- comparison_object$iso_err_dt


  dt <- data.table(BM = c("IR bias<br>< 20%", "IR bias<br>< 20%", "IR bias<br>< 20%", "IR bias<br>> 20%", "IR bias<br>> 20%", "IR bias<br>> 20%", "Missing peak", "Missing peak", "Missing peak"),
                   unaligned = c("IR bias<br>< 20%", "IR bias<br>> 20%", "Missing peak", "IR bias<br>< 20%", "IR bias<br>> 20%", "Missing peak", "IR bias<br>< 20%", "IR bias<br>> 20%", "Missing peak"))

  dt1 <- as.sunburstDF(dt)
  dt1 <- dt1[grepl("NA", ids) == FALSE]

  dt1[,values := c(nrow(bm[!is.na(ErrorRel_A_b)]), #BM ratios
                   nrow(ratios[diffH20PP_pp == "Inc. < 20%p"]), #ug good ratios
                   nrow(ratios[diffH20PP_pp == "Inc. > 20%p"]), #ug bad ratios
                   nrow(bm[!is.na(ErrorRel_A_b)])-
                     nrow(ratios[!is.na(diffH20PP_pp)]), #ug missing ratios
                   nrow(ratios[diffH20PP_pp == "Inc. < 20%p" &
                                 diffH20PP_ft == "Inc. < 20%p"]), #ug good - g good ratios
                   nrow(ratios[diffH20PP_pp == "Inc. < 20%p" &
                                 diffH20PP_ft == "Inc. > 20%p"]), #ug good - g bad ratios
                   nrow(ratios[diffH20PP_pp == "Inc. < 20%p" &
                                 is.na(diffH20PP_ft)]), #ug good - g good ratios
                   nrow(ratios[diffH20PP_pp == "Inc. > 20%p" &
                                 diffH20PP_ft == "Inc. < 20%p"]), #ug bad - g good ratios
                   nrow(ratios[diffH20PP_pp == "Inc. > 20%p" &
                                 diffH20PP_ft == "Inc. > 20%p"]), #ug bad - g bad ratios
                   nrow(ratios[diffH20PP_pp == "Inc. > 20%p" &
                                 is.na(diffH20PP_ft)]), #ug bad - g missing ratios
                   nrow(ratios[is.na(diffH20PP_pp) &
                                 diffH20PP_ft == "Inc. < 20%p"]), #ug missing - g good ratios
                   nrow(ratios[is.na(diffH20PP_pp) &
                                 diffH20PP_ft == "Inc. > 20%p"]), #ug missing - g bad ratios
                   nrow(bm[!is.na(ErrorRel_A_b)]) -
                     nrow(ratios[!is.na(diffH20PP_pp)]) -
                     nrow(ratios[is.na(diffH20PP_pp) &
                                   diffH20PP_ft == "Inc. < 20%p"]) -
                     nrow(ratios[is.na(diffH20PP_pp) &
                                   diffH20PP_ft == "Inc. > 20%p"]) #ug missing - g missing ratios
  )
  ]

  dt1 <- dt1[values > 0]

  dt1[labels == "total", colors := "#FFFFFF"]
  dt1[labels == "Missing peak", colors := "#ccd1d1"]
  dt1[labels == "IR bias<br>< 20%", colors := "#82e0aa"]
  dt1[labels == "IR bias<br>> 20%", colors := "#ec7063"]

  dt1[labels == "total", labels := "Benchmark<br>isotopic ratios"]
  dt1[labels == "Missing peak", labels := "Missing peak(s)"]
  dt1[labels == "IR bias<br>< 20%", labels := "IR bias<br>inc. < 20%"]
  dt1[labels == "IR bias<br>> 20%", labels := "IR bias<br>inc. > 20%"]

  p <-
    plotly::plot_ly(data = dt1,
                    ids = ~ids,
                    labels= ~labels,
                    parents = ~parents,
                    values= ~values,
                    type='sunburst',
                    branchvalues = 'total',
                    marker = list(colors = as.list(dt1$colors))
    )

  return(p)

}







#' as.sunburstDF
#'
#' @param DF DF
#' @param valueCol valueCol
#'
#' @description This function was written by ismirsehregal on stackoverflow https://stackoverflow.com/questions/57395424/how-to-format-data-for-plotly-sunburst-diagram
#' @export
#'
as.sunburstDF <- function(DF, valueCol = NULL){
  require(data.table)

  DT <- data.table(DF, stringsAsFactors = FALSE)
  DT[, root := "total"]
  setcolorder(DT, c("root", names(DF)))

  hierarchyList <- list()
  if(!is.null(valueCol)){setnames(DT, valueCol, "values", skip_absent=TRUE)}
  hierarchyCols <- setdiff(names(DT), "values")

  for(i in seq_along(hierarchyCols)){
    currentCols <- names(DT)[1:i]
    if(is.null(valueCol)){
      currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
    }
    setnames(currentDT, length(currentCols), "labels")
    hierarchyList[[i]] <- currentDT
  }

  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)

  parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parentCols) := NULL]
  return(hierarchyDT)
}
