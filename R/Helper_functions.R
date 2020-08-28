#' plotly_click_wo_warnings
#'
#' @param sc sc
#'
#' @return
#' @export
#'
#' @noRd
plotly_click_wo_warnings <- function(sc){

  storeWarn<- getOption("warn")
  options(warn = -1)
  event <- plotly::event_data("plotly_click", source = sc, priority = "event")


  #options(warn = storeWarn)

  return(event)

}



#' getXIC
#'
#' @param PC PC
#' @param IndexNumber IndexNumber
#'
#' @export
#'
#' @noRd
getXIC <- function(PC, IndexNumber){

plot.table <- data.table(rt = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, RT.v], split = ","))),
                         int = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, Intensities.v], split = ","))))

return(plot.table)

}



#' reIndexFeatures
#'
#' @param vct vct
#'
#' @return
#'
#' @noRd
reIndexFeatures <- function(vct){

  apply(as.matrix(vct), 1, function(x, tab = names(sort(table(vct), decreasing = TRUE))){

    idx <- which(tab == x)

    y <- paste0("F" ,idx)
    y
  })



}



#' round.woe
#'
#' @param x x
#' @param stellen stellen
#'
#' @export
#'
#' @noRd
round.woe <- function(x, stellen){

  if(!is.na(x) == TRUE & !is.null(x) == TRUE){

    return(round(x, stellen))

  } else return(NA)


}





