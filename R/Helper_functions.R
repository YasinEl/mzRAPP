#' plotly_click_wo_warnings
#'
#' @param sc sc
#'
#' @return dataframe; plotly event
#'
#' @import plotly
#'
#' @keywords internal
plotly_click_wo_warnings <- function(sc){

  storeWarn<- getOption("warn")
  options(warn = -1)
  event <- plotly::event_data("plotly_click", source = sc, priority = "event")

  return(event)

}



#' getXIC
#'
#' @param PC PC
#' @param IndexNumber IndexNumber
#'
#'
#'
#' @keywords internal
getXIC <- function(PC, IndexNumber){

plot.table <- data.table::data.table(rt = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, RT.v], split = ","))),
                         int = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, Intensities.v], split = ","))))

return(plot.table)

}



#' reIndexFeatures
#'
#' @param vct vct
#'
#' @return list
#'
#' @keywords internal
reIndexFeatures <- function(vct){

  apply(as.matrix(vct), 1, function(x, tab = names(sort(table(vct), decreasing = TRUE))){

    idx <- which(tab == x)

    y <- paste0("F" ,idx)
    y
  })



}



#' round_woe
#'
#' @param x x
#' @param stellen stellen
#'
#'
#' @keywords internal
round_woe <- function(x, stellen){

  if(!is.na(x) == TRUE & !is.null(x) == TRUE){

    return(round(x, stellen))

  } else return(NA)


}




#' top_to_x
#'
#' @param number numeric(1)
#' @param x roof to this number
#'
#' @return numeric
#' @keywords internal
#'
top_to_x <- function(number, x = 0){

  if(!is.na(number) && is.numeric(number) & number < 0){return(0)} else {return(number)}

}








