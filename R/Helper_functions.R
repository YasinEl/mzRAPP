#' getXIC
#'
#' @param PC
#' @param IndexNumber
#'
#' @return
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
#' @param vct
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
#' @param x
#' @param digi
#'
#' @return
#' @export
#'
#' @noRd
round.woe <- function(x, digi){

  if(is.numeric(x) == TRUE){

    return(round(x, digi))

  } else return(NA)


}





