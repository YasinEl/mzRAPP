#' getOverlapWithLine1
#'
#' @param a
#' @param b
#'
#' @return
#'
#' @export
#'
#' @examples
getOverlapWithLine1 <- function(a, b){

  x <- data.table(a = a,
                  b = b)
  rowN <- nrow(x)
  ol <- NULL

  if(rowN < 2){return(1)}

  for(i in 2:rowN){

    temp <- x[c(1,i), Reduce(intersect,mapply(seq, a, b, 1))]

    ol <- c(ol, temp)

  }
if( S4Vectors::isEmpty(ol) ) {return(1)}
    histo <- as.data.table(table(ol))
    if(nrow(histo) == 0){return(1)}
    scan <- median(as.numeric(histo[N == max(N)]$ol))
    return(scan)
    }
