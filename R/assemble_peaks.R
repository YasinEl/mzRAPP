#' assemble_peaks
#'
#' @param peak.grp
#' @param s
#' @param e
#' @param breakP
#'
#' @return
#'
#' @examples
assemble_peaks <- function(peak.grp, s, e, breakP) {
  if (anyNA(breakP)) {
    data.frame(peak.grp = peak.grp,
               s = as.integer(s),
               e = as.integer(e),
               stringsAsFactors = FALSE)
  }
  else {
    borders <- c(s, breakP, e)

    dfs <- lapply(seq_along(borders)[-length(borders)], function(i) {
      data.frame(s = borders[i],
                 e = borders[as.integer(i) + 1L],
                 stringsAsFactors = FALSE)
    })
    output <- data.frame(peak.grp = peak.grp, do.call(rbind, dfs, TRUE))

    return(output)
  }
}
