#' get_EIC_table
#'
#' @param rt
#' @param int
#' @param Min.PpP
#'
#' @return
#' @export
#'
#' @examples
get_EIC_table <- function(rt, int, Min.PpP) {
  EIC.dt <- data.table(
    rt = rt,
    int = int,
    val = as.vector(S4Vectors::Rle(int > 0)),
    len = unlist(lapply(S4Vectors::Rle(int > 0)@lengths, function(x) {
      rep(x, x)
    })),
    int_wo_spikes = int
  )

  EIC.dt[val == TRUE &
           len <= max(Min.PpP / 3, 3)]$int_wo_spikes <- 0

  EIC.dt$val <- as.vector(S4Vectors::Rle(EIC.dt$int_wo_spikes > 0))
  EIC.dt$len <-
    unlist(lapply(S4Vectors::Rle(EIC.dt$int_wo_spikes > 0)@lengths, function(x) {
      rep(x, x)
    }))

  EIC.dt[val == FALSE & len < 3]$int_wo_spikes <- NA



  EIC.dt_tmp <-
    EIC.dt[!is.na(int_wo_spikes)][, "rt"][, int_smooth := sapply(signal::sgolayfilt(
      EIC.dt[!is.na(int_wo_spikes)]$int_wo_spikes,
      p =
        3,
      n = max(5, ifelse(
        DescTools::IsOdd(round(0.05 * nrow(EIC.dt[int > 0.1 * max(int)]))) == TRUE,
        round(0.05 * nrow(EIC.dt[int >
                                   0.1 * max(int)])),
        round(0.05 * nrow(EIC.dt[int >
                                   0.1 * max(int)])) + 1
      ))
    ),
    function(ele) {if (ele < 0) ele = 0 else ele = ele})]


  EIC.dt <- EIC.dt_tmp[EIC.dt, on = .(rt)]
  EIC.dt[, sc.i := seq(nrow(EIC.dt))][]

  return(EIC.dt)

}
