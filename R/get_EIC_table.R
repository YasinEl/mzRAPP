#' get_EIMatches_BM_NPPpeaks
#'
#' @param rt rt
#' @param int int
#' @param Min.PpP Min.PpP
#'
#'
#'
#' @keywords internal
get_EIMatches_BM_NPPpeaks <- function(rt, int, Min.PpP) {
  EIC.dt <- data.table::data.table(
    rt = rt[!duplicated(rt)],
    int = int[!duplicated(rt)],
    val = as.vector(S4Vectors::Rle(int[!duplicated(rt)] > 0)),
    len = unlist(lapply(S4Vectors::Rle(int[!duplicated(rt)] > 0)@lengths, function(x) {
      rep(x, x)
    })),
    int_wo_spikes = int[!duplicated(rt)]
  )

#  EIC.dt[val == TRUE &
#           len <= max(Min.PpP / 3, 2)]$int_wo_spikes <- 0
#
#  EIC.dt$val <- as.vector(S4Vectors::Rle(EIC.dt$int_wo_spikes > 0))
#  EIC.dt$len <-
#    unlist(lapply(S4Vectors::Rle(EIC.dt$int_wo_spikes > 0)@lengths, function(x) {
#      rep(x, x)
#    }))

  EIC.dt <- EIC.dt[val == TRUE & len <= max(Min.PpP / 3, 2), int_wo_spikes := NA]

  if(sum(EIC.dt$int_wo_spikes, na.rm =  TRUE) == 0 | length(EIC.dt[int_wo_spikes > 0]$int_wo_spikes) < 5) {
    EIC.dt <- EIC.dt[, int_smooth := int_wo_spikes][]
    return(EIC.dt)
  }

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
