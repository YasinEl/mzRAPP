#' plot_Peak
#'
#' @param PC PeakCandidates tabel from from which peak should be plotted
#' @param IndexNumber IDX number of peak to be plotted
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importfrom plotly ggplotly
#'
#' @examples
plot_Peak  <- function(PC, IndexNumber){

  plot.table <- data.table(rt = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, RT.v], split = ","))),
                           int = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, Intensities.v], split = ","))))

  p <- ggplot(plot.table, aes(x=rt, y=int)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = PC[IDX == IndexNumber, peaks.baseL], color = "blue") +
    geom_vline(xintercept = PC[IDX == IndexNumber, peaks.StartTime], color = "red") +
    geom_vline(xintercept = PC[IDX == IndexNumber, peaks.EndTime], color = "red") +
    ggtitle(IndexNumber)

  ggplotly(p)

}

#' plot_Peak_with_predicted_peak
#'
#' @param PC
#' @param molecule
#' @param file
#'
#' @return
#' @export
#'
#' @examples
plot_Peak_with_predicted_peak  <- function(PC_object, IndexNumber){


  #if(is.null(nrow(PC_object[IDX == IndexNumber, molecule]))){return(NULL)}

  mol <- as.character(PC_object[IDX == IndexNumber, molecule])


  fil <- as.character(PC_object[IDX == IndexNumber, FileName])


  add <- as.character(PC_object[IDX == IndexNumber, adduct])

  m0.grp <- as.character(PC_object[IDX == IndexNumber, peaks.M0.grp])

  #return(paste0(nrow(PC_object[molecule == mol & FileName == fil & adduct == add & isoabb == 100]), mol, add))


  IDXM0 <- as.character(PC_object[molecule == mol & FileName == fil & adduct == add & isoabb == 100 & peaks.M0.grp == m0.grp][1]$IDX)

  iso <- PC_object[IDX == IndexNumber, isoabb]

  plot.table.MX <- data.table(rt = as.numeric(unlist(strsplit(PC_object[IDX == IndexNumber, RT.v], split = ","))),
                           int = as.numeric(unlist(strsplit(PC_object[IDX == IndexNumber, Intensities.v], split = ","))))

  plot.table.M0 <- data.table(rt = as.numeric(unlist(strsplit(PC_object[IDX == IDXM0, RT.v], split = ","))),
                              int = as.numeric(unlist(strsplit(PC_object[IDX == IDXM0, Intensities.v], split = ","))) * iso / 100)

  p <- ggplot() +
    geom_line(data = plot.table.MX, aes(x = rt, y = int), color = "black") +
    geom_point(data = plot.table.MX, aes(x = rt, y = int), color = "black") +
    #geom_hline(yintercept = PC_object[IDX == IndexNumber, peaks.baseL], color = "blue") +
    geom_vline(xintercept = PC_object[IDX == IndexNumber, peaks.StartTime], color = "red") +
    geom_vline(xintercept = PC_object[IDX == IndexNumber, peaks.EndTime], color = "red") +


    geom_line(data = plot.table.M0, aes(x = rt, y = int), color = "grey") +
    geom_point(data = plot.table.M0, aes(x = rt, y = int), color = "grey") +
    #geom_hline(yintercept = PC_object[IDX == IDXM0, peaks.baseL] * iso / 100, color = "grey") +
    geom_vline(xintercept = PC_object[IDX == IDXM0, peaks.StartTime], color = "grey") +
    geom_vline(xintercept = PC_object[IDX == IDXM0, peaks.EndTime], color = "grey") +



    ggtitle(paste0(mol, " ", add, " ", round(iso, 2)))

  plotly::ggplotly(p)


}




#' plot_Peak_per_mol
#'
#' @param PC_object
#' @param mol
#' @param ia
#' @param add
#'
#' @return
#' @export
#'
#' @examples
plot_Peak_per_mol  <- function(PC_object, mol, ia = 100, add = "M+H"){
UT_comp = FALSE
  if("molecule_b" %in% colnames(PC_object) == TRUE){
    UT_comp = TRUE
    PC_object <- PC_object[main_peak == TRUE | is.na(peak_area_ug)]
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "molecule_b", "molecule")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "adduct_b", "adduct")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "isoabb_b", "isoabb")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "sample_name_b", "FileName")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "RT.v_b", "RT.v")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "Intensities.v_b", "Intensities.v")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "rt_start_b", "peaks.StartTime")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "rt_end_b", "peaks.EndTime")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "peaks.idx_b", "peaks.idx")
  }

  plot_dt <- data.table(sample = NA, rt = NA, int = NA, start = NA, end = NA)
  if(UT_comp == TRUE){

    plot_dt <- cbind(plot_dt, data.table(start.ut = NA, end.ut = NA))
  }

  for (i in unique(PC_object[molecule == mol & adduct == add & round(isoabb, 2) == ia]$FileName)) {
    for (a in unique(PC_object[molecule == mol & adduct == add & round(isoabb, 2) == ia]$peaks.idx)){
    plot_dt <-  rbind(fill = TRUE,
      plot_dt,
      data.table(
        sample = rep(i, length(unlist(
          strsplit(PC_object[FileName == i &
                               molecule == mol &
                               adduct == add &
                               peaks.idx == a &
                               round(isoabb, 2) == ia, RT.v], split = ",")
        ))),



        rt = as.numeric(unlist(strsplit(
          PC_object[FileName == i &
                      molecule == mol &
                      adduct == add &
                      peaks.idx == a &
                      round(isoabb, 2) == ia, RT.v], split = ","
        ))),

        int = as.numeric(unlist(strsplit(
          PC_object[FileName == i &
                      molecule == mol &
                      adduct == add &
                      peaks.idx == a &
                      round(isoabb, 2) == ia, Intensities.v], split = ","
        ))),

        start = rep(PC_object[FileName == i &
                                molecule == mol &
                                adduct == add &
                                peaks.idx == a &
                                round(isoabb, 2) == ia, peaks.StartTime], length(unlist(
                                  strsplit(PC_object[FileName == i &
                                                       molecule == mol &
                                                       adduct == add &
                                                       peaks.idx == a &
                                                       round(isoabb, 2) == ia, RT.v], split = ",")
                                ))),

        end = rep(PC_object[FileName == i &
                              molecule == mol &
                              adduct == add &
                              peaks.idx == a &
                              round(isoabb, 2) == ia, peaks.EndTime], length(unlist(
                                strsplit(PC_object[FileName == i &
                                                     molecule == mol &
                                                     adduct == add &
                                                     peaks.idx == a &
                                                     round(isoabb, 2) == ia, RT.v], split = ",")
                              ))),


        start.ut = rep(ifelse(UT_comp == TRUE, PC_object[FileName == i &
                                molecule == mol &
                                adduct == add &
                                peaks.idx == a &
                                round(isoabb, 2) == ia, rt_start_ug], NA), length(unlist(
                                  strsplit(PC_object[FileName == i &
                                                       molecule == mol &
                                                       adduct == add &
                                                       peaks.idx == a &
                                                       round(isoabb, 2) == ia, RT.v], split = ",")
                                ))),

        end.ut = rep(ifelse(UT_comp == TRUE, PC_object[FileName == i &
                                   molecule == mol &
                                   adduct == add &
                                   peaks.idx == a &
                                   round(isoabb, 2) == ia, rt_end_ug], NA), length(unlist(
                                     strsplit(PC_object[FileName == i &
                                                          molecule == mol &
                                                          adduct == add &
                                                          peaks.idx == a &
                                                          round(isoabb, 2) == ia, RT.v], split = ",")
                                   )))
      )
    )
    }
  }
plot_dt <- plot_dt[-1]


  p <- ggplot() +
    geom_line(data = plot_dt, aes(x = rt, y = int, color = sample)) +
    geom_point(data = plot_dt, aes(x = rt, y = int, color = sample)) +
    geom_vline(data = plot_dt, aes(xintercept = start, colour = sample)) +
    geom_vline(data = plot_dt, aes(xintercept = end, colour = sample)) +
    theme(legend.position = "right") +
    ggtitle(paste0(mol, " ", add, " ", ia)) +
    labs(x = "Retention time [sec]", y = "Intensity")

  if(UT_comp == TRUE){
    p <- p +
    geom_vline(data = plot_dt, aes(xintercept = start.ut, colour = sample), linetype='dashed') +
    geom_vline(data = plot_dt, aes(xintercept = end.ut, colour = sample), linetype='dashed')
  }

  plotly::ggplotly(p, dynamicTicks = TRUE)

}
