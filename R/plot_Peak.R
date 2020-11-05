#' plot_Peak
#'
#' @param PC output from \code{\link{find_bench_peaks}}
#' @param IndexNumber IDX number of peak to be plotted
#'
#' @return plotly object
#' @export
#'
plot_Peak  <- function(PC, IndexNumber){

  UT_comp <- FALSE
  if("molecule_b" %in% colnames(PC) == TRUE){
    UT_comp <- TRUE
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "molecule_b", "molecule")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "adduct_b", "adduct")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "isoab_b", "isoab")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "sample_name_b", "FileName")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "RT.v_b", "RT.v")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "Intensities.v_b", "Intensities.v")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "rt_start_b", "peaks.StartTime")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "rt_end_b", "peaks.EndTime")
    colnames(PC) <- replace(colnames(PC), colnames(PC) == "comp_id_b", "IDX")
  }

  plot.table <- data.table(rt = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, RT.v], split = ","))),
                           int = as.numeric(unlist(strsplit(PC[IDX == IndexNumber, Intensities.v], split = ","))))

  p <- ggplot(plot.table, aes(x=rt, y=int)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = PC[IDX == IndexNumber, peaks.StartTime] - 0.01, color = "red") +
    geom_vline(xintercept = PC[IDX == IndexNumber, peaks.EndTime]+0.01, color = "red") +
    theme(legend.position = "right") +
    labs(x = "Retention time [sec]",
         y = "Intensity",
         caption = "Chromatogram plotted as extracted in benchmark.",
         title = paste0(PC[IDX == IndexNumber, molecule], " ",
                       PC[IDX == IndexNumber, adduct], " IsoAb.:",
                       round(PC[IDX == IndexNumber, isoab], 2)))


  if(UT_comp == TRUE){
    p <-
    p + annotate("text",
             x = min(plot.table$rt) + (max(plot.table$rt) - min(plot.table$rt)) * 0.8,
             y = max(plot.table$int),
             label = "Benchmark",
             color="red")
    }

  if(UT_comp == TRUE && PC[IDX == IndexNumber]$NPP_status != 'Not Found'){

    p <- p +
      geom_vline(xintercept = PC[IDX == IndexNumber, rt_start_ug], color = "blue", linetype='dashed') +
      geom_vline(xintercept = PC[IDX == IndexNumber, rt_end_ug], color = "blue", linetype='dashed') +
      annotate("text",
               x = min(plot.table$rt) + (max(plot.table$rt) - min(plot.table$rt)) * 0.8,
               y = 0.9*max(plot.table$int),
               label = "Non-targeted",
               color="blue")


  }


  plotly::ggplotly(p, dynamicTicks = TRUE) %>%
    plotly::layout(title = list(text = paste0(paste0(PC[IDX == IndexNumber, molecule], " ",
                                             PC[IDX == IndexNumber, adduct], " IsoAb.:",
                                             round(PC[IDX == IndexNumber, isoab], 2)),
                                      '<br>',
                                      '<sup>',
                                      "Chromatogram plotted as extracted in benchmark.",
                                      '</sup>')))
}

#' plot_Peak_with_predicted_peak
#'
#' @param PC_object output from \code{\link{find_bench_peaks}}
#' @param IndexNumber IDX number of peak to be plotted
#'
#'
#' @return plotly object
#' @export
#'
plot_Peak_with_predicted_peak  <- function(PC_object, IndexNumber){


  mol <- as.character(PC_object[IDX == IndexNumber, molecule])


  fil <- as.character(PC_object[IDX == IndexNumber, FileName])


  add <- as.character(PC_object[IDX == IndexNumber, adduct])

  m0.grp <- as.character(PC_object[IDX == IndexNumber, peaks.M0.grp])

  IDXM0 <- as.character(PC_object[molecule == mol & FileName == fil & adduct == add & isoab == 100 & peaks.M0.grp == m0.grp][1]$IDX)

  iso <- PC_object[IDX == IndexNumber, isoab]

  plot.table.MX <- data.table(rt = as.numeric(unlist(strsplit(PC_object[IDX == IndexNumber, RT.v], split = ","))),
                           int = as.numeric(unlist(strsplit(PC_object[IDX == IndexNumber, Intensities.v], split = ","))))

  plot.table.M0 <- data.table(rt = as.numeric(unlist(strsplit(PC_object[IDX == IDXM0, RT.v], split = ","))),
                              int = as.numeric(unlist(strsplit(PC_object[IDX == IDXM0, Intensities.v], split = ","))) * iso / 100)

  p <- ggplot() +
    geom_line(data = plot.table.MX, aes(x = rt, y = int), color = "black") +
    geom_point(data = plot.table.MX, aes(x = rt, y = int), color = "black") +
    geom_vline(xintercept = PC_object[IDX == IndexNumber, peaks.StartTime], color = "red") +
    geom_vline(xintercept = PC_object[IDX == IndexNumber, peaks.EndTime], color = "red") +


    geom_line(data = plot.table.M0, aes(x = rt, y = int), color = "grey") +
    geom_point(data = plot.table.M0, aes(x = rt, y = int), color = "grey") +
    geom_vline(xintercept = PC_object[IDX == IDXM0, peaks.StartTime], color = "grey") +
    geom_vline(xintercept = PC_object[IDX == IDXM0, peaks.EndTime], color = "grey") +



    ggtitle(paste0(mol, " ", add, " ", round(iso, 2)))

  plotly::ggplotly(p, dynamicTicks = TRUE)


}




#' plot_Peak_per_mol
#'
#' @param PC_object output from \code{\link{compare_peaks}} or output from \code{\link{find_bench_peaks}}
#' @param mol molecule
#' @param ia isotopic abundance rounded to 2 digits
#' @param add adduct
#'
#' @return plotly object
#' @export
#'
plot_Peak_per_mol  <- function(PC_object, mol, ia = 100, add = "M+H"){
UT_comp = FALSE

if(is.list(PC_object) == TRUE && is.data.table == FALSE){
  PC_object <- rbindlist(list(PC_object$Matches_BM_NPPpeaks, PC_object$Unmatched_BM_NPPpeaks), fill = TRUE, use.names = TRUE)
}


  if("molecule_b" %in% colnames(PC_object) == TRUE){
    UT_comp = TRUE
    PC_object <- PC_object[main_peak == TRUE | is.na(peak_area_ug)]
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "molecule_b", "molecule")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "adduct_b", "adduct")
    colnames(PC_object) <- replace(colnames(PC_object), colnames(PC_object) == "isoab_b", "isoab")
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

  for (i in unique(PC_object[molecule == mol & adduct == add & round(isoab, 2) == ia]$FileName)) {
    for (a in unique(PC_object[molecule == mol & adduct == add & round(isoab, 2) == ia]$peaks.idx)){
    plot_dt <-  rbind(fill = TRUE,
      plot_dt,
      data.table(
        sample = rep(i, length(unlist(
          strsplit(PC_object[FileName == i &
                               molecule == mol &
                               adduct == add &
                               peaks.idx == a &
                               round(isoab, 2) == ia, RT.v], split = ",")
        ))),



        rt = as.numeric(unlist(strsplit(
          PC_object[FileName == i &
                      molecule == mol &
                      adduct == add &
                      peaks.idx == a &
                      round(isoab, 2) == ia, RT.v], split = ","
        ))),

        int = as.numeric(unlist(strsplit(
          PC_object[FileName == i &
                      molecule == mol &
                      adduct == add &
                      peaks.idx == a &
                      round(isoab, 2) == ia, Intensities.v], split = ","
        ))),

        start = rep(PC_object[FileName == i &
                                molecule == mol &
                                adduct == add &
                                peaks.idx == a &
                                round(isoab, 2) == ia, peaks.StartTime], length(unlist(
                                  strsplit(PC_object[FileName == i &
                                                       molecule == mol &
                                                       adduct == add &
                                                       peaks.idx == a &
                                                       round(isoab, 2) == ia, RT.v], split = ",")
                                ))),

        end = rep(PC_object[FileName == i &
                              molecule == mol &
                              adduct == add &
                              peaks.idx == a &
                              round(isoab, 2) == ia, peaks.EndTime], length(unlist(
                                strsplit(PC_object[FileName == i &
                                                     molecule == mol &
                                                     adduct == add &
                                                     peaks.idx == a &
                                                     round(isoab, 2) == ia, RT.v], split = ",")
                              ))),


        start.ut = rep(ifelse(UT_comp == TRUE, PC_object[FileName == i &
                                molecule == mol &
                                adduct == add &
                                peaks.idx == a &
                                round(isoab, 2) == ia, rt_start_ug], NA), length(unlist(
                                  strsplit(PC_object[FileName == i &
                                                       molecule == mol &
                                                       adduct == add &
                                                       peaks.idx == a &
                                                       round(isoab, 2) == ia, RT.v], split = ",")
                                ))),

        end.ut = rep(ifelse(UT_comp == TRUE, PC_object[FileName == i &
                                   molecule == mol &
                                   adduct == add &
                                   peaks.idx == a &
                                   round(isoab, 2) == ia, rt_end_ug], NA), length(unlist(
                                     strsplit(PC_object[FileName == i &
                                                          molecule == mol &
                                                          adduct == add &
                                                          peaks.idx == a &
                                                          round(isoab, 2) == ia, RT.v], split = ",")
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





#' plot_IR_peaks
#'
#' @param PC PC
#' @param plotly_key plotly_key
#'
#'
#' @keywords internal
plot_IR_peaks  <- function(PC, plotly_key){
  id_vetor <- unlist(strsplit(plotly_key, split = "_;_"))

  PC <- PC[molecule_b == id_vetor[1] &
             adduct_b == id_vetor[2] &
             (isoab_b == id_vetor[3] | isoab_b == 100) &
             sample_name_b == id_vetor[4]]

  plot.table_li <- data.table(rt = as.numeric(unlist(strsplit(PC[isoab_b != 100, RT.v_b], split = ","))),
                              int = as.numeric(unlist(strsplit(PC[isoab_b != 100, Intensities.v_b], split = ","))),
                              lab = round(as.numeric(id_vetor[3]), 2))

  plot.table_hi <- data.table(rt = as.numeric(unlist(strsplit(PC[isoab_b == 100, RT.v_b], split = ","))),
                              int = as.numeric(unlist(strsplit(PC[isoab_b == 100, Intensities.v_b], split = ","))),
                              lab = 100)


  p_hi <- ggplot(plot.table_hi, aes(x=rt, y=int)) +
    theme_classic() +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = PC[isoab_b == 100, rt_start_b], color = "red") +
    geom_vline(xintercept = PC[isoab_b == 100, rt_end_b], color = "red") +
    labs(x = "Retention time [sec]",
         y = "Intensity",
         subtitle = "Chromatograms plotted as extracted during benchmark generation.",
         title = paste0(PC[isoab_b == 100, molecule_b], " | ",
                        PC[isoab_b == 100, adduct_b], " | ",
                        PC[isoab_b == 100, sample_name_b])) +
    geom_vline(xintercept = PC[isoab_b == 100, rt_start_ug], color = "blue", linetype='dashed') +
    geom_vline(xintercept = PC[isoab_b == 100, rt_end_ug], color = "blue", linetype='dashed') +
    facet_wrap(~lab)

  p_li <- ggplot(plot.table_li, aes(x=rt, y=int)) +
    theme_classic() +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = PC[isoab_b != 100, rt_start_b], color = "red") +
    geom_vline(xintercept = PC[isoab_b != 100, rt_end_b], color = "red") +
    labs(x = "Retention time [sec]", y = "Intensity") +
    annotate("text",
             x = min(plot.table_li$rt) + (max(plot.table_li$rt) - min(plot.table_li$rt)) * 0.8,
             y = max(plot.table_li$int),
             label = "Benchmark",
             color="red") +
    geom_vline(xintercept = PC[isoab_b != 100, rt_start_ug], color = "blue", linetype='dashed') +
    geom_vline(xintercept = PC[isoab_b != 100, rt_end_ug], color = "blue", linetype='dashed') +
    annotate("text",
             x = min(plot.table_li$rt) + (max(plot.table_li$rt) - min(plot.table_li$rt)) * 0.8,
             y = 0.9*max(plot.table_li$int),
             label = "Non-targeted",
             color="blue") +
    facet_wrap(~lab)

  plotly::subplot(plotly::ggplotly(p_hi, dynamicTicks = TRUE), plotly::ggplotly(p_li, dynamicTicks = TRUE))  %>%
    plotly::layout(title = list(text = paste0(paste0(PC[isoab_b == 100, molecule_b], " | ",
                                                     PC[isoab_b == 100, adduct_b], " | ",
                                                     PC[isoab_b == 100, sample_name_b]),
                                              '<br>',
                                              '<sup>',
                                              "Chromatogram plotted as extracted in benchmark.",
                                              '</sup>')),
                   margin=list(t = 90))


}
