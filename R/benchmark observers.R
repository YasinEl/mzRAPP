observe({
  benchmark <- isolate(benchmark_data())
  benchmark <- benchmark$PCal


  var = input$bench_plotHisto


  suppressWarnings(
    if(!(var %in% c("molecule", "FileName", "Grp", "adduct"))){
      p <- ggplot() +
        geom_histogram(data = benchmark[!is.na(get(var))], aes(get(var)), bins = 30) +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var])

    } else{
      p <- ggplot() +
        geom_bar(data = benchmark[!is.na(get(var))], aes(as.character(get(var)))) +
        ggtitle("Overview - Histogram") +
        xlab(names(choice_vector_bench)[choice_vector_bench == var])

    }
  )


  output$bench_plotHisto <- renderPlotly(plotly::ggplotly(p, dynamicTicks = TRUE))


  output$graph_area_bench_3 <-
    renderPlotly(plot_Peak_with_predicted_peak(benchmark,
                                               IndexNumber = input$index_number))

})

observe({
  benchmark_set <- isolate(benchmark_data())
  benchmark_all <- benchmark_set$PCal
  benchmark <- unique(benchmark_all[!is.na(peaks.PpP) & isoabb == 100, c("molecule", "FileName", "isoabb")], by = c("molecule", "FileName"))
  benchmark_files <- benchmark_set$files
  benchmark_targets <- benchmark_set$targets
  benchmark_targets <- unique(benchmark_targets[, "molecule"])



  files.dt <- data.table(FileName = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(benchmark_files)))
  files.dt[,fileIdx:= seq(nrow(files.dt))]
  benchmark_targets$fileIdx <- rep(1, nrow(benchmark_targets))
  benchmark_targets <- benchmark_targets[files.dt, on=.(fileIdx<=fileIdx), allow.cartesian = TRUE]

  plot.dt <- benchmark[benchmark_targets, on = .(molecule, FileName), nomatch = NA]

  plot.dt$Found <- !is.na(plot.dt$isoabb)


  plot.dt <- plot.dt[Found == TRUE, .(nr = .N), by = .(molecule, Found)][, !"Found"][plot.dt, on =.(molecule), nomatch = NA]
  plot.dt[is.na(nr)]$nr <- 0

  plot_hm <- ggplot(
    plot.dt,
    aes(
      x = reorder(as.character(molecule), nr),
      y = as.character(FileName),
      fill = as.factor(Found),
      molecule = molecule,
      FileName = FileName
    )
  ) +
    geom_tile() +
    scale_fill_manual(values=c("firebrick", "forestgreen")) +
    ggtitle("Found/not found compounds per sample") +
    labs(x = "molecule", y = "file name", fill = "Found")# +
  #theme(axis.text.x = element_text(angle = 45))
  #    theme(legend.title = element_blank())
  #
  output$graph_area_bench_hm <-
    renderPlotly(plotly::ggplotly(
      plot_hm,
      width = 1000,
      tooltip = c("molecule", "FileName")
    ))

  output$results_text_b <- renderText(paste0("# of benchmark peaks: ", nrow(benchmark_all), "       # of compounds: " , length(unique(benchmark_all$molecule)),
                                             "       median FWHM [s]: ", round(median(benchmark_all$peaks.FW50M, na.rm = TRUE), 1), "       median # points per peak: ",
                                             median(benchmark_all$peaks.PpP), "       median mz accuracy [ppm]: ",
                                             round(median(benchmark_all$peaks.mz_accuracy_ppm), 1)))

})
