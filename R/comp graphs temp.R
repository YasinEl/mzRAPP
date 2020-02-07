######
#Calculate different graphs
#####################

#EIC_plot
observeEvent({input$mol_c; input$add_c; input$ia_c},{
  comp <- comparison()
  req(comp)
  comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)

  if(nrow(comp.dt[molecule_b == input$mol_c & adduct_b == input$add_c & round(isoabb_b, 2) == input$ia_c]) > 0){

    p <- suppressWarnings(
      plot_Peak_per_mol(
        comp.dt,
        mol = input$mol_c,
        add = input$add_c,
        ia = input$ia_c
      )
    )

    output$graph_area_4 <- renderPlotly(p)
  }
}, ignoreInit = FALSE)


#Scatter_plot
observe({
  #observeEvent(c(input$overview_plot_input_x, input$overview_plot_input_y),{
  comp <- comparison()
  req(comp)
  f_nf_plot <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)

  f_nf_plot <- f_nf_plot[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

  f_nf_plot <- suppressWarnings(f_nf_plot[order(as.numeric(f_nf_plot$f_nf_col), decreasing = TRUE),])

  x = input$overview_plot_input_x
  y = input$overview_plot_input_y

  suppressWarnings(
    p <- ggplot() +
      geom_point(data = f_nf_plot[f_nf_col == TRUE], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                         y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                         col = "F",
                                                         molecule = molecule_b,
                                                         adduct = adduct_b,
                                                         isoabb = round(isoabb_b, 2),
                                                         sample_name = sample_name_b),
                 color = "blue") +

      geom_point(data = f_nf_plot[f_nf_col == FALSE], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                          y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                          col = "NF",
                                                          molecule = molecule_b,
                                                          adduct = adduct_b,
                                                          isoabb = isoabb_b,
                                                          sample_name = sample_name_b),
                 color = "red") +

      labs(x = if(x != "peak_height_b" & x != "peak_area_b") {names(choice_vector_comp)[choice_vector_comp == x]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == x], ")")},
           y = if(y != "peak_height_b" & y != "peak_area_b") {names(choice_vector_comp)[choice_vector_comp == y]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == y], ")")}) +
      ggtitle("Overview of found/not found peaks and their variables")
  )
  output$overview_plot <- renderPlotly(plotly::ggplotly(p, tooltip = c("molecule", "adduct", "isoabb", "sample_name"), dynamicTicks = TRUE))
})

#Missing value heatmap
observe({
  comparison <- comparison()

  req(comparison)

  hm_dt <- rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)

  fwrite(hm_dt, 'debug_r_s.csv')

  hm_dt[, missing_peaks := find_r_s_error(
    comp_id_b,
    molecule_b,
    adduct_b,
    sample_id_b,
    isoabb_b,
    peak_area_b,
    peak_area_ug,
    peak_height_b
  ), by = .(molecule_b, adduct_b, isoabb_b)]



  hm_dt <- (hm_dt[, main_feature_check := ifelse((length(unique(na.omit(feature_id_g))) == 1) &
                                                   (isoabb_b == 100), 'TRUE', 'TRUE'),
                  by = .(molecule_b, adduct_b, isoabb_b)])
  hm_dt <- hm_dt[, overgroup := paste0(molecule_b, adduct_b)]
  hm_dt <- hm_dt[, if (any(missing_peaks != 'F')) .SD, by = .(molecule_b, adduct_b, isoabb_b)]
  hm_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]


  plot_r_s <- ggplot(
    hm_dt,
    aes(
      x = as.character(plot_group),
      y = sample_id_b,
      fill = missing_peaks,
      molecule = molecule_b,
      mz = mz_acc_b,
      isoabb = isoabb_b,
      adduct = adduct_b,
      FileName = sample_name_b
    )
  ) +
    geom_tile() +
    scale_fill_manual(values=c("forestgreen", "firebrick", "royalblue4", "mediumpurple1")) +
    ggtitle("Missing peaks") +
    labs(x = "benchmark features", y = "sample IDs", fill = "b_peaks") +
    theme(legend.title = element_blank())

  output$graph_area_1 <-
    renderPlotly(plotly::ggplotly(
      plot_r_s,
      tooltip = c("molecule", "adduct", "isoabb", "FileName", "mz")
    ))



  ####################################
  ########Alignment Errors table######
  ####################################

  testDT <-
    rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)

  #if(length(unique(testDT$peak_area_g)) != 1){
  #testDT <-
  #  testDT[, main_feature := as.numeric(names(which.max(table(feature_id_g)))), by =
  #           .(molecule_b, adduct_b, isoabb_b)]
  test <- testDT[, count_errors_max(.SD), .SDcols=c('molecule_b',
                                                    'adduct_b',
                                                    'main_peak',
                                                    'sample_id_b',
                                                    'isoabb_b',
                                                    'feature_id_g',
                                                    'peak_group_b',
                                                    'peak_area_g',
                                                    'peak_area_ug'),
                 by=.(molecule_b, adduct_b)]
  test <- setnames(test, c('V1', 'molecule_b', 'adduct_b'), c('errors', 'Molecule', 'Adduct'))
  output$error_count <- renderTable(test[errors > 0])


  #}










  ###############end alignment error table


  cov_dt <-
    comparison$c_table
  cov_dt <- na.omit(cov_dt, cols = c('peak_area_ug'))
  #cov_dt$main_peak <- rep(TRUE, nrow(cov_dt))
  #nf_dt <- rbindlist(list(comparison$nf_g))

  #View(nf_dt)




  ####wieder einblenden
  #cov_dt <- na.omit(cov_dt, cols = c('peak_area_ug'))
  #cov_dt <-
  #  cov_dt[, complete_group_set := ifelse(.N == max(samples_per_group_b, na.rm = TRUE), 'TRUE', 'FALSE'), by =
  #           .(molecule_b, adduct_b, isoabb_b, grp_b)]
  #cov_dt <- cov_dt[complete_group_set == 'TRUE']



  # cov_dt_plot_dt <-
  #    cov_dt[, .(benchmark_peaks = sd(peak_area_b, na.rm = TRUE) / mean(peak_area_b, na.rm = TRUE) * 100,
  #               UT_unaligned_peaks = sd(peak_area_ug, na.rm = TRUE) / mean(peak_area_ug, na.rm = TRUE) * 100
  #    ), by = .(molecule_b, adduct_b, isoabb_b, grp_b)]


  #  cov_dt_plot_dt[, diffH10PP := as.character(UT_unaligned_peaks - benchmark_peaks > 10)]

  # cov_dt_plot_dt[diffH10PP == "TRUE"]$diffH10PP <- "Inc. > 10%p"
  #  cov_dt_plot_dt[diffH10PP == "FALSE"]$diffH10PP <- "Inc. < 10%p"

  # cov_dt_plot_dt <-
  #    melt(
  #      cov_dt_plot_dt,
  #      id.vars = c('molecule_b', 'adduct_b', 'grp_b', 'isoabb_b', 'diffH10PP'),
  #      measure.vars = c('benchmark_peaks', 'UT_unaligned_peaks'),
  #      variable.name = 'data_type',
  #      value.name = 'CV'
  #    )

  #  cov_dt_plot_dt[, grp_col := paste0(molecule_b, adduct_b, grp_b, isoabb_b)]
  #########################################
  #experiment for ratio instead of cv

  newcols <- c("benchmark", "non_targeted")


  cov_dt <- na.omit(cov_dt, cols = c("peak_area_b", "peak_area_ug"))

  cov_dt <- cov_dt[main_peak == TRUE]



  DT_tmp <- cov_dt[isoabb_b != 100][cov_dt[isoabb_b == 100],
                                    on=.(sample_name_b, molecule_b, adduct_b),
                                    nomatch = 0L, allow.cartesian=TRUE][,(newcols) := .((peak_area_b / ((i.peak_area_b * isoabb_b) / 100) - 1) * 100,
                                                                                        (peak_area_ug / ((i.peak_area_ug * isoabb_b) / 100) - 1) * 100)]


  cov_dt_plot_dt <- merge(cov_dt, DT_tmp[,.(comp_id_b, benchmark, non_targeted)], by = 'comp_id_b', all.x = TRUE, allow.cartesian = TRUE)



  #cov_dt_plot_dt[, diffH10PP := as.character(abs(benchmark) < abs(non_targeted) &
  #                                             abs(non_targeted - benchmark) > 20)]


  cov_dt_plot_dt[, diffH20PP := as.character(abs(abs(benchmark) - abs(non_targeted)) > 10 &
                                               abs(non_targeted - benchmark) > 20 &
                                               abs(non_targeted) > 30)]



  cov_dt_plot_dt[diffH20PP == "TRUE"]$diffH20PP <- "Inc. > 20%p"
  cov_dt_plot_dt[diffH20PP == "FALSE"]$diffH20PP <- "Inc. < 20%p"

  cov_dt_plot_dt <-
    melt(
      cov_dt_plot_dt,
      id.vars = c('molecule_b', 'adduct_b', 'grp_b', 'isoabb_b', 'sample_name_b', 'diffH20PP'),
      measure.vars = c("benchmark", "non_targeted"),
      variable.name = 'data_type',
      value.name = 'Pred_error'
    )


  cov_dt_plot_dt[, grp_col := paste0(molecule_b, adduct_b, grp_b, isoabb_b, sample_name_b)]

  cov_dt_plot_dt <- na.omit(cov_dt_plot_dt, cols = "diffH20PP")


  ####################


  aplot_table_test <<- cov_dt_plot_dt

  plot_cov <- ggplot(cov_dt_plot_dt[isoabb_b < 100]) +
    suppressWarnings( geom_line(suppressWarnings( aes(x = data_type,
                                                      y = Pred_error,
                                                      group = paste(grp_col, diffH20PP),
                                                      color = diffH20PP,
                                                      molecule = molecule_b,
                                                      adduct = adduct_b,
                                                      isoabb = isoabb_b,
                                                      sample = sample_name_b,
                                                      #grp = grp_b,
                                                      diffH20PP = diffH20PP
    )), alpha = 0.3)) +
    scale_color_manual(name = "+ > 20%p", values=c("blue", "red")) +
    ggtitle("Quality of peak abundances") +
    labs(x = "", y = "IT pred error [%]") +
    theme(legend.title = element_blank())
  #plot_test <<- plot_cov

  output$graph_area_2 <- renderPlotly(ggplotly(plot_cov, tooltip = c("molecule", "adduct", "isoabb", "sample", "Pred_error")))#"grp")))

  peak_i <- generate_results_text(comparison = comparison)


  output$results_text <- renderText(paste0(peak_i,
                                           "     Missing values (S|R): ", nrow(hm_dt[missing_peaks == "S"]), "|", nrow(hm_dt[missing_peaks == "R"]),
                                           "     Pred. error increase >20%p: ", nrow(cov_dt_plot_dt[diffH20PP == "Inc. > 20%p"])/2, "/", nrow(cov_dt_plot_dt[!is.na(diffH20PP)]) / 2, " (",
                                           round(nrow(cov_dt_plot_dt[diffH20PP == "Inc. > 20%p"])/nrow(cov_dt_plot_dt) * 100, 1), "%)",
                                           "     Min. # of alignment errors: ", sum(test$errors, na.rm = TRUE)))







  observeEvent(input$graph_select_input, {

    req(comparison)

    f_nf_plot <-
      rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)
    f_nf_plot <-
      f_nf_plot[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

    #print(sapply(f_nf_plot[, input$graph_select_input], class))
    if(input$graph_select_input %in% c("molecule_b", "adduct_b", "Grp_b", "sample_name_b")){

      the_plot1 <-
        ggplot(f_nf_plot,
               aes_string(x = input$graph_select_input, fill = 'f_nf_col')) + stat_count(position =
                                                                                           'dodge') +
        scale_color_manual(name = "peaks found") +
        ggtitle("Distribution of found/not found peaks") +
        theme(axis.text.x = element_blank())


    } else{

      #calc binwidth
      #binwidth = 0.5

      var <- input$graph_select_input



      if(var == "peak_height_b" | var == "peak_area_b"){
        f_nf_plot[, var] <- log10(setDT(f_nf_plot)[, ..var])
      }


      binwidth <- (max(as.numeric(unlist(f_nf_plot[, ..var])), na.rm = TRUE) - min(as.numeric(unlist(f_nf_plot[, ..var])), na.rm = TRUE)) / 20


      # Count how many of each lab1 within each bin of var1
      df_bin <- f_nf_plot %>%
        count(var = floor(!! sym(var)/binwidth)*binwidth, f_nf_col)

      df_bin <- as.data.table(df_bin)
      df_bin <- df_bin[df_bin[, .(MAXn = max(n)), by = var], on = .(var)]
      df_bin <- df_bin[MAXn>=10]


      # Get "no" share within each bin
      df_sum <- df_bin %>%
        group_by(var) %>%
        summarize(no_pct = 100 * sum(n * (f_nf_col == "TRUE")) / sum(n))

      t <- plotly::ggplotly(
        ggplot2::ggplot() +
          geom_col(data = df_bin, aes(var, n, fill = f_nf_col),
                   position = position_dodge(preserve = "single")) +
          theme(legend.position = "none") +
          scale_fill_manual(values  = c("red", "blue")) +
          ggtitle("Distribution of found/not found peaks")
      )


      the_plot1 <- t %>% add_trace(x=~var,
                                   y =~no_pct,
                                   line = list(color = 'rgb(0, 0, 0)'),
                                   marker = list(color = 'rgb(0, 0, 0)'),
                                   yaxis = "y2",
                                   data = df_sum,
                                   showlegend = FALSE,
                                   inherit = FALSE,
                                   mode = 'lines+markers',
                                   type = "scatter")%>%
        layout(yaxis2 = list(
          #tickfont = list(size=11.7),
          titlefont=list(size=14.6),
          overlaying = "y",
          #nticks = 5,
          side = "right",
          title = "found peaks [%]"
        ),
        yaxis = list(title = "peak count",
                     titlefont=list(size=14.6)
        ),
        xaxis = list(title = names(choice_vector_comp)[choice_vector_comp == var],
                     titlefont=list(size=14.6)
        ),
        margin = list(r = 100),
        showlegend = FALSE
        )

    }
    output$graph_area_3 <- renderPlotly(plotly::ggplotly(the_plot1, dynamicTicks = TRUE))
  })
})


####Test Move
#observeEvent(input$start_compare, {
#  updateTabsetPanel(session = session, 'main_panel', selected = 'results_tab_peaks')
#})
#######



#observeEvent(input$generate_benchmark, {
#  updateTabsetPanel(session = session, 'main_panel', selected = 'benchmark_results')
#})

######MS DAIL ERROR
# observe({
#   comparison <- comparison()
#   print(comparison)
#   matched_dt <-
#     rbindlist(list(comparison$c_table), fill = TRUE)
#
#   if(length(unique(matched_dt$peak_area_g)) != 1){
#
#   matched_dt <- matched_dt[, c("molecule_b", "adduct_b", "isoabb_b", "peak_area_g", "feature_id_g", "sample_name_b", "sample_id_b")]
#
#   print(matched_dt)
#   fwrite(matched_dt, 'matched.csv')
#
#
#   if(!all(is.na(matched_dt$feature_id_g))){
#
#     print('111')
#     hm_split_plot_dt <- matched_dt[isoabb_b == 100 & !is.na(feature_id_g), .(ut_feature_nr = length(unique(feature_id_g))), by = .(molecule_b, adduct_b)]
#     print('222')
#     hm_split_plot_dt <- hm_split_plot_dt[ut_feature_nr == 1]
#     print('333')
#     hm_split_plot_dt <- matched_dt[hm_split_plot_dt, on = .(molecule_b, adduct_b)]
#     print('444')
#     hm_split_plot_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]
#     print('555')
#     hm_split_plot_dt <- hm_split_plot_dt[, reindexedFeatures_g := reIndexFeatures(feature_id_g), by = .(plot_group)]
#     print('666')
#     fwrite(hm_split_plot_dt, 'hm.csv')
#     hm_split_plot_dt <- hm_split_plot_dt[hm_split_plot_dt[, .(mainFeature_g = names(sort(table(feature_id_g), decreasing = TRUE))[1]), by = .(plot_group)], on = .(plot_group)]
#     print('777')
#     hm_split_plot_dt <- hm_split_plot_dt[, split_flag := (length(unique(reindexedFeatures_g)) > 1), by = .(plot_group)]
#     print('888')
#     hm_split_plot_dt <- hm_split_plot_dt[split_flag == TRUE]
#     print('999')
#
#     hm_split = ggplot(
#       hm_split_plot_dt,
#       aes(
#         x = as.character(plot_group),
#         y = as.character(sample_id_b),
#         fill = reindexedFeatures_g,
#         molecule = molecule_b,
#         isoabb = isoabb_b,
#         adduct = adduct_b,
#         FileName = sample_name_b
#       )
#     ) +
#       geom_tile() +
#       #scale_fill_manual(values=c("forestgreen", "firebrick", "royalblue4", "mediumpurple1")) +
#       ggtitle("Split features") +
#       labs(x = "benchmark features", y = "sample IDs", fill = "b_peaks") +
#       theme(legend.title = element_blank())
#
#
#  output$graph_hm_split <- renderPlotly(plotly::ggplotly(
#    hm_split,
#    tooltip = c("molecule", "adduct", "isoabb", "FileName"),
#    dynamicTicks = TRUE
#   ))
#
#  }
#   }
# })
######


#input buttons to be updated live
####
observe({
  comp <- comparison()
  req(comp)
  comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)
  updateSelectInput(session, 'mol_c', choices = as.character(unique(comp.dt$molecule_b)), selected = as.character(unique(comp.dt$molecule_b)[1]))
})

observeEvent(input$mol_c,{
  comp <- comparison()
  req(comp)
  comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)
  updateSelectInput(session, 'add_c', choices = unique(comp.dt[molecule_b == input$mol_c]$adduct_b))
})

observeEvent(c(input$mol_c, input$add_c),{
  comp <- comparison()
  req(comp)
  comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)
  updateSelectInput(session, 'ia_c', choices = sort(round(unique(comp.dt[molecule_b == input$mol_c & adduct_b == input$add_c]$isoabb_b), 2), decreasing = TRUE))
})
