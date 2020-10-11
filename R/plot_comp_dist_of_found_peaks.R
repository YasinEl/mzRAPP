#' plot_comp_dist_of_found_peaks
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#' @param var variable (column name) to be plotted
#' @param choice_vector_comp named vector containing variable to be plotted as element
#' @param post_alignment TRUE/FALSE should data be plotted from before or after alignment.
#'
#' @return plotly object
#' @export
#'
plot_comp_dist_of_found_peaks <- function(comparison_data, var, choice_vector_comp, post_alignment = FALSE){
  if(missing(var) | missing(comparison_data)) return(plotly::ggplotly(ggplot() + ggtitle("Missing arguments")))

  if(missing(choice_vector_comp)){
    choice_vector_comp <- NULL
  }

  if(post_alignment == TRUE){

    feat_t <- comparison_data[["Matches_BM_NPPpeaks_NPPfeatures"]]
    feat_t <- feat_t[main_feature == TRUE & !is.na(area_b)]
    BM_bu <- rbindlist(list(comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE], comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)
    BM_bu$sample_id_b <- as.factor(BM_bu$sample_id_b)
    feat_t <- feat_t[main_feature == TRUE]
    vct <- colnames(BM_bu)[grepl("_b", colnames(BM_bu))]
    f_nf_dt <- feat_t[!is.na(area_b) &
                        main_feature == TRUE, c("molecule_b",
                                                "adduct_b",
                                                "isoab_b",
                                                "sample_id_b",
                                                "area_g")][BM_bu[,..vct], on = .(molecule_b,
                                                                                 adduct_b,
                                                                                 isoab_b,
                                                                                 sample_id_b)]

    f_nf_plot <- f_nf_dt[, f_nf_col := ifelse(!is.na(area_g), 'TRUE', 'FALSE')]



  } else if(post_alignment == FALSE){

    f_nf_dt <-  rbindlist(list(comparison_data$Matches_BM_NPPpeaks, comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)
    f_nf_plot <- f_nf_dt[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

  }

  if(is.character(unlist(f_nf_plot[,..var]))){

    f_nf_plot <- cbind(f_nf_plot[, "f_nf_col"], f_nf_plot[, ..var])
    df_tmp <- f_nf_plot
    colnames(df_tmp)[2] <- "var_r"
    df_tmp <- df_tmp[, .N, by = .(var_r, f_nf_col)]
    df_sum <- df_tmp



    df_tmp$dpl <- duplicated(df_tmp$var_r)
    compl <- df_tmp$var_r[df_tmp$dpl]
    uncompl <- df_tmp[!var_r %in% compl]
    dt <- data.table(var_r = uncompl$var_r,
                     f_nf_col = !as.logical(df_tmp[!var_r %in% compl]$f_nf_col),
                     N = rep(0, length(uncompl$var_r)))
    f_nf_plot <- rbind(df_sum, dt)
    colnames(f_nf_plot)[colnames(f_nf_plot) == "var_r"] <- var

    plot_dist <-
        ggplot() +
      theme_classic() +
          geom_col(data = f_nf_plot, aes(get(var), N, fill = f_nf_col),
                   position = position_dodge(preserve = "single")) +
          theme(legend.position = "none") +
          scale_fill_manual(values  = c(`FALSE` =  "#ccd1d1", `TRUE` = "#82e0aa")) +
          ggtitle("Distribution of found/not found peaks") +
      xlab(var) + ylab("peak count")



  } else{

    if(var == "peak_height_b" | var == "peak_area_b"){
      f_nf_plot[, var] <- log10(setDT(f_nf_plot)[, ..var])
    }


    binwidth <- (max(as.double(unlist(f_nf_plot[, ..var])), na.rm = TRUE) - min(as.double(unlist(f_nf_plot[, ..var])), na.rm = TRUE)) / 20

    # Count how many of each lab1 within each bin of var1
    df_bin <- f_nf_plot %>%
      count(var = floor(!! sym(var)/binwidth)*binwidth, f_nf_col)
    df_bin_vct <- sort(df_bin$var)
    df_bin <- as.data.table(df_bin)
    df_bin <- na.omit(df_bin)
    df_bin <- df_bin[df_bin[, .(MAXn = max(n)), by = var], on = .(var)]

    #add zeros
    df_tmp <- df_bin

    df_tmp$dpl <- duplicated(df_tmp$var)
    compl <- as.double(df_tmp$var[df_tmp$dpl])
    uncompl <- df_tmp[!var %in% compl]
    uncompl$var <- as.double(uncompl$var)

    if(nrow(uncompl) > 0) {
      subst <-
        apply(uncompl, 1, function(x){

          c(var = as.double(unname(x[1])), f_nf_col = as.logical(unname(x[2]) == FALSE), n = 0L, MAXn = as.integer(unname(x[4])))

        })
      subst <- as.data.table(subst, keep.rownames = TRUE)
      subst <- dcast(melt(subst, id.vars = "rn"), variable ~ rn)[, -1]
      subst$f_nf_col <- as.logical(subst$f_nf_col)
      df_bin <- rbind(df_bin, subst, use.names = TRUE, fill = FALSE)
    }

      df_bin$var <- as.double(df_bin$var)
      df_bin$f_nf_col <- as.logical(df_bin$f_nf_col)
      df_bin <- df_bin[order(rank(var))]
      df_bin$var <- round(unlist(lapply(unique(df_bin_vct), function(x){return(c(round(x,5),round(x,5)))})),5)
    # Get "no" share within each bin
    df_sum <- df_bin %>%
      group_by(var) %>%
      summarize(no_pct = 100 * sum(n * (f_nf_col == "TRUE")) / sum(n))

    t <- plotly::ggplotly(
      ggplot() +
        geom_col(data = df_bin, aes(var, n, fill = f_nf_col),
                 position = position_dodge(preserve = "single"),
                 orientation = "x") +
        theme_classic() +
        theme(legend.position = "none") +
        scale_fill_manual(values  = c(`FALSE` =  "#ccd1d1", `TRUE` = "#82e0aa")) +
        ggtitle("Distribution of found/not found peaks")
    )
    plot_dist <- t %>% plotly::add_trace(x=~var,
                                 y =~no_pct,
                                 line = list(color = 'rgb(0, 0, 0)'),
                                 marker = list(color = 'rgb(0, 0, 0)'),
                                 yaxis = "y2",
                                 data = df_sum,
                                 showlegend = FALSE,
                                 inherit = FALSE,
                                 mode = 'lines+markers',
                                 type = "scatter")%>%
      plotly::layout(yaxis2 = list(
        titlefont=list(size=14.6),
        overlaying = "y",
        side = "right",
        title = "found peaks [%]"
      ),
      yaxis = list(title = "peak count",
                   titlefont=list(size=14.6)
      ),
      xaxis = list(title = if(is.null(choice_vector_comp)){ as.character(var)} else {names(choice_vector_comp)[choice_vector_comp == var]}, #names(choice_vector_comp)[choice_vector_comp == var],
                   titlefont=list(size=14.6)
      ),
      margin = list(r = 100),
      showlegend = FALSE
      )

  }
  return(plotly::ggplotly(plot_dist, dynamicTicks = TRUE))
}
