#' plot_comp_dist_of_found_peaks
#'
#' @param comparison_data
#' @param var
#'
#' @return
#' @export
#'
#' @examples
plot_comp_dist_of_found_peaks <- function(comparison_data, var, choice_vector_comp, post_alignment = FALSE){


  if(post_alignment == TRUE){
    feat_t <- melt_fftable(comparison_data)

    BM_bu <- rbindlist(list(comparison_data$c_table[main_peak == TRUE], comparison_data$nf_b_table), fill = TRUE)

    BM_bu$sample_id_b <- as.factor(BM_bu$sample_id_b)

    feat_t <- feat_t[main_feature == TRUE]

    vct <- colnames(BM_bu)[grepl("_b", colnames(BM_bu))]

    f_nf_dt <- feat_t[!is.na(area_b) &
                        main_feature == TRUE, c("molecule_b",
                                                "adduct_b",
                                                "isoabb_b",
                                                "sample_id_b",
                                                "area_g")][BM_bu[,..vct], on = .(molecule_b,
                                                                                 adduct_b,
                                                                                 isoabb_b,
                                                                                 sample_id_b)]

    f_nf_plot <- f_nf_dt[, f_nf_col := ifelse(!is.na(area_g), 'TRUE', 'FALSE')]

    from_here <<- f_nf_plot


  } else if(post_alignment == FALSE){


    f_nf_dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)

    f_nf_plot <- f_nf_dt[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

  }


  if(var %in% c("molecule_b", "adduct_b", "Grp_b", "sample_name_b")){

    plot_dist <-
      ggplot(f_nf_plot,
             aes_string(x = var, fill = 'f_nf_col')) + stat_count(position =
                                                                                         'dodge') +
      scale_color_manual(name = "peaks found") +
      ggtitle("Distribution of found/not found peaks") +
      theme(axis.text.x = element_blank())


  } else{

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

    sum_thing <<-  df_sum
    bin_thing <<-  df_bin

    t <- plotly::ggplotly(
      ggplot2::ggplot() +
        geom_col(data = df_bin, aes(var, n, fill = f_nf_col),
                 position = position_dodge(preserve = "single")) +
        theme(legend.position = "none") +
        scale_fill_manual(values  = c("red", "blue")) +
        ggtitle("Distribution of found/not found peaks")
    )


    plot_dist <- t %>% add_trace(x=~var,
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
  return(plotly::ggplotly(plot_dist, dynamicTicks = TRUE))
}
