#' plot_comp_iso_pred_error
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#' @param post_alignment TRUE/FALSE should NT data from before or after alignment be plotted
#' @param BMvsPPvsAl TRUE/FALSE should argument post_alignment be ignored in order to plot both in one plot
#'
#' @return plotly object
#' @export
#'
plot_comp_iso_pred_error <- function(comparison_data, post_alignment = FALSE, BMvsPPvsAl = TRUE) {
#if(post_alignment == TRUE){

  #dt <-  rbindlist(list(comparison_data$ff_table), fill = TRUE)

 # dt <- melt_fftable(comparison_data)

 # dt <- dt[main_feature == TRUE]

 # iso_err_dt <- na.omit(dt, cols = c('area_g'))

 # iso_err_dt <- na.omit(iso_err_dt, cols = c("area_b", "area_g"))

  #iso_err_dt <- iso_err_dt[main_peak == TRUE]

#  iso_err_dt$id <- seq(1:nrow(iso_err_dt))

 # DT_tmp <- iso_err_dt[isoab_b != 100][iso_err_dt[isoab_b == 100],
#                                        on=.(sample_name_b, molecule_b, adduct_b),
#                                        nomatch = 0L, allow.cartesian=TRUE][,c("benchmark", "non_targeted") := .((area_b / ((i.area_b * isoab_b) / 100) - 1) * 100,
 #                                                                                                                (area_g / ((i.area_g * isoab_b) / 100) - 1) * 100)]


#  iso_err_dt <- merge(iso_err_dt, DT_tmp[,.(id, benchmark, non_targeted)], by = 'id', all.x = TRUE, allow.cartesian = TRUE)


#  iso_err_dt[, diffH20PP := as.character(abs(abs(benchmark) - abs(non_targeted)) > 10 &
#                                           abs(non_targeted - benchmark) > 20 &
#                                           abs(non_targeted) > 30)]

#  iso_err_dt[diffH20PP == "TRUE"]$diffH20PP <- "Inc. > 20%p"
#  iso_err_dt[diffH20PP == "FALSE"]$diffH20PP <- "Inc. < 20%p"

#  whatsin <<- iso_err_dt

#  iso_err_dt <-
#    melt(
#      iso_err_dt,
#      id.vars = c('molecule_b', 'adduct_b', 'isoab_b', 'sample_name_b', 'diffH20PP'),
#      measure.vars = c("benchmark", "non_targeted"),
#      variable.name = 'data_type',
#      value.name = 'Pred_error'
#    )


 # iso_err_dt[, grp_col := paste0(molecule_b, adduct_b, isoab_b, sample_name_b)]

#  iso_err_dt <- na.omit(iso_err_dt, cols = "diffH20PP")





#} else if(post_alignment == FALSE){

  iso_err_dt <- comparison_data$iso_err_dt

  if(nrow(iso_err_dt[!is.na(NPP_features)]) == 0) {
    BMvsPPvsAl <- FALSE
    }

  #iso_err_dt[diffH20PP_pp == "TRUE" , diffH20PP := "Inc. > 20%p"]
  #iso_err_dt[diffH20PP_pp == "FALSE" , diffH20PP := "Inc. < 20%p"]
  #iso_err_dt[diffH20PP_pp == "TRUE"]$diffH20PP <- "Inc. > 20%p"
  #iso_err_dt[diffH20PP_pp == "FALSE"]$diffH20PP <- "Inc. < 20%p"
  if(BMvsPPvsAl == FALSE){

  if(post_alignment == FALSE){
    iso_err_dt$diffH20PP <- iso_err_dt$diffH20PP_pp

    iso_err_dt <-
      melt(
        iso_err_dt,
        id.vars = c('molecule_b', 'adduct_b', 'Grp_b', 'isoab_b', 'sample_name_b', 'diffH20PP'),
        measure.vars = c("benchmark", "NPP_peak picking"),
        variable.name = 'data_type',
        value.name = 'Pred_error'
      )


  } else if(post_alignment == TRUE) {
    iso_err_dt$diffH20PP <- iso_err_dt$diffH20PP_ft

    iso_err_dt <-
      melt(
        iso_err_dt,
        id.vars = c('molecule_b', 'adduct_b', 'Grp_b', 'isoab_b', 'sample_name_b', 'diffH20PP'),
        measure.vars = c("benchmark", "NPP_features"),
        variable.name = 'data_type',
        value.name = 'Pred_error'
      )


  } else {stop("Argument post_alignment must be TRUE or FALSE!")}

  }else if(BMvsPPvsAl == TRUE){

    iso_err_dt$diffH20PP <- iso_err_dt$diffH20PP_ft
    iso_err_dt[diffH20PP_pp == "Inc. < 20%p" & (diffH20PP_ft == "Inc. > 20%p"), diffH20PP := "Feature Inc. > 20%p"]
    #iso_err_dt[diffH20PP_pp == "Inc. < 20%p" & (diffH20PP_ft == "Inc. > 20%p"), diffH20PP := "Feature Inc. > 20%p"]


    iso_err_dt <-
      melt(
        iso_err_dt,
        id.vars = c('molecule_b', 'adduct_b', 'isoab_b', 'sample_name_b', 'diffH20PP'),
        measure.vars = c("benchmark", "NPP_peak picking", "NPP_features"),
        variable.name = 'data_type',
        value.name = 'Pred_error'
      )



  }

  iso_err_dt[, grp_col := paste(molecule_b, adduct_b, isoab_b, sample_name_b, sep = "_;_")]

  iso_err_dt <- na.omit(iso_err_dt, cols = "Pred_error")

#} else {stop("Argument post_alignment must be TRUE or FALSE!")}

  p <- ggplot(iso_err_dt[isoab_b < 100]) +
    suppressWarnings( geom_line(suppressWarnings( aes(x = data_type,
                                                      y = Pred_error,
                                                      group = paste(grp_col, diffH20PP),
                                                      color = diffH20PP,
                                                      molecule = molecule_b,
                                                      adduct = adduct_b,
                                                      isoab = isoab_b,
                                                      sample = sample_name_b,
                                                      #grp = grp_b,
                                                      diffH20PP = diffH20PP,
                                                      key = grp_col
    )), alpha = 0.3)) +
    theme_classic() +
    #scale_color_manual(name = "+ > 20%p", values=c("blue", "red")) +
    scale_color_manual(name = "+ > 20%p", values=c(`Inc. < 20%p` = "blue", `Inc. > 20%p` = "red", `Feature Inc. > 20%p` = "goldenrod2")) +
    ggtitle("Relative IT ratio bias") +
    labs(x = "", y = "IT ratio bias [%]") +
    theme(legend.title = element_blank())


  return(plotly::ggplotly(p, tooltip = c("molecule", "adduct", "isoab", "sample", "Pred_error"), dynamicTicks = "y", source = "IRbias"))

}
