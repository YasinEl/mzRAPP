#' plot_vs_prediction
#'
#' @param PC
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_vs_prediction <- function(PC, x = ExpectedArea, y, col_by = molecule){
  pcp <- PC[isoab < 100 & isoab_ol == FALSE & Iso_count > 1]


  p <- ggplot2::ggplot(pcp,
                       aes(x=!!enquo(x), y= !!enquo(y), molecule = molecule, isoab = isoab, adduct = adduct, FileName = FileName, IDX = IDX, ErrorRel_H = ErrorRel_H), se = TRUE) +
    theme_bw() +
    geom_point(aes(col=!!enquo(col_by)), size=3) +
    ggtitle(paste0("# of Peaks: ", nrow(pcp))) +
    theme(legend.position="None",
          plot.title = element_text(size=20, face="bold", hjust = 0.5),
          axis.text=element_text(size=13),
          axis.text.x=element_text(size=13),
          axis.title=element_text(size=17,face="bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


  pp <- plotly::ggplotly(p, dynamicTicks = TRUE, tooltip = c("IDX", "molecule", "adduct", "isoab", "FileName", "ErrorRel_H"))
  plotly::hide_legend(pp)
}
