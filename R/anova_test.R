#' generate_vulcano_data
#'
#' @param nf_b_table
#' @param c_table
#'
#' @return
#' @export
#'
#' @examples
generate_vulcano_data <- function(c_table, nf_b_table){


  anova_dt <- rbindlist(list(c_table, nf_b_table), fill=TRUE)
  anova_dt <- anova_dt[, c("molecule_b", "adduct_b", "isoab_b", "Grp_b", "sample_name_b", "peak_area_b", "peak_area_ug", "peak_area_g")]
  anova_dt <- anova_dt[, anova_group := paste(molecule_b, adduct_b, isoab_b, sep='___')]





  anova_dt <-
  anova_dt[, .(liste = list(

    aov_b = {



    if(length(unique(Grp_b[!is.na(peak_area_b)]))>1){

      tmp <- aov(peak_area_b[!is.na(peak_area_b)] ~ as.factor(Grp_b[!is.na(peak_area_b)]))


      if(length(unique(Grp_b[!is.na(peak_area_b)]))>2){
        data.table(TukeyHSD(tmp)$`as.factor(Grp_b[!is.na(peak_area_b)])`,
                        keep.rownames = TRUE)[, c("rn", "p adj")][, c("test", "abu", "identity", "fc") := .("tukeyhsd", "benchmark", anova_group,
                                                                                                            {
                                                                                                              grps <- strsplit(rn, split = "-")[[1]]
                                                                                                              mean(peak_area_b[Grp_b == grps[1]], na.rm = TRUE)/
                                                                                                                mean(peak_area_b[Grp_b == grps[2]], na.rm = TRUE)
                                                                                                            })]
      } else{
        data.table(rn = paste0(tmp[["xlevels"]][["as.factor(Grp_b[!is.na(peak_area_b)])"]], collapse = "-"),
                        `p adj` = summary(tmp)[[1]][["Pr(>F)"]][1],
                   test = "t-test",
                   abu = "benchmark",
                   identity = anova_group)[, fc := {
                     grps <- strsplit(rn, split = "-")[[1]]
                     mean(peak_area_b[Grp_b == grps[1]], na.rm = TRUE)/
                       mean(peak_area_b[Grp_b == grps[2]], na.rm = TRUE)
                   }]
      }
    } else{data.table(rn = NA, `p adj` = NA, identity = anova_group)}},


  aov_ug = if(length(unique(Grp_b[!is.na(peak_area_ug)]))>1){

    tmp <- aov(peak_area_ug[!is.na(peak_area_ug)] ~ as.factor(Grp_b[!is.na(peak_area_ug)]))

    if(length(unique(Grp_b[!is.na(peak_area_ug)]))>2){
      data.table(TukeyHSD(tmp)$`as.factor(Grp_b[!is.na(peak_area_ug)])`,
                 keep.rownames = TRUE)[, c("rn", "p adj")][, c("test", "abu", "identity", "fc") := .("tukeyhsd", "NPP_ug", anova_group,
                                                                                               {
                                                                                                 grps <- strsplit(rn, split = "-")[[1]]
                                                                                                 mean(peak_area_ug[Grp_b == grps[1]], na.rm = TRUE)/
                                                                                                   mean(peak_area_ug[Grp_b == grps[2]], na.rm = TRUE)
                                                                                               })]
    } else{
      data.table(rn = paste0(tmp[["xlevels"]][["as.factor(Grp_b[!is.na(peak_area_ug)])"]], collapse = "-"),
                 `p adj` = summary(tmp)[[1]][["Pr(>F)"]][1],
                 test = "t-test",
                 abu = "NPP_ug",
                 identity = anova_group)[, fc := {
                   grps <- strsplit(rn, split = "-")[[1]]
                   mean(peak_area_ug[Grp_b == grps[1]], na.rm = TRUE)/
                     mean(peak_area_ug[Grp_b == grps[2]], na.rm = TRUE)
                 }]
    }
  } else{data.table(rn = NA, `p adj` = NA, identity = anova_group)},



  aov_g = if(length(unique(Grp_b[!is.na(peak_area_g)]))>1){

    tmp <- aov(peak_area_g[!is.na(peak_area_g)] ~ as.factor(Grp_b[!is.na(peak_area_g)]))

    if(length(unique(Grp_b[!is.na(peak_area_g)]))>2){
      data.table(TukeyHSD(tmp)$`as.factor(Grp_b[!is.na(peak_area_g)])`,
                 keep.rownames = TRUE)[, c("rn", "p adj")][, c("test", "abu", "identity", "fc") := .("tukeyhsd", "NPP_g", anova_group,
                                                                                               {
                                                                                                 grps <- strsplit(rn, split = "-")[[1]]
                                                                                                 mean(peak_area_g[Grp_b == grps[1]], na.rm = TRUE)/
                                                                                                   mean(peak_area_g[Grp_b == grps[2]], na.rm = TRUE)
                                                                                               })]
    } else{
      data.table(rn = paste0(tmp[["xlevels"]][["as.factor(Grp_b[!is.na(peak_area_g)])"]], collapse = "-"),
                 `p adj` = summary(tmp)[[1]][["Pr(>F)"]][1],
                 test = "t-test",
                 abu = "NPP_g",
                 identity = anova_group)[, fc := {
                   grps <- strsplit(rn, split = "-")[[1]]
                   mean(peak_area_g[Grp_b == grps[1]], na.rm = TRUE)/
                     mean(peak_area_g[Grp_b == grps[2]], na.rm = TRUE)
                 }]
    }
  } else{data.table(rn = NA, `p adj` = NA, identity = anova_group)}




  )), keyby = anova_group]$liste








op <- rbindlist(anova_dt, fill = TRUE)

op <- op[!is.na(rn),!"test"]

op <- op[, c("molecule", "adduct", "isoab") := tstrsplit(identity, "___", fixed=TRUE)]



op2 <- dcast(op, rn + identity + molecule + adduct + isoab ~ abu, value.var = c("p adj", "fc"), fun = mean)


op2[, c("signi_b", "signi_ug", "signi_g") := .(sum(`p adj_benchmark` < 0.05) > 0, any(`p adj_NPP_ug` < 0.05), any(`p adj_NPP_g` < 0.05)),
    by = .(rn, molecule, adduct)]

op2[, fc_benchmark := as.numeric(fc_benchmark)]
op2[fc_benchmark < 1, fc_benchmark := -1/fc_benchmark]
op2[, fc_NPP_ug := as.numeric(fc_NPP_ug)]
op2[fc_NPP_ug < 1, fc_NPP_ug := -1/fc_NPP_ug]
op2[, fc_NPP_g := as.numeric(fc_NPP_g)]
op2[fc_NPP_g < 1, fc_NPP_g := -1/fc_NPP_g]
op2[, lost := FALSE]
op2[signi_b == TRUE & `p adj_benchmark` < 0.05 & (signi_ug == TRUE | signi_g == TRUE) & (`p adj_NPP_ug` > 0.05 | `p adj_NPP_g` > 0.05),
    lost := TRUE]

return(op2)

}




#' plot_vulcano
#'
#' @param vulcano_data
#'
#' @return
#' @export
#'
#' @examples
plot_vulcano <- function(vulcano_data){


op2 <- vulcano_data

op3 <-
  melt(
    op2,
    id.vars = c('molecule', 'adduct', 'isoab', 'rn', 'lost'),
    measure.vars = list(c("p adj_benchmark", "p adj_NPP_ug", "p adj_NPP_g"),
                        c("fc_benchmark", "fc_NPP_ug", "fc_NPP_g")),
    value.name = c("p_val", "fc")
    #variable.name = "test"
  )

op3$variable <- c("benchmark", "peak_picking", "aligned")[op3$variable]


suppressWarnings(
ggplotly(
  ggplot() +
    geom_point(data = op3[variable != "peak_picking" & lost == FALSE], aes(x = ifelse(fc < 0,
                                                                                      -log2(as.numeric(-fc)),
                                                                                      log2(as.numeric(fc))),
                                                                           y = -log10(as.numeric(p_val)),
                                                                           shape = variable,
                                                                           color = lost,
                                                                           molecule = molecule,
                                                                           adduct = adduct,
                                                                           isoab = round(as.numeric(isoab),2),
                                                                           Comparison = rn)) +
    geom_segment(data = op2, aes(x = ifelse(fc_benchmark < 0,
                                            -log2(as.numeric(-fc_benchmark)),
                                            log2(as.numeric(fc_benchmark))),
                                 xend = ifelse(fc_NPP_g < 0,
                                               -log2(as.numeric(-fc_NPP_g)),
                                               log2(as.numeric(fc_NPP_g))),
                                 y = -log10(as.numeric(`p adj_benchmark`)),
                                 yend = -log10(as.numeric(`p adj_NPP_g`)), col = lost)) +
    geom_point(data = op3[variable != "peak_picking" & lost == TRUE], aes(x = ifelse(fc < 0,
                                                                                     -log2(as.numeric(-fc)),
                                                                                     log2(as.numeric(fc))),
                                                                          y = -log10(as.numeric(p_val)),
                                                                          shape = variable,
                                                                          color = lost,
                                                                          molecule = molecule,
                                                                          adduct = adduct,
                                                                          isoab = round(as.numeric(isoab),2),
                                                                          Comparison = rn)) +

    geom_hline(yintercept = -log10(0.05)) +
    geom_vline(xintercept = log2(2)) +
    geom_vline(xintercept = -log2(2)) +
    xlab("log2(fold change)") +
    ylab("-log10(p value)"),
  tooltip = c("molecule", "adduct", "isoab", "Comparison"), dynamicTicks = TRUE
)
)

}




