#' melt_fftable
#'
#' @param fftable fftable
#'
#'
#' @keywords internal
melt_fftable <- function(ff_table, Matches_BM_NPPpeaks){

  dt <-  ff_table
  #dt <- fftable

  #dt <- dt[main_feature == TRUE]

  id.cols <- c("feature_id_b", "feature_id_g", "molecule_b", "isoab_b", "adduct_b",
               "total_area_b", "min_mz_start", "max_mz_end", "min_rt_start",
               "max_rt_end", "main_feature")

  dt_melt_b <- melt(dt,
                    id.vars = id.cols,
                    measure.vars = colnames(dt)[grepl(glob2rx("sample_*_b"), colnames(dt))],
                    value.name = "area_b",
                    variable.name = "sample_id_b",
                    variable.factor = FALSE)

  dt_melt_b$sample_id_b <-  substr(dt_melt_b$sample_id_b, 8, nchar(dt_melt_b$sample_id_b) - 2)

  dt_melt_g <- melt(dt,
                    id.vars = id.cols,
                    measure.vars = colnames(dt)[grepl(glob2rx("sample_*_g"), colnames(dt))],
                    value.name = "area_g",
                    variable.name = "sample_id_b",
                    variable.factor = FALSE)

  dt_melt_g$sample_id_b <-  substr(dt_melt_g$sample_id_b, 8, nchar(dt_melt_g$sample_id_b) - 2)

  dt_n <- dt_melt_g[dt_melt_b, on = colnames(dt_melt_b)[-length(dt_melt_b)]]

  tmp <- unique(data.table(sample_id_b = as.factor(Matches_BM_NPPpeaks$sample_id_b),
                           sample_name_b = Matches_BM_NPPpeaks$sample_name_b))
  dt_n <- dt_n[tmp, on = .(sample_id_b)]



  return(dt_n)

}
