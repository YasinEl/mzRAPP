
#' match_features_to_benchmark
#'
#' @param g_table g_table
#' @param b_table b_table
#' @param Matches_BM_NPPpeaks Matches_BM_NPPpeaks
#' @param Unmatched_BM_NPPpeaks Unmatched_BM_NPPpeaks
#'
#' @keywords internal

match_features_to_benchmark <- function(g_table,
                                        b_table,
                                        Matches_BM_NPPpeaks,
                                        Unmatched_BM_NPPpeaks){

  if(nrow(g_table) > 0){

    ff_table_dt <- pick_main_feature(feature_compare(b_table, g_table, Matches_BM_NPPpeaks[, c("feature_id_b", "feature_id_g")]))

    dt <- ff_table_dt#[main_feature == TRUE]
    #dt[, dpl := duplicated(dt, by = c("feature_id_b"))]
    #dt <- dt[dpl != TRUE][, !"dpl"]

    id.cols <- c("feature_id_b", "feature_id_g", "molecule_b", "isoab_b", "adduct_b",
                 "total_area_b", "min_mz_start", "max_mz_end", "min_rt_start",
                 "max_rt_end", "main_feature")

    dt_melt_b <- data.table::melt(dt,
                                  id.vars = id.cols,
                                  measure.vars = colnames(dt)[grepl(utils::glob2rx("sample_*_b"), colnames(dt))],
                                  value.name = "area_b",
                                  variable.name = "sample_id_b",
                                  variable.factor = FALSE)

    dt_melt_b[, sample_id_b := as.factor(substr(dt_melt_b$sample_id_b, 8, nchar(dt_melt_b$sample_id_b) - 2))]

    dt_melt_g <- data.table::melt(dt,
                                  id.vars = id.cols,
                                  measure.vars = colnames(dt)[grepl(utils::glob2rx("sample_*_g"), colnames(dt))],
                                  value.name = "area_g",
                                  variable.name = "sample_id_b",
                                  variable.factor = FALSE)

    dt_melt_g[, sample_id_b := as.factor(substr(dt_melt_g$sample_id_b, 8, nchar(dt_melt_g$sample_id_b) - 2))]


    dt_n <- dt_melt_g[dt_melt_b, on = colnames(dt_melt_b)[-length(dt_melt_b)]]

    tmp <- unique(data.table(sample_id_b = as.factor(Matches_BM_NPPpeaks[["sample_id_b"]]),
                             sample_name_b = Matches_BM_NPPpeaks[["sample_name_b"]]))

    Matches_BM_NPPpeaks_NPPfeatures <- dt_n[tmp, on = .(sample_id_b)]

    ug_info <- data.table::rbindlist(list(Matches_BM_NPPpeaks, Unmatched_BM_NPPpeaks), fill = TRUE, use.names = TRUE)


    Matches_BM_NPPpeaks_NPPfeatures <-
      Matches_BM_NPPpeaks_NPPfeatures[!is.na(area_b)][ug_info[, c("molecule_b",
                                                                  "adduct_b",
                                                                  "isoab_b",
                                                                  "sample_name_b",
                                                                  "peak_area_b",
                                                                  "peak_area_ug")],
                                                      on = .(molecule_b, adduct_b, isoab_b, sample_name_b)]

  } else {

    Matches_BM_NPPpeaks_NPPfeatures <- stats::setNames(data.table(matrix(nrow = 0, ncol = 15)), c("feature_id_b", "feature_id_g", "molecule_b", "isoab_b", "adduct_b",
                                                                                                  "total_area_b", "min_mz_start", "max_mz_end", "min_rt_start",
                                                                                                  "max_rt_end", "main_feature", "sample_id_b", "area_g", "area_b",
                                                                                                  "sample_name_b"))
  }

  return(Matches_BM_NPPpeaks_NPPfeatures)

}
