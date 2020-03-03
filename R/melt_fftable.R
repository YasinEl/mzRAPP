#' melt_fftable
#'
#' @param fftable
#'
#' @return
#' @export
#'
#' @examples
melt_fftable <- function(comparison_data){

  dt <-  rbindlist(list(comparison_data$ff_table), fill = TRUE)
  #dt <- fftable

  dt <- dt[main_feature == TRUE]

  dt_melt_b <- melt(dt,
                    id.vars = colnames(dt)[1:11],
                    measure.vars = colnames(dt)[grepl(glob2rx("sample_*_b"), colnames(dt))],
                    value.name = "area_b",
                    variable.name = "sample_id_b",
                    variable.factor = FALSE)

  dt_melt_b$sample_id_b <-  substr(dt_melt_b$sample_id_b, 8, nchar(dt_melt_b$sample_id_b) - 2)

  dt_melt_g <- melt(dt,
                    id.vars = colnames(dt)[1:11],
                    measure.vars = colnames(dt)[grepl(glob2rx("sample_*_g"), colnames(dt))],
                    value.name = "area_g",
                    variable.name = "sample_id_b",
                    variable.factor = FALSE)

  dt_melt_g$sample_id_b <-  substr(dt_melt_g$sample_id_b, 8, nchar(dt_melt_g$sample_id_b) - 2)

  dt_n <- dt_melt_g[dt_melt_b, on = colnames(dt_melt_b)[-length(dt_melt_b)]]

  tmp <- unique(data.table(sample_id_b = as.factor(ev_return_list[["c_table"]][["sample_id_b"]]),
                           sample_name_b = ev_return_list[["c_table"]][["sample_name_b"]]))
  dt_n <- dt_n[tmp, on = .(sample_id_b)]



  return(dt_n)

}
