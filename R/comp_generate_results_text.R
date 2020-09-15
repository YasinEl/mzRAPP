#' generate_results_text
#'
#'
#'
#' @description in order to estimate confidence intervals boots trapping is performed on benchmark molecules with R=1000 using \code{\link{boot.ci}} with type="basic".
#'
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#'
#'
#' @return returns a list containing containing different performance metrics of non-targeted data pre-processing.
#' @export
#'
generate_results_text <- function(comparison_data){

  UT_peaks <-
    nrow(rbindlist(list(comparison_data$c_table[main_peak == TRUE], comparison_data$nf_g), fill = TRUE))




  bm_tab <- rbindlist(list(comparison_data$c_table[main_peak == TRUE], comparison_data$nf_b_table), fill = TRUE, use.names = TRUE)
  bm_tab[is.na(main_peak), main_peak := FALSE]


  peaks_pp <- bm_tab[,.(Found_peaks_pp = sum(main_peak),
                        Not_Found_peaks_pp = length(main_peak) - sum(main_peak)), by = .(molecule_b)]

  peaks_ft <- comparison_data$feature_table[!is.na(peak_area_b) & !is.na(main_feature) && main_feature == TRUE, .(Found_peaks_ft = sum(!is.na(area_g)),
                                                                   Not_Found_peaks_ft = sum(is.na(area_g))),
                                            by = .(molecule_b)]

  IRb <- comparison_data$iso_err_dt[, c("molecule_b", "diffH20PP_pp", "diffH20PP_ft")][, .(IRb_ok_pp = sum(diffH20PP_pp == "Inc. < 20%p", na.rm = TRUE),
                                                                                           IRb_off_pp = sum(diffH20PP_pp == "Inc. > 20%p", na.rm = TRUE),
                                                                                           IRb_ok_ft = sum(diffH20PP_ft == "Inc. < 20%p", na.rm = TRUE),
                                                                                           IRb_off_ft = sum(diffH20PP_ft == "Inc. > 20%p", na.rm = TRUE)),
                                                                                       by = .(molecule_b)]



  split_pp <- comparison_data$split_table[,c("molecule_b")][,.( Split_peaks = .N), by = .(molecule_b)]

  mw_tab <- comparison_data$rs_table[, .(R_pp = sum(missing_peaks_ug == "R", na.rm = TRUE),
                                         S_pp = sum(missing_peaks_ug == "S", na.rm = TRUE),
                                         R_ft = sum(missing_peaks_g == "R", na.rm = TRUE),
                                         S_ft = sum(missing_peaks_g == "S", na.rm = TRUE)),
                                     by = .(molecule_b)]


  ali_tab <- comparison_data$ali_error_table[, .(Min.er = sum(Min.errors, na.rm = TRUE),
                                                 BM.div = sum(BM.div, na.rm = TRUE),
                                                 lost = sum(Lost_b.A, na.rm = TRUE)),
                                             by = .(Molecule)]

  colnames(ali_tab)[1] <- "molecule_b"



  sum_tab <- unique(bm_tab[!is.na(molecule_b), "molecule_b"])
  sum_tab <- peaks_pp[sum_tab, on = .(molecule_b)]
  sum_tab <- peaks_ft[sum_tab, on = .(molecule_b)]
  sum_tab <- split_pp[sum_tab, on = .(molecule_b)]
  sum_tab <- IRb[sum_tab, on = .(molecule_b)]
  sum_tab <- mw_tab[sum_tab, on = .(molecule_b)]
  sum_tab <- ali_tab[sum_tab, on = .(molecule_b)]



  main_peak_table <- comparison_data$c_table[main_peak == TRUE]

  found_ug_peaks <- nrow(main_peak_table)#length(unique(main_peak_table$comp_id_ug))
  #split_features <- sum(main_peak_table[, count_split_features(feature_id_g), by=feature_id_b]$V1, na.rm=TRUE)


  if(nrow(comparison_data$rs_table[missing_peaks_ug == "S" | missing_peaks_ug == "R"]) == 0) {
    bv_Missing_peaks <- rep(FALSE, 10)
  } else {
    bv_Missing_peaks <- c(rep(TRUE, nrow(comparison_data$rs_table[missing_peaks_ug == "R"])),
                          rep(FALSE, nrow(comparison_data$rs_table[missing_peaks_ug == "S"])))

  }


  if(nrow(comparison_data$rs_table[missing_peaks_g == "S" | missing_peaks_g == "R"]) == 0) {
    bv_Missing_peaks_g <- rep(FALSE, 10)
  } else {
    bv_Missing_peaks_g <- c(rep(TRUE, nrow(comparison_data$rs_table[missing_peaks_g == "R"])),
                            rep(FALSE, nrow(comparison_data$rs_table[missing_peaks_g == "S"])))

  }


  results_text <- list(Assessed_tool = comparison_data$info_list$algorithm,
                       Benchmark = list(
                         BM_peaks = (sum(sum_tab$Found_peaks_pp, na.rm = TRUE) + sum(sum_tab$Not_Found_peaks_pp, na.rm = TRUE)),
                         Features = comparison_data$info_list$nr_of_b_features
                       ),
                       Before_alignment = list(
                         NT_peaks = UT_peaks,
                         Found_peaks = list(count = sum(sum_tab$Found_peaks_pp, na.rm = TRUE),
                                            CI = boot::boot.ci(boot::boot(sum_tab,
                                                                          function(data, indices){
                                                                            dt<-data[indices,]
                                                                            round(sum(dt$Found_peaks_pp, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE) + sum(dt$Not_Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                          },
                                                                          R = 1000),
                                                               index=1,
                                                               type='basic')$basic
                         ),
                         Split_peaks = list(count = sum(sum_tab$Split_peaks, na.rm = TRUE),
                                            CI = boot::boot.ci(boot::boot(sum_tab,
                                                                          function(data, indices){
                                                                            dt<-data[indices,]
                                                                            round(sum(dt$Split_peaks, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE) + sum(dt$Not_Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                          },
                                                                          R = 1000),
                                                               index=1,
                                                               type='basic')$basic
                         ),
                         Missing_peaks = list(
                           Systematic = sum(sum_tab$S_pp, na.rm = TRUE),
                           Random =  list(count = sum(sum_tab$R_pp, na.rm = TRUE),
                                          CI = if(sum(sum_tab$R_pp, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                        function(data, indices){
                                                                          dt<-data[indices,]
                                                                          round(sum(dt$R_pp, na.rm = TRUE)/(sum(dt$R_pp, na.rm = TRUE) + sum(dt$S_pp, na.rm = TRUE))*100,0)
                                                                        },
                                                                        R = 1000),
                                                             index=1,
                                                             type='basic')$basic} else {rep(0,5)}
                           )
                         ),
                         IR_quality = list(
                           Error_inc_below20pp = sum(sum_tab$IRb_ok_pp, na.rm = TRUE),
                           Error_inc_above20pp = list(count = sum(sum_tab$IRb_off_pp, na.rm = TRUE),
                                                      CI = boot::boot.ci(boot::boot(sum_tab,
                                                                                    function(data, indices){
                                                                                      dt<-data[indices,]
                                                                                      round(sum(dt$IRb_off_pp, na.rm = TRUE)/(sum(dt$IRb_off_pp, na.rm = TRUE) + sum(dt$IRb_ok_pp, na.rm = TRUE))*100,0)
                                                                                    },
                                                                                    R = 1000),
                                                                         index=1,
                                                                         type='basic')$basic
                           )
                         )
                       ),
                       Alignmnet = list(
                         Min.Errors = list(count = sum(sum_tab$Min.er, na.rm = TRUE),
                                           CI = boot::boot.ci(boot::boot(sum_tab,
                                                                         function(data, indices){
                                                                           dt<-data[indices,]
                                                                           round(sum(dt$Min.er, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                         },
                                                                         R = 1000),
                                                              index=1,
                                                              type='basic')$basic),
                         BM_divergences = list(count = sum(sum_tab$BM.div, na.rm = TRUE),
                                               CI = boot::boot.ci(boot::boot(sum_tab,
                                                                             function(data, indices){
                                                                               dt<-data[indices,]
                                                                               round(sum(dt$BM.div, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                             },
                                                                             R = 1000),
                                                                  index=1,
                                                                  type='basic')$basic),
                         Lost_b.A = list(count = sum(sum_tab$lost, na.rm = TRUE),
                                         CI = boot::boot.ci(boot::boot(sum_tab,
                                                                       function(data, indices){
                                                                         dt<-data[indices,]
                                                                         round(sum(dt$lost, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                       },
                                                                       R = 1000),
                                                            index=1,
                                                            type='basic')$basic)
                       ),
                       After_alignmnet = list(
                         Found_peaks = list(count = sum(sum_tab$Found_peaks_ft, na.rm = TRUE),
                                            CI =  boot::boot.ci(boot::boot(sum_tab,
                                                                           function(data, indices){
                                                                             dt<-data[indices,]
                                                                             round(sum(dt$Found_peaks_ft, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE) + sum(dt$Not_Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                           },
                                                                           R = 1000),
                                                                index=1,
                                                                type='basic')$basic
                         ),
                         Found_features = nrow(unique(comparison_data$feature_table[!is.na(area_b) &
                                                                                      !is.na(area_g) &
                                                                                      main_feature == TRUE,
                                                                                    c("molecule_b", "adduct_b", "isoab_b")], cols = c("molecule_b", "adduct_b", "isoab_b"))),
                         Missing_peaks = list(
                           Systematic = sum(sum_tab$S_ft, na.rm = TRUE),
                           Random =  list(count = sum(sum_tab$R_ft, na.rm = TRUE),
                                          CI = if(sum(sum_tab$R_ft, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                        function(data, indices){
                                                                          dt<-data[indices,]
                                                                          round(sum(dt$R_ft, na.rm = TRUE)/(sum(dt$R_ft, na.rm = TRUE) + sum(dt$S_ft, na.rm = TRUE))*100,0)
                                                                        },
                                                                        R = 1000),
                                                             index=1,
                                                             type='basic')$basic} else{rep(0,5)}
                           )
                         ),
                         IR_quality = list(
                           Error_inc_below20pp = sum(sum_tab$IRb_ok_ft, na.rm = TRUE),
                           Error_inc_above20pp = list(count = sum(sum_tab$IRb_off_ft, na.rm = TRUE),
                                                      CI = boot::boot.ci(boot::boot(sum_tab,
                                                                                    function(data, indices){
                                                                                      dt<-data[indices,]
                                                                                      round(sum(dt$IRb_off_ft, na.rm = TRUE)/(sum(dt$IRb_off_ft, na.rm = TRUE) + sum(dt$IRb_ok_ft, na.rm = TRUE))*100,0)
                                                                                    },
                                                                                    R = 1000),
                                                                         index=1,
                                                                         type='basic')$basic)
                         )
                       )
  )
  return(results_text)
}


