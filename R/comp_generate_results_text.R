#' derive_performance_metrics
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
derive_performance_metrics <- function(comparison_data){

  UT_peaks <-
    nrow(rbindlist(list(comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE], comparison_data$nf_g), fill = TRUE))




  sum_tab <- comparison_data$Overview_per_molecule

  #what <<- sum_tab

  main_peak_table <- comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE]

  found_ug_peaks <- nrow(main_peak_table)#length(unique(main_peak_table$comp_id_ug))
  #split_features <- sum(main_peak_table[, count_split_features(feature_id_g), by=feature_id_b]$V1, na.rm=TRUE)


 # if(nrow(comparison_data$MissingPeak_classification[missing_peaks_ug == "S" | missing_peaks_ug == "R"]) == 0) {
 #   bv_Missing_peaks <- rep(FALSE, 10)
  #} else {
  #  bv_Missing_peaks <- c(rep(TRUE, nrow(comparison_data$MissingPeak_classification[missing_peaks_ug == "R"])),
  #                        rep(FALSE, nrow(comparison_data$MissingPeak_classification[missing_peaks_ug == "S"])))
#
#  }


#  if(nrow(comparison_data$MissingPeak_classification[missing_peaks_g == "S" | missing_peaks_g == "R"]) == 0) {
#    bv_Missing_peaks_g <- rep(FALSE, 10)
#  } else {
#    bv_Missing_peaks_g <- c(rep(TRUE, nrow(comparison_data$MissingPeak_classification[missing_peaks_g == "R"])),
#                            rep(FALSE, nrow(comparison_data$MissingPeak_classification[missing_peaks_g == "S"])))

 # }


  results_text <- list(Assessed_tool = comparison_data$BM_NPPoutput_size$algorithm,
                       Benchmark = list(
                         BM_peaks = (sum(sum_tab$Found_peaks_pp, na.rm = TRUE) + sum(sum_tab$Not_Found_peaks_pp, na.rm = TRUE)),
                         Features = comparison_data$BM_NPPoutput_size$nr_of_b_features
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
                                                                          ret <- round(sum(dt$R_pp, na.rm = TRUE)/(sum(dt$R_pp, na.rm = TRUE) + sum(dt$S_pp, na.rm = TRUE))*100,0)
                                                                          if(is.nan(ret)){
                                                                            return(0)
                                                                          } else return(ret)
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
                                                                         ret <- round(sum(dt$lost, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,0)
                                                                         if(is.nan(ret)){
                                                                           return(0)
                                                                         } else return(ret)
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
                         Found_features = nrow(unique(comparison_data$Matches_BM_NPPpeaks_NPPfeatures[!is.na(area_b) &
                                                                                      !is.na(area_g) &
                                                                                      main_feature == TRUE,
                                                                                    c("molecule_b", "adduct_b", "isoab_b")], cols = c("molecule_b", "adduct_b", "isoab_b"))),
                         Missing_peaks = list(
                           Systematic = sum(sum_tab$S_ft, na.rm = TRUE),
                           Random =  list(count = sum(sum_tab$R_ft, na.rm = TRUE),
                                          CI = if(sum(sum_tab$R_ft, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                        function(data, indices){
                                                                          dt<-data[indices,]
                                                                          ret <- round(sum(dt$R_ft, na.rm = TRUE)/(sum(dt$R_ft, na.rm = TRUE) + sum(dt$S_ft, na.rm = TRUE))*100,0)
                                                                          if(is.nan(ret)){
                                                                            return(0)
                                                                          } else return(ret)
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
                                                                                      ret <- round(sum(dt$IRb_off_ft, na.rm = TRUE)/(sum(dt$IRb_off_ft, na.rm = TRUE) + sum(dt$IRb_ok_ft, na.rm = TRUE))*100,0)
                                                                                      if(is.nan(ret)){
                                                                                        return(0)
                                                                                      } else return(ret)
                                                                                    },
                                                                                    R = 1000),
                                                                         index=1,
                                                                         type='basic')$basic)
                         )
                       )
  )
  return(results_text)
}


