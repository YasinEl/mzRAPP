#' derive_performance_metrics
#'
#'
#'
#' @description Returns a list with counts, statistics and performance metrics. For each metric a count and for some also a confidence interval (CI)
#' as estimated via bootstrapping is reported. CIs are reported for percentages which should not only be true for the benchmark but also be an estimate for non-targeted
#' processing results. For more information please check the details section below. Background on the logic behind the different metrics is provided in the mzRAPP Readme
#' \url{https://github.com/YasinEl/mzRAPP#generation-and-interpretation-of-npp-performance-metrics}
#'
#' @param comparison_data output from \code{\link{compare_peaks}}
#'
#'
#' @details Bootstrapping is performed on benchmark molecules with R=1000 using \code{\link{boot.ci}} with type="basic" which returns a vector of
#' length = 5. The last two numbers of this vectors correspond to the upper and lower boundaries of the CI. For more information please check \code{\link{boot.ci}}.
#'
#' @details \strong{Benchmark:} information on the used benchmark (BM)
#' @details BM_peaks: number of BM peaks
#' @details Features: number of aligned BM features
#' @details \strong{Before_alignment:} Information on the non-targeted peak picking step. Ideas behind the different metrics are explained in the mzRAPP readme.
#' @details NT_peaks: NA
#' @details Found_peaks$count: Number of BM peaks for which a match was found in unaligned results.
#' @details Found_peaks$CI: CI for percentage of BM peaks for which a match was found.
#' @details Split_peaks$count: Number of split-peaks detected (as defined in mzRAPP readme)
#' @details Split_peaks$CI: CI for percentage of split-peaks from all matches (Found_peaks$count + Split_peaks$count)
#' @details Missing_peaks$Systematic: count of low missing peaks (as defined in mzRAPP readme)
#' @details Missing_peaks$Random$count: count of high missing peaks
#' @details Missing_peaks$Random$CI: CI for percentage of high missing peaks from all classifiable missing peaks (Missing_peaks$Systematic + Missing_peaks$Random$count)
#' @details IR_quality$Error_inc_below20pp: count for isotopologue ratios biases which did not increase by more than 20 %p. (as defined in mzRAPP readme)
#' @details IR_quality$Error_inc_above20pp$count: count for isotopologue ratios biases which did increase by more than 20 %p.
#' @details IR_quality$Error_inc_above20pp$CI: CI for percentage of all isotopologue ratios derivable from matched non-targeted peaks which did increase by more than 20 %p.
#' @details \strong{Alignment:} Information on the non-targeted alignment step. Ideas behind the different metrics are explained in the mzRAPP readme.
#' @details Min.Errors$count: Count of alignment errors
#' @details Min.Errors$CI: CI for percentage of alignment errors from all matched peaks (Found_peaks$count)
#' @details BM_divergences$count: count of divergences in alignment between the BM and the non-targeted output. (Min.Errors$count is a subgroup of this)
#' @details BM_divergences$CI: CI for percentage of benchmark divergences from all matched peaks (Found_peaks$count)
#' @details Lost_b.A$count: count of matched non-targeted peaks (Found_peaks$count) which were present in the unaligned, but not the aligned output.
#' @details Lost_b.A$CI: CI for percentage of not-found matched peaks from all matched peaks (Found_peaks$count)
#' @details \strong{After_alignment:} The same metrics calculated before alignment are also calculated here (with the exception of Split_peaks
#' which can not be derived from aligned results)
#'
#' @return returns a list containing different performance metrics of non-targeted data pre-processing.
#' @export
#'
derive_performance_metrics <- function(comparison_data){

  #set.seed(12987)

  UT_peaks <-
    nrow(data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE], comparison_data$nf_g), fill = TRUE))
  sum_tab <- comparison_data$Overview_per_molecule
  main_peak_table <- comparison_data$Matches_BM_NPPpeaks[main_peak == TRUE]

  found_ug_peaks <- nrow(main_peak_table)#length(unique(main_peak_table$comp_id_ug))

  results_text <- list(Assessed_tool = comparison_data$BM_NPPoutput_size$algorithm,
                       Benchmark = list(
                         BM_peaks = (sum(sum_tab$Found_peaks_pp, na.rm = TRUE) + sum(sum_tab$Not_Found_peaks_pp, na.rm = TRUE)),
                         Features = comparison_data$BM_NPPoutput_size$nr_of_b_features
                       ),
                       Before_alignment = list(
                         NT_peaks = NA,#UT_peaks,
                         Found_peaks = list(count = sum(sum_tab$Found_peaks_pp, na.rm = TRUE),
                                            CI = boot::boot.ci(boot::boot(sum_tab,
                                                                          function(data, indices){
                                                                            dt<-data[indices,]
                                                                            round(sum(dt$Found_peaks_pp, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE) + sum(dt$Not_Found_peaks_pp, na.rm = TRUE))*100,2)
                                                                          },
                                                                          R = 1000),
                                                               index=1,
                                                               type='basic')$basic
                         ),
                         Split_peaks = list(count = sum(sum_tab$Split_peaks, na.rm = TRUE),
                                            CI = if(sum(sum_tab$Split_peaks, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                          function(data, indices){
                                                                            dt<-data[indices,]
                                                                            round(sum(dt$Split_peaks, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE) + sum(dt$Split_peaks, na.rm = TRUE))*100,2)
                                                                          },
                                                                          R = 1000),
                                                               index=1,
                                                               type='basic')$basic} else {rep(0,5)}
                         ),
                         Missing_peaks = list(
                           Systematic = sum(sum_tab$S_pp, na.rm = TRUE),
                           Random =  list(count = sum(sum_tab$R_pp, na.rm = TRUE),
                                          CI = if(sum(sum_tab$R_pp, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                        function(data, indices){
                                                                          dt<-data[indices,]
                                                                          ret <- round(sum(dt$R_pp, na.rm = TRUE)/(sum(dt$R_pp, na.rm = TRUE) + sum(dt$S_pp, na.rm = TRUE))*100,2)
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
                                                      CI =  if(sum(sum_tab$IRb_off_pp, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                                    function(data, indices){
                                                                                      dt<-data[indices,]
                                                                                      round(sum(dt$IRb_off_pp, na.rm = TRUE)/(sum(dt$IRb_off_pp, na.rm = TRUE) + sum(dt$IRb_ok_pp, na.rm = TRUE))*100,2)
                                                                                    },
                                                                                    R = 1000),
                                                                         index=1,
                                                                         type='basic')$basic} else {rep(0,5)}
                           )
                         )
                       ),
                       Alignment = list(
                         Min.Errors = list(count = sum(sum_tab$Min.er, na.rm = TRUE),
                                           CI = if(sum(sum_tab$Min.er, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                         function(data, indices){
                                                                           dt<-data[indices,]
                                                                           ret <- round(sum(dt$Min.er, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,2)
                                                                           if(is.nan(ret)){
                                                                             return(0)
                                                                           } else return(ret)

                                                                         },
                                                                         R = 1000),
                                                              index=1,
                                                              type='basic')$basic} else rep(0,5)
                                           ),
                         BM_divergences = list(count = sum(sum_tab$BM.div, na.rm = TRUE),
                                               CI = if(sum(sum_tab$BM.div, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                             function(data, indices){
                                                                               dt<-data[indices,]
                                                                               ret <- round(sum(dt$BM.div, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,2)
                                                                               if(is.nan(ret)){
                                                                                 return(0)
                                                                                 } else return(ret)
                                                                               },
                                                                             R = 1000),
                                                                  index=1,
                                                                  type='basic')$basic} else rep(0,5)
                                               ),
                         Lost_b.A = list(count = sum(sum_tab$lost, na.rm = TRUE),
                                         CI = if(sum(sum_tab$lost, na.rm = TRUE) > 0){boot::boot.ci(boot::boot(sum_tab,
                                                                       function(data, indices){
                                                                         dt<-data[indices,]
                                                                         ret <- round(sum(dt$lost, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE))*100,2)
                                                                         if(is.nan(ret)){
                                                                           return(0)
                                                                         } else return(ret)
                                                                         },
                                                                       R = 1000),
                                                            index=1,
                                                            type='basic')$basic} else rep(0,5)
                                         )
                       ),
                       After_alignment = list(
                         Found_peaks = list(count = sum(sum_tab$Found_peaks_ft, na.rm = TRUE),
                                            CI =  boot::boot.ci(boot::boot(sum_tab,
                                                                           function(data, indices){
                                                                             dt<-data[indices,]
                                                                             round(sum(dt$Found_peaks_ft, na.rm = TRUE)/(sum(dt$Found_peaks_pp, na.rm = TRUE) + sum(dt$Not_Found_peaks_pp, na.rm = TRUE))*100,2)
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
                                                                          ret <- round(sum(dt$R_ft, na.rm = TRUE)/(sum(dt$R_ft, na.rm = TRUE) + sum(dt$S_ft, na.rm = TRUE))*100,2)
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
                                                                                      ret <- round(sum(dt$IRb_off_ft, na.rm = TRUE)/(sum(dt$IRb_off_ft, na.rm = TRUE) + sum(dt$IRb_ok_ft, na.rm = TRUE))*100,2)
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


