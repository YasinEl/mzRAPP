#' generate_results_text
#'
#' @param comparison
#'
#' @return
#' @export
#'
#' @examples
generate_results_text <- function(comparison_data){
  UT_peaks <-
    nrow(rbindlist(list(comparison_data$c_table[main_peak == TRUE], comparison_data$nf_g), fill = TRUE))


  main_peak_table <- comparison_data$c_table[main_peak == TRUE]

  found_ug_peaks <- length(unique(main_peak_table$comp_id_ug))
  #split_features <- sum(main_peak_table[, count_split_features(feature_id_g), by=feature_id_b]$V1, na.rm=TRUE)


  if(nrow(comparison_data$rs_table[missing_peaks == "S" | missing_peaks == "R"]) == 0) {
    bv_Missing_peaks <- rep(FALSE, 10)
  } else {
    bv_Missing_peaks <- c(rep(TRUE, nrow(comparison_data$rs_table[missing_peaks == "R"])),
                          rep(FALSE, nrow(comparison_data$rs_table[missing_peaks == "S"])))

  }

  results_text <- list(Assessed_tool = comparison_data$info_list$algorithm,
                       Benchmark = list(
                         BM_peaks = comparison_data$info_list$nr_of_b_peaks,
                         Features = comparison_data$info_list$nr_of_b_features
                       ),
                       Before_alignment = list(
                         NT_peaks = UT_peaks,
                         Found_peaks = list(count = found_ug_peaks,
                                            CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, found_ug_peaks),
                                                                                             rep(FALSE, comparison_data$info_list$nr_of_b_peaks - found_ug_peaks))),
                                                                          function(data, indices){
                                                                            dt<-data[indices,]
                                                                            round(length(which(dt))/length(dt)*100,1)
                                                                          },
                                                                          R = 1000),
                                                               index=1,
                                                               type='basic')$basic
                         ),
                         Split_peaks = list(count = length(unique(comparison_data$split_table$comp_id_b)),
                                            CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, length(unique(comparison_data$split_table$comp_id_b))),
                                                                                             rep(FALSE, comparison_data$info_list$nr_of_b_peaks -
                                                                                                   length(unique(comparison_data$split_table$comp_id_b))))),
                                                                          function(data, indices){
                                                                            dt<-data[indices,]
                                                                            round(length(which(dt))/length(dt)*100,1)
                                                                          },
                                                                          R = 1000),
                                                               index=1,
                                                               type='basic')$basic
                         ),
                         Missing_peaks = list(
                           Systematic = nrow(comparison_data$rs_table[missing_peaks == "S"]),
                           Random =  list(count = nrow(comparison_data$rs_table[missing_peaks == "R"]),
                                          CI = boot::boot.ci(boot::boot(data.frame(var = bv_Missing_peaks),
                                                                        function(data, indices){
                                                                          dt<-data[indices,]
                                                                          round(length(which(dt))/length(dt)*100,1)
                                                                        },
                                                                        R = 1000),
                                                             index=1,
                                                             type='basic')$basic
                           )
                         ),
                         IR_quality = list(
                           Error_inc_below20pp = nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. < 20%p"]),
                           Error_inc_above20pp = list(count = nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. > 20%p"]),
                                                      CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. > 20%p"])),
                                                                                                       rep(FALSE, nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. < 20%p"])))),
                                                                                    function(data, indices){
                                                                                      dt<-data[indices,]
                                                                                      round(length(which(dt))/length(dt)*100,1)
                                                                                    },
                                                                                    R = 1000),
                                                                         index=1,
                                                                         type='basic')$basic
                           )
                         )
                       ),
                       Alignmnet = list(
                         #Errors = sum(comparison_data$ali_error_table$Min.errors, na.rm = TRUE)
                         Min.Errors = list(count = sum(comparison_data$ali_error_table$Min.errors, na.rm = TRUE),
                                       CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, sum(comparison_data$ali_error_table$Min.errors, na.rm = TRUE)),
                                                                                        rep(FALSE, found_ug_peaks))),#var = comparison_data$ali_error_table$Min.errors),


                                                                     function(data, indices){
                                                                       dt<-data[indices,]
                                                                       round(length(which(dt))/length(dt)*100,1)
                                                                     },
                                                                     R = 1000),
                                                          index=1,
                                                          type='basic')$basic),
                         BM_divergences = list(count = sum(comparison_data$ali_error_table$BM.div, na.rm = TRUE),
                                         CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, sum(comparison_data$ali_error_table$BM.div, na.rm = TRUE)),
                                                                                          rep(FALSE, found_ug_peaks))),
                                                                       function(data, indices){
                                                                         dt<-data[indices,]
                                                                         round(length(which(dt))/length(dt)*100,1)
                                                                       },
                                                                       R = 1000),
                                                            index=1,
                                                            type='basic')$basic),
                         Lost_b.A = list(count = sum(comparison_data$ali_error_table$Lost_b.A, na.rm = TRUE),
                                         CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, sum(comparison_data$ali_error_table$Lost_b.A, na.rm = TRUE)),
                                                                                          rep(FALSE, found_ug_peaks))),
                                                                       function(data, indices){
                                                                         dt<-data[indices,]
                                                                         round(length(which(dt))/length(dt)*100,1)
                                                                       },
                                                                       R = 1000),
                                                            index=1,
                                                            type='basic')$basic)
                       ),
                       After_alignmnet = list(
                         Found_peaks = list(count = nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE & !is.na(area_g)]),
                                            CI =  boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE & !is.na(area_g)])),
                                                                                              rep(FALSE, comparison_data$info_list$nr_of_b_peaks -
                                                                                                    nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE & !is.na(area_g)])))),
                                                                           function(data, indices){
                                                                             dt<-data[indices,]
                                                                             round(length(which(dt))/length(dt)*100,1)
                                                                           },
                                                                           R = 1000),
                                                                index=1,
                                                                type='basic')$basic
                         ),
                         Found_features = nrow(unique(comparison_data$feature_table[!is.na(area_b) &
                                                                                      !is.na(area_g) &
                                                                                      main_feature == TRUE,
                                                                                    c("molecule_b", "adduct_b", "isoabb_b")], cols = c("molecule_b", "adduct_b", "isoabb_b"))),
                         IR_quality = list(
                           Error_inc_below20pp = nrow(comparison_data$iso_err_dt[diffH20PP_ft == "Inc. < 20%p"]),
                           Error_inc_above20pp = list(count = nrow(comparison_data$iso_err_dt[diffH20PP_ft == "Inc. > 20%p"]),
                                                      CI = boot::boot.ci(boot::boot(data.frame(var = c(rep(TRUE, nrow(comparison_data$iso_err_dt[diffH20PP_ft == "Inc. > 20%p"])),
                                                                                                       rep(FALSE, nrow(comparison_data$iso_err_dt[diffH20PP_ft == "Inc. < 20%p"])))),
                                                                                    function(data, indices){
                                                                                      dt<-data[indices,]
                                                                                      round(length(which(dt))/length(dt)*100,1)
                                                                                    },
                                                                                    R = 1000),
                                                                         index=1,
                                                                         type='basic')$basic)
                         )
                       )
  )


  #results_text <- paste0("Assessed tool: ", comparison_data$info_list$algorithm, "     NT-peaks: ", UT_peaks,     "     ", "Found peaks: ",  found_ug_peaks, "/",
  #                       comparison_data$info_list$nr_of_b_peaks, " (", round((found_ug_peaks/comparison_data$info_list$nr_of_b_peaks)*100, 1), "%)     Peak fragments: ",  length(unique(comparison_data$split_table$comp_id_b)),
  #                       "     Missing Value (S|R): ", nrow(comparison_data$rs_table[missing_peaks == "S"]), "|", nrow(comparison_data$rs_table[missing_peaks == "R"]),
  #                       "     Pred. error increase >20%p: ", nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. > 20%p"]), "/", nrow(comparison_data$iso_err_dt[!is.na(diffH20PP_pp)]), " (",
  #                       round(nrow(comparison_data$iso_err_dt[diffH20PP_pp == "Inc. > 20%p"])/nrow(comparison_data$iso_err_dt[!is.na(diffH20PP_pp)]) * 100, 1), "%)",
  #                       "     Min. # of alignment errors: ", sum(comparison_data$ali_error_table$errors, na.rm = TRUE),  "     Found peaks (features): ",
  #                       nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE & !is.na(area_g)]), "/", comparison_data$info_list$nr_of_b_peaks, " (",
  #                       round(nrow(comparison_data$feature_table[!is.na(area_b) & main_feature == TRUE & !is.na(area_g)])/comparison_data$info_list$nr_of_b_peaks * 100, 1), "%)",
  #                       "     Found features (features): ",
  #                       nrow(unique(comparison_data$feature_table[!is.na(area_b) &
  #                                                                   !is.na(area_g) &
  #                                                                   main_feature == TRUE,
  #                                                                 c("molecule_b", "adduct_b", "isoabb_b")], cols = c("molecule_b", "adduct_b", "isoabb_b"))), "/",
  #                       comparison_data$info_list$nr_of_b_features, " (",
  #                       round(nrow(unique(comparison_data$feature_table[!is.na(area_b) &
  #                                                                         !is.na(area_g) &
  #                                                                         main_feature == TRUE,
  #                                                                       c("molecule_b", "adduct_b", "isoabb_b")], cols = c("molecule_b", "adduct_b", "isoabb_b")))/
  #                         comparison_data$info_list$nr_of_b_features*100, 1), "%)", "     Pred. error increase >20%p (features): ", nrow(comparison_data$iso_err_dt[diffH20PP_ft == "Inc. > 20%p"]), "/", nrow(comparison_data$iso_err_dt[!is.na(diffH20PP_ft)]), " (",
  #                       round(nrow(comparison_data$iso_err_dt[diffH20PP_ft == "Inc. > 20%p"])/nrow(comparison_data$iso_err_dt[!is.na(diffH20PP_ft)]) * 100, 1), "%)")

  return(results_text)
}


#count_split_features <- function(feature_id_ug){
#  if(length(unique(feature_id_ug))>1){return (TRUE)}
#  else {return (FALSE)}
#}

#generate_results_text(comparison_ug_g)
