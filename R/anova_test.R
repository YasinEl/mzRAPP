#ANOVA analysis & Vlcano Plot
#Calc Anova

library(data.table)

vulcano_plot <- function (comparison, no_of_samples){
  anova_dt <- rbindlist(list(comparison$c_table, comparison$nf_b_table), fill=TRUE)
  #Only Keep features where all benchmark peeks are present
  print(nrow(anova_dt))
  anova_dt <- anova_dt[, if(.N == no_of_samples) .SD, by = .(molecule_b, adduct_b, isoabb_b, grp_b)]
  print(nrow(anova_dt))
  #Genertate Tuckey for each Molecule_adduct_Iso combination if 2 or more groups with atleast one valid datapoint are present
  anova_dt <- anova_dt[, anova_group := paste(molecule_b, adduct_b, isoabb_b, sep='_')]
  #fwrite(anova_dt, 'anova_dbug.csv')

  #result_dt<-TukeyHSD(aov(peak_area_b ~ as.factor(grp_b), data=anova_dt))



  for (group in unique(anova_dt$anova_group)){
    #print(group)
    temp_data <- anova_dt[anova_group == group]
    if (length(unique(temp_data$grp_b))>=2){
      temp_data$grp_b <- as.factor(temp_data$grp_b)
      aov_temp <- aov(peak_area_g ~ grp_b, data=temp_data)
      tukey_result <- TukeyHSD(aov_temp)
      tukey_dt <- data.table(tukey_result$grp_b, keep.rownames = TRUE)
      tukey_dt[, anova_group := group]
      setnames(tukey_dt, c('rn', 'p adj'), c('comparison', 'p_adj'))

      #Calculate foldchanges, probaly can be done with apply
      mean_list <- as.list(tapply(temp_data$peak_area_g, temp_data$grp_b, mean))
      fold_change_list <- list()
      for (name_1 in names(mean_list)){
        temp_grp <- name_1
        temp_value <- mean_list[[temp_grp]]

        for (name_2 in names(mean_list)){
          comparison_name <- paste(name_1, name_2, sep = '-')
          fold_change = temp_value/mean_list[[name_2]]
          #if (fold_change < 1){
          #  fold_change <- (mean_list[[name_2]]/temp_value)
          #}
          fold_change_list[comparison_name] <- fold_change
        }
      }

      fold_change_dt <- transpose(setDT(fold_change_list, keep.rownames = TRUE), keep.names = 'comparison')
      setnames(fold_change_dt, 'V1', 'fold_change')

      tukey_fold_change_result <- merge(tukey_dt, fold_change_dt, by=c('comparison'), all.x = TRUE)
      #print(fold_change_dt)
      #print(tukey_fold_change_result)

      #Merge datatable
      if (!exists('result_dt')){
        result_dt <- tukey_fold_change_result
      } else {
        result_dt <- rbindlist(list(result_dt, tukey_fold_change_result))
      }
    } else {
      #print('Only one group')
    }
    #print('------')
  }






  #fwrite(result_dt, 'tukey_result.csv')

  result_dt <- result_dt[, ':='('plot_p' = -log10(p_adj),
                                'plot_fold' = log2(fold_change))]
  print(result_dt)
  return(result_dt)
}


#plot_data <- vulcano_plot(comparison_ev, 5)
#print(plot_data)
#ggplotly(ggplot(plot_data, aes(x=plot_fold, y=plot_p, group=anova_group, comparison=comparison)) +
#                labs(x='log2(fold change)', y='-log10(p value)')+
#                 geom_point())
