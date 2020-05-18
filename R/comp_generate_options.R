generate_options <- function(raw_b_table, algo){
  #Hardcoded internal columns
  columns_dt <- data.table('internal_columns' = c('peak_height', 'peak_area', 'sample_name','molecule', 'adduct', 'isoabb',
                                                  'rt_start', 'rt_end', 'rt', 'mz', 'comp_id', 'peak_group', 'mz_start', 'mz_end'),
                           'b_columns' = c('peaks.height', 'peaks.area', 'FileName', 'molecule', 'adduct', 'isoabb',
                                           'peaks.StartTime', 'peaks.EndTime', 'peaks.rt_raw', 'mz', 'IDX', 'peaks.M0.grp', 'peaks.mz_min', 'eic_mzmax')
                           )
  #Get sample names from benchmark
  samples_dt <- data.table('sample_id' = seq(unique(raw_b_table$FileName)),
                           'b_samples' = sort(unique(raw_b_table$FileName)))
  switch(algo,
    'XCMS' = {
      #Add ug columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('peak_height', 'peak_area', 'sample_name', 'rt_start', 'rt_end', 'rt', 'mz', 'mz_start', 'mz_end'),
                                                 'ug_columns' = c('maxo', 'into', 'sample', 'rtmin', 'rtmax', 'rt', 'mz', 'mzmin', 'mzmax')),
                          all.x = TRUE, by=c('internal_columns'))
      #Add g columns
#      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('rt_start', 'rt_end', 'rt', 'mz'),
#                                                 'g_columns' = c('rtmin', 'rtmax', 'rt', 'mz')),
#                          all.x = TRUE, by=c('internal_columns'))
            columns_dt <- merge(columns_dt, data.table('internal_columns' = c('rt', 'mz'),
                                                       'g_columns' = c('rt', 'mz')),
                                all.x = TRUE, by=c('internal_columns'))

      #Add ug samples
      samples_dt <- samples_dt[, 'ug_samples' := sample_id]
      #Add g samples
      samples_dt <- samples_dt[, 'g_samples' := ifelse(grepl('^[0-9]', b_samples), paste0('X', b_samples), b_samples)]
    },
    'El-MAVEN' = {
      #Add ug columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('peak_height', 'peak_area', 'sample_name', 'rt_start', 'rt_end', 'rt', 'mz', 'mz_start', 'mz_end'),
                                                 'ug_columns' = c('peakIntensity', 'peakAreaTopCorrected', 'sample', 'rtmin', 'rtmax', 'rt', 'peakMz', 'mzmin', 'mzmax')),
                          all.x = TRUE, by=c('internal_columns'))

      #Add g columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('rt', 'mz'),
                                                 'g_columns' = c('medRt', 'medMz')),
                          all.x = TRUE, by=c('internal_columns'))

      #Add ug samples
      samples_dt <- samples_dt[, 'ug_samples' := b_samples]
      #Add g samples
      samples_dt <- samples_dt[, 'g_samples' := b_samples]


    },
    'msDial' = {
      #Add ug columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('peak_height', 'peak_area', 'rt_start', 'rt_end', 'rt', 'mz'),
                                                 'ug_columns' = c('Height', 'Area', 'RT left(min)', 'RT right (min)', 'RT (min)', 'Precursor m/z')),
                          all.x = TRUE, by=c('internal_columns'))
      #Add g columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('rt', 'mz'),
                                                 'g_columns' = c('Average Rt(min)', 'Average Mz')),
                          all.x = TRUE, by=c('internal_columns'))

      #Add ug samples
      samples_dt <- samples_dt[, 'ug_samples' := b_samples]
      #Add g samples
      samples_dt <- samples_dt[, 'g_samples' := b_samples]
    },
    'mzMine' = {
      #Add ug columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('peak_height', 'peak_area', 'rt_start', 'rt_end', 'rt', 'mz', 'mz_start', 'mz_end'),
                                                 'ug_columns' = c('Peak height', 'Peak area', 'Peak RT start', 'Peak RT end', 'Peak RT', 'Peak m/z', 'Peak m/z min', 'Peak m/z max')),
                          all.x = TRUE, by=c('internal_columns'))
      #Add g columns
      columns_dt <- merge(columns_dt, data.table('internal_columns' = c('rt', 'mz'),
                                                 'g_columns' = c('row retention time', 'row m/z')),
                          all.x = TRUE, by=c('internal_columns'))

      #Add ug samples
      samples_dt <- samples_dt[, 'ug_samples' := paste0(b_samples, '.mzML')]
      #Add g samples
      samples_dt <- samples_dt[, 'g_samples' := paste0(b_samples, '.mzML')]
    },
    {return(NULL)})

  #Find smaller column and pad it with NA
  if(nrow(samples_dt) > nrow(columns_dt)){
    diff = nrow(samples_dt) - nrow(columns_dt)
    for (i in 1:diff){
      columns_dt <- rbind(columns_dt, columns_dt[NA])
    }
  } else if(nrow(columns_dt) > nrow(samples_dt)){
    diff = nrow(columns_dt) - nrow(samples_dt)
    for (i in 1:diff){
      samples_dt <- rbind(samples_dt, samples_dt[NA])
    }
  }

  options_dt <- cbind(columns_dt, samples_dt)
  return(options_dt)
}
