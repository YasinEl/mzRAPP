#' findBenchPeaks
#'
#' @description Takes a the output of \code{\link{getROIsForEICs}} and detects and filters peak candidates.
#'
#' @param files vector with file paths
#' @param Grps data frame with two columns: one for filenames without .mzML (sample_name) and one for their respective sample group affiliations (sample_group).
#' @param CompCol_all output from function \code{\link{getROIsForEICs}}
#' @param Min.PointsperPeak minimum number of points per peak for a peak to be considered
#' @param peak.spotting.factor this parameter is ignored when user.rtmin/user.rtmax are given in the CompCol_all table. Relative height to the highest point of the EIC above which points should be considered during peak detection process. e.g. 0.001 corresponds to 0.1\% of the maximum.
#' @param Integration_baseL_factor relative peak height factor upon which points should be considered to be part of the peak. 0.1 would correspond to 10\% of the peak maximum.
#' @param plan see \code{\link{plan}}
#' @param Min.cor.w.main_adduct Minimum pearson correlation coefficient between main_adduct and other adducts for other adducts to be retained
#' @param Min.cor.w.M0 Minimum pearson correlation coefficient between highest isotopologues and lower isotopologues for lower isotopologues to be retained.
#' @param Min.iso.count Minimum number of isotopotologues per compound to be kept in the final output. Has to be more than one.
#' @param return_unsuc_searches Should unsuccsessfull searches be returned (TRUE/FALSE)
#' @param Min.Res this parameter is ignored when user.rtmin/user.rtmax are given in the CompCol_all table. At which maximum height (percentage; measured relative to the lower peaks maximum) should to chromatographic peaks be resolved for them to be considered as separate peaks
#' @param max.rt.diff_sec maximum difference between user.rt in position of peak maximum in seconds
#' @param max.mz.diff_ppm maximum difference between intensity weighted mz of a peak and the calculated mz of the expeted ion species in ppm
#'
#' @return data table with peak variables extracted from found peaks.
#' @export

findBenchPeaks <- function(files,
                           Grps,
                           CompCol_all,
                           Min.PointsperPeak = 10,
                           peak.spotting.factor = 0.001,
                           Integration_baseL_factor = 0.1,
                           Min.Res = 60,
                           plan = "multiprocess",
                           Min.cor.w.main_adduct = 0.8,
                           Min.cor.w.M0 = 0.75,
                           Min.iso.count = 2,
                           return_unsuc_searches = FALSE,
                           max.rt.diff_sec = 20,
                           max.mz.diff_ppm = 5)
{



  #if(length(files) < 2){stop("Please provide more than 1 mzML file.")}
  if(!isTRUE(is.data.frame(CompCol_all))){stop(paste0("CompCol_all has to be a data frame and/or data table!"))}
  if(!isTRUE(is.data.table(CompCol_all))){CompCol_all <- as.data.table(CompCol_all)}


  if(!is.character(CompCol_all$molecule)) {CompCol_all$molecule <- as.character(CompCol_all$molecule)}
  if(!isTRUE(is.data.frame(Grps))){stop(paste0("Grps has to be a data frame and/or data table!"))}
  if(!isTRUE(is.data.table(Grps))){Grps <- as.data.table(CompCol_all)}
  missing_cols <- setdiff(c("sample_name", "sample_group"), colnames(Grps))
  if(length(missing_cols) > 0){stop(paste0("Sample_group table is lacking columns: ", paste(missing_cols, collapse = ", ")))}
  Grps <- Grps[, sample_name := tools::file_path_sans_ext(basename(sample_name))]

  if(length(files[!file.exists(files)] > 0)) stop(paste0("It seems like some of your mzML files do not exist, cannot be accessed or contain spelling errors! Specificly:", paste(files[!file.exists(files)], collapse = ", ")))

  if(length(intersect(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)), Grps$sample_name)) == 0){
    stop("Filenames in your sample_group file are not the same as those of your uploaded mzML files!")
  }



  if(all(c("user.rtmin", "user.rtmax") %in% colnames(CompCol_all))){
    colnames(CompCol_all)[which(names(CompCol_all) == "user.rtmin")] <- "rtmin"
    colnames(CompCol_all)[which(names(CompCol_all) == "user.rtmax")] <- "rtmax"
  }


  CompCol <-
    na.omit(CompCol_all,
            cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))

#print(nrow(CompCol))
#cc <<- CompCol


  ##################################
  #prepare parallel processing
  ##################################
  doFuture::registerDoFuture()
  future::plan(plan)
  `%dopar%` <- foreach::`%dopar%`
  #future::plan(list("sequential", "multiprocess"))
  #future::plan(multiprocess(workers = 40))
  Output <- list()
  Output <-
    foreach::foreach(file = 1:nrow(unique(CompCol[, "FileName"])),
            .packages = c("mzRAPP", "data.table")) %dopar% {
#                          for(file in 1:nrow(unique(CompCol[, "FileName"]))){


              ##################################
              #read in mzML files
              ##################################
              suppressWarnings(
              .raw_data <- MSnbase::readMSData(
                files = files[file],
                pdata = new("NAnnotatedDataFrame", Grps[sample_name == sub(pattern = "(.*)\\..*$",
                                                                           replacement = "\\1",
                                                                           basename(files[file]))]),
                msLevel. = 1,
                mode = "onDisk"
              )
              )

              MA.Isos <- data.table(NULL)
              ##################################
              #setup order of EIC evaluation: 1. most abundant isotopologue (MAiso) of main adduct (main_adduct); 2. less abundant isotopologues (LAisos) of main adduct if peak has been found in "1.";
              #3. most abundant isotopologue of other adducts (screen_adducts) if peak has been found in "1."; 4. less abundant isotopologues of screen adduct if peak has been found in "3."
              ##################################
              for (adduct.run in c("main_adduct", "screen_adducts")) {
                for (iso.run in c("MAiso", "LAisos")) {

                  #print(adduct.run)
                  #print(iso.run)


                  Bureau <- NULL
                  if(adduct.run == "main_adduct" & iso.run == "MAiso" | nrow(MA.Isos) > 0 & "peaks.PpP" %in% colnames(MA.Isos)){
                    .CompCol_xic <- Limit_Target_list(CompCol, MA.Isos, iso.run, adduct.run, files[file], Min.PointsperPeak)
                  } else .CompCol_xic <- data.table(NULL)


                  if(nrow(.CompCol_xic) > 0){
                    #print("something")
                    ##################################
                    #extract EICs, add according file names and create empty list for storage of peak information
                    ##################################
                    suppressWarnings(
                    .ChromData <- MSnbase::chromatogram(
                      .raw_data,
                      rt = as.matrix(unname(.CompCol_xic[, c("StartTime.EIC", "EndTime.EIC")])),
                      mz = as.matrix(unname(.CompCol_xic[, .(MinMz = eic_mzmin - 0.0001,
                                                             MaxMz = eic_mzmax + 0.0001)][, .(MinMz, MaxMz)])),
                      missing = 0,
                      msLevel = 1L,
                      aggregationFun = "max"#,
                      #BPPARAM = SnowParam(detectCores()-1)
                    ))


                    MSnbase::sampleNames(.ChromData) <-
                      basename(sub(
                        pattern = "(.*)\\..*$",
                        replacement = "\\1",
                        basename(files[file])
                      ))


                    ##################################
                    #apply peak detection and evaluation to all EICs; Output is stored in Bureau
                    ##################################
                    Bureau <-
                    #future.apply::future_lapply(seq(length(.ChromData)), function(i,
                      lapply(seq(length(.ChromData)), function(i,
                                                               CompCol_xic = .CompCol_xic,
                                                               raw_data = .raw_data,
                                                               ChromData = .ChromData) {

                        ##################################
                        #prepare list for collecting information on peaks of one extracted ion chromatogram
                        ##################################
                        Drawer_fill <- as.list(rep(NA, 9))
                        names(Drawer_fill) <-
                          c(
                            "molecule",
                            "adduct",
                            "isoab",
                            "user.rt",
                            "FileName",
                            "Grp",
                            "peaks",
                            "Intensities.v",
                            "RT.v"
                          )


                        ##################################
                        #store basic information on each EIC
                        ##################################
                        Drawer_fill[["molecule"]] <- CompCol_xic[i]$molecule
                        Drawer_fill[["adduct"]] <-  CompCol_xic[i]$adduct
                        Drawer_fill[["isoab"]] <- CompCol_xic[i]$isoab

                          Drawer_fill[["user.rt"]] <- if("user.rt" %in% colnames(CompCol_xic)){CompCol_xic[i]$user.rt}else{NA}

                        Drawer_fill[["FileName"]] <- raw_data@phenoData@data[["sample_name"]]
                        Drawer_fill[["Grp"]] <- raw_data@phenoData@data[["sample_group"]]
                        Drawer_fill[["RT.v"]] <- paste(as.character(unname(ChromData[[i]]@rtime)), collapse = ",")
                        Drawer_fill[["Intensities.v"]] <- paste(as.character(unname(ChromData[[i]]@intensity)), collapse = ",")
                        #print( Drawer_fill[["molecule"]])
                        #print( Drawer_fill[["FileName"]])
                        ##################################
                        #prepare table with smoothed and spike depleted EICs
                        ##################################
                        EIC.dt <- get_EIC_table(rt = unname(ChromData[[i]]@rtime),
                                                            int = unname(ChromData[[i]]@intensity),
                                                            Min.PpP = Min.PointsperPeak)


                        if (sum(EIC.dt$int > 0)) {

                          if (iso.run == "LAisos" |
                              (adduct.run == "screen_adducts" & iso.run == "MAiso")) {

                            ##################################
                            #add EIC of highest isotopologue (for lower isotopologues of an adduct) or EIC of main adduct (for most abundant isotopologue of other adducts) to EIC.dt
                            ##################################
                            EIC.dt[, M0_int :=
                                     as.numeric(unlist(strsplit(
                                       MA.Isos[molecule == CompCol_xic[i]$molecule &
                                                 adduct == ifelse(iso.run == "MAiso",
                                                                  paste0(CompCol_xic[i]$main_adduct),
                                                                  paste0(CompCol_xic[i]$adduct)) &
                                                 FileName == raw_data@phenoData@data[["sample_name"]]][1]$Intensities.v,
                                       split = ","
                                     )))]


                            ##################################
                            #generate table of peaks that justified this search (highest isotopologue for lower isotopologues of each adduct; highest isotopologue of main
                            #adduct for other highest isotopologue of other adducts)
                            ##################################
                            M0_peaks <- MA.Isos[molecule == CompCol_xic[i]$molecule &
                                                  adduct == ifelse(iso.run == "MAiso",
                                                                   paste0(CompCol_xic[i]$main_adduct),
                                                                   paste0(CompCol_xic[i]$adduct)) &
                                                  FileName == raw_data@phenoData@data[["sample_name"]]]

                          }


                          ##################################
                          #extract peaks from complete EICs (this is only done for the most abundant isotopologue of the main adduct)
                          ##################################

                          #check for manual boundaries
                          manual_bound <- FALSE
                          if (iso.run == "MAiso" & adduct.run == "main_adduct") {
                            if(all(c("rtmin", "rtmax") %in% colnames(CompCol_xic))){
                              if(!is.na(CompCol_xic[i]$rtmin) & !is.na(CompCol_xic[i]$rtmax)){
                                if(CompCol_xic[i]$rtmin > min(EIC.dt[!is.na(int_wo_spikes)]$rt) &
                                   CompCol_xic[i]$rtmax < max(EIC.dt[!is.na(int_wo_spikes)]$rt) &
                                   CompCol_xic[i]$rtmin < CompCol_xic[i]$rtmax){
                                  l.peaks <- data.table(idx = 1, rtmin = CompCol_xic[i]$rtmin, rtmax = CompCol_xic[i]$rtmax, M0.grp = 1)
                                  manual_bound <- TRUE
                                }
                              }
                            }

#print(manual_bound)
                            #automatic boundary imputation
                            if(manual_bound == FALSE){
                              l.peaks <- cutout_peaks(
                                int = EIC.dt[!is.na(int_wo_spikes)]$int,
                                rt = EIC.dt[!is.na(int_wo_spikes)]$rt,
                                Min.PpP = Min.PointsperPeak,
                                peak.spotting.factor. = peak.spotting.factor,
                                Integration_baseL_factor. = Integration_baseL_factor,
                                Min.Res. = Min.Res,
                                l = if(is.null(CompCol_xic[i]$user.rtmin)){1} else{if(!is.na(as.numeric(CompCol_xic[i]$user.rtmin))){
                                  which.min(abs(CompCol_xic[i]$user.rtmin - EIC.dt[!is.na(as.numeric(int_wo_spikes))]$rt))}else {1}},
                                r = if(is.null(CompCol_xic[i]$user.rtmax)){length(EIC.dt[!is.na(int_wo_spikes)]$rt)} else{
                                  if(!is.na(CompCol_xic[i]$user.rtmax)){
                                  which.min(abs(CompCol_xic[i]$user.rtmax - EIC.dt[!is.na(int_wo_spikes)]$rt))}else{length(EIC.dt[!is.na(int_wo_spikes)]$rt)}}
                              )
                            }

                            ##################################
                            #extract peaks from rt range of justifying peaks (highest isotopologue for lower isotopologues of each adduct; highest isotopologue of main
                            #adduct for other highest isotopologue of other adducts)
                            ##################################
                          } else if (nrow(M0_peaks) > 0)  {

                            l.peaks <- mapply(
                              cutout_peaks,
                              l = sapply(M0_peaks$peaks.StartTime, function(val) {
                                #ifelse(which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) - 1 <= 1,
                                #       1,
                                #       which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) - 1)
                                which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val))
                              }),
                              r = sapply(M0_peaks$peaks.EndTime, function(val) {
                                #ifelse(
                                #  which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) + 1 >= length(EIC.dt[!is.na(int_wo_spikes)]$rt),
                                #  length(EIC.dt[!is.na(int_wo_spikes)]$rt),
                                #  which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) + 1
                                #)
                                which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val))
                              }),
                              M0.grp = M0_peaks$peaks.M0.grp, #ifelse(iso.run == "LAisos", paste0(M0_peaks$peaks.M0.grp), NA),
                              #main_adduct.grp = NA, #ifelse(iso.run == "MAiso", M0_peaks$peaks.main_adduct.grp, NA),
                              MoreArgs = list(
                                int = EIC.dt[!is.na(int_wo_spikes)]$int,
                                rt = EIC.dt[!is.na(int_wo_spikes)]$rt,
                                Min.PpP = Min.PointsperPeak,
                                peak.spotting.factor. = peak.spotting.factor,
                                Integration_baseL_factor. = Integration_baseL_factor,
                                Min.Res. = Min.Res
                              ),
                              SIMPLIFY = FALSE
                            )


                            if (!is.null(l.peaks)) {
                              l.peaks <- rbindlist(l.peaks, use.names = TRUE)
                              l.peaks$idx <- 1:nrow(l.peaks)
                            }
                          } else if (nrow(M0_peaks) == 0) {
                            l.peaks <- NULL
                          }

                          if (!is.null(l.peaks)) {
                            if (nrow(l.peaks) > 0 & length(l.peaks) > 1) {

                              ##################################
                              #get start and end time for detected peaks and add them to the table
                              ##################################

                              Integration_baseL_factor_set <- Integration_baseL_factor
                              if (iso.run == "MAiso" & adduct.run == "main_adduct" & manual_bound == TRUE){
                                #l.peaks <- l.peaks[, StartTime := rtmin]
                                #l.peaks <- l.peaks[, EndTime := rtmax]
                                Integration_baseL_factor_set <- 0.03

                              }

                               l.peaks <- l.peaks[l.peaks[, .(StartTime = GetFWXM(
                                     EIC.dt[rt >= rtmin &
                                              rt <= rtmax &
                                              !is.na(int_wo_spikes)]$rt,
                                     EIC.dt[rt >= rtmin &
                                              rt <= rtmax &
                                              !is.na(int_wo_spikes)]$int,
                                     min(EIC.dt[rt >= rtmin &
                                                  rt <= rtmax &
                                                  !is.na(int_wo_spikes)]$int),
                                     ifelse(iso.run == "MAiso", Integration_baseL_factor_set, 0.03),
                                     peak_borders = TRUE
                                   )[1],
                                 EndTime = GetFWXM(
                                   EIC.dt[rt >= rtmin &
                                            rt <= rtmax &
                                            !is.na(int_wo_spikes)]$rt,
                                   EIC.dt[rt >= rtmin &
                                            rt <= rtmax &
                                            !is.na(int_wo_spikes)]$int,
                                   min(EIC.dt[rt >= rtmin &
                                                rt <= rtmax &
                                                !is.na(int_wo_spikes)]$int),
                                   ifelse(iso.run == "MAiso", Integration_baseL_factor_set, 0.03),
                                   peak_borders = TRUE
                                 )[2]), by = .(idx)], on = .(idx)]


                              #l.peaks[, StartTime := ifelse(is.na(StartTime), as.double(rtmin), as.double(StartTime))]
                              #l.peaks[, EndTime := ifelse(is.na(EndTime), as.double(rtmax), as.double(EndTime))]


                              l.peaks <- l.peaks[!is.na(StartTime) & !is.na(EndTime)]

                              if(nrow(l.peaks) < 1){
                                l.peaks <- NULL
                                Drawer_fill[["peaks"]] <- l.peaks
                                Drawer_closed <- data.frame(Drawer_fill, stringsAsFactors = FALSE)
                                return(Drawer_closed)
                              }
#print(l.peaks)

                              if(manual_bound == FALSE || sum(EIC.dt[rt > l.peaks$StartTime & rt < l.peaks$EndTime]$int) > 0){
                              ##################################
                              #get information on mz peaks for each chromatographic peak
                              ##################################
                                #print("mzs")
                              l.peaks.mz_list <- Get_MZ_list(l.peaks, raw_data, CompCol_xic[i], EIC.dt)
#print("mze")
                              ##################################
                              #add additional variables for each chromatographic peak
                              ##################################
                              l.peaks <- Get_peak_vars(l.peaks, EIC.dt, CompCol_xic[i], l.peaks.mz_list, iso.run, adduct.run, manual_bound)
#print("ve")
#print(l.peaks)
                              }
                            } else {
                              l.peaks <- NULL
                            }

                          } else {
                            l.peaks <- NULL
                          }
                          Drawer_fill[["peaks"]] <- l.peaks
                        }
                        Drawer_closed <- data.frame(Drawer_fill, stringsAsFactors = FALSE)
                        return(Drawer_closed)
                      })
                  }
                  ##################################
                  #first combine peaks from all compounds into one table then peaks from all adducts and isotopologues into one table
                  ##################################
                  if (iso.run == "MAiso") {
                    #print("here")
                    #why <<- Bureau

                    MA.Isos <- rbindlist(Bureau, fill = TRUE, use.names = TRUE)

                    #fwrite(MA.Isos, "maisos1.csv")
#print("maisosstart")
#print(colnames(MA.Isos))
                    if(!("peaks.PpP" %in% colnames(MA.Isos))){
                      MA.Isos[, peaks.PpP := 0]
                    }

                    if(!("peaks.mz_accuracy_ppm" %in% colnames(MA.Isos))){
                      MA.Isos[, peaks.mz_accuracy_ppm := 5000]
                    }
#print("maisosstart2")
#print(colnames(MA.Isos))

                    if(nrow(MA.Isos) > 0 & !is.null(MA.Isos) & "peaks.PpP"  %in% colnames(MA.Isos)){
                      #print("maiso")
                      #print("maisosstart3")
                      #print(colnames(MA.Isos))
                      MA.Isos <- MA.Isos[peaks.PpP >= Min.PointsperPeak & peaks.mz_accuracy_ppm < max.mz.diff_ppm]
                      #print("maisosstart4")
                      #print(colnames(MA.Isos))
                      MA.Isos <- clean_peak_assignments(MA.Isos)
                      #print("maisodone")
                      #print(colnames(MA.Isos))
                      #print(nrow(MA.Isos))
                      #fwrite(MA.Isos, "testMAisos.csv")
                      }
                    if (adduct.run == "screen_adducts"){
                      if(nrow(MA.Isos) > 0 & !is.null(MA.Isos) & "peaks.cor_w_main_add"  %in% colnames(MA.Isos)) MA.Isos <- MA.Isos[peaks.cor_w_main_add >= Min.cor.w.main_adduct]
                    }
                  } else if (iso.run == "LAisos"){
                    #whyLA <<- Bureau
                    LA.Isos <- rbindlist(Bureau, fill = TRUE, use.names = TRUE)
                    if(nrow(LA.Isos) > 0 & !is.null(LA.Isos) & c("peaks.cor_w_M0")  %in% colnames(LA.Isos)) LA.Isos <- LA.Isos[peaks.cor_w_M0 >= Min.cor.w.M0 &
                                                                                                                              peaks.PpP >= Min.PointsperPeak &
                                                                                                                              peaks.mz_accuracy_ppm < max.mz.diff_ppm]
                    if (adduct.run == "main_adduct") {

                      ALL.Isos.perfile <-
                        as.data.table(rbind(MA.Isos, LA.Isos, fill = TRUE))

                    } else {

                      return(as.data.table(
                        rbind(ALL.Isos.perfile, MA.Isos, LA.Isos, fill = TRUE)
                      ))
                    }
                  }
                }
              }
            }

  future::plan("sequential")
#print("out")


  Result <- rbindlist(Output, fill = TRUE, use.names = TRUE)

  ttpp <<- Result
  #get rid of double isos
  Result <- unique(Result, by = c("molecule", "isoab", "adduct", "peaks.M0.grp", "FileName"))

  #get rid of double cross isos
  Result <- clean_peak_assignments(Result)

  #Result <- Result[(!is.na(peaks.FW25M) | !is.na(peaks.unres.e) | !is.na(peaks.unres.s)) & !is.na(peaks.FW75M)]

  Result <- Result[peaks.FW50M > 1.5 * peaks.data_rate]

  Result$IDX <- seq.int(nrow(Result))

  Result <- predict_Iso(Result,
                        "FileName",
                        c("molecule", "adduct", "peaks.M0.grp"),
                        "isoab",
                        flag_extremes = TRUE
  )

  Result <- Result[isoab_ol == FALSE]

  Result <-
    Result[Result[!is.na(peaks.idx) &
                    (isoab == 100 |
                       (peaks.cor_w_M0 > Min.cor.w.M0 & isoab_ol == FALSE)), .(Iso_count = length(unique(isoab))),
                  by = .(molecule, adduct, FileName, peaks.M0.grp)],
           on = .(molecule, adduct, FileName, peaks.M0.grp)]

  Result <- Result[Iso_count >= Min.iso.count]






  #get rid of double peaks
  #Result <- clean_peak_assignments(Result)



  if(return_unsuc_searches == TRUE){
    Result <-
      Result[CompCol_all, on = .(molecule, adduct, isoab, FileName), allow.cartesian = TRUE]
  } else {
    Result <-
      CompCol_all[Result[!is.na(peaks.idx)], on = .(molecule, adduct, isoab, FileName), nomatch = NULL]
  }









  grp_tmp <- Grps[,.(samples_per_group = .N), by =.(sample_group)]
  colnames(grp_tmp)[1] <- "Grp"
  Result <- Result[grp_tmp, on = .(Grp)]

  Result <- Result[!is.na(molecule)]




  Result[, iso_id := paste(round(mz_ex, 4), round(isoab, 2), sep = "_")]

  Result$IDX <- seq.int(nrow(Result))


  if(any(duplicated(Result[, c("molecule", "isoab", "adduct", "FileName")]))){
    Results_ia100 <- Result[isoab == 100]


    Results_ia100 <- Results_ia100[, rt_diff := .(peaks.rt_raw - user.rt), by = .(molecule, adduct, isoab, FileName)]
    Results_ia100 <- Results_ia100[, min_rt_diff := .(min(rt_diff)), by = .(molecule, adduct, isoab, FileName)]
    Results_ia100 <- Results_ia100[rt_diff == min_rt_diff][, !c("rt_diff", "min_rt_diff")]

    Result <- Result[Results_ia100[, c("molecule", "adduct", "FileName", "peaks.M0.grp")], on =.(molecule, adduct, FileName, peaks.M0.grp), nomatch = NULL]

  }

  #print(colnames(Result))

  return(Result)

}
