#' findBenchPeaks
#'
#' @description Takes a the output of \code{\link{getROIsForEICs}} and detects and filters peak candidates.
#'
#' @param files vector with file paths
#' @param Grps data frame with two columns: one for filenames without .mzML (sample_name) and one for their respective sample group affiliations (sample_group).
#' @param CompCol_all output from function \code{\link{getROIsForEICs}}
#' @param Min.PointsperPeak minimum number of points per peak for a peak to be considered
#' @param peak.spotting.factor relative height to the heighest point of the EIC above which points should be considered. e.g. 0.001 corresponds to 0.1\% of the maximum.
#' @param Integration_baseL_factor h
#' @param plan see \code{\link{plan}}
#' @param Min.cor.w.main_adduct Minimum pearson correlation coefficient between main_adduct and other adducts for other adducts to be considered
#' @param Min.cor.w.M0 Minimum pearson correlation coefficient between higher isotopologues and lower isotopologues for lower isotopologues to be considered
#' @param Min.iso.count Minimum number of isotopotologues per compound to be kept in the final output
#' @param return_unsuc_searches Should unsuccsessfull searches be returned (TRUE/FALSE)
#' @param Min.Res At which maximum height (percentage; measured relative to the lower peaks maximum) should to chromatographic peaks be resolved for them to be considered as seperate peaks
#'
#'
#' @import foreach
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



  if(length(files) < 2){stop("Please provide more than 1 mzML file.")}
  if(!isTRUE(is.data.frame(CompCol_all))){stop(paste0("CompCol_all has to be a data frame and/or data table!"))}
  if(!isTRUE(is.data.table(CompCol_all))){CompCol_all <- as.data.table(CompCol_all)}


  if(!is.character(CompCol_all$molecule)) {CompCol_all$molecule <- as.character(CompCol_all$molecule)}
  if(!isTRUE(is.data.frame(Grps))){stop(paste0("Grps has to be a data frame and/or data table!"))}
  if(!isTRUE(is.data.table(Grps))){Grps <- as.data.table(CompCol_all)}
  missing_cols <- setdiff(c("sample_name", "sample_group"), colnames(Grps))
  if(length(missing_cols) > 0){stop(paste0("Sample_group table is lacking columns: ", paste(missing_cols, collapse = ", ")))}
  Grps <- Grps[, sample_name := tools::file_path_sans_ext(basename(sample_name))]

  if(length(files[!file.exists(files)] > 0)) stop(paste0("It seems like some of your mzML files do not exist, cannot be accessed or contain spelling errors! Specificly:", paste(files[!file.exists(files)], collapse = ", ")))





  CompCol <-
    na.omit(CompCol_all,
            cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))





  ##################################
  #prepare parallel processing
  ##################################
  doFuture::registerDoFuture()
  future::plan(plan)
  #future::plan(list("sequential", "multiprocess"))
  #future::plan(multiprocess(workers = 40))
  Output <- list()
  Output <-
    foreach(file = 1:length(files),
            .packages = c("mzRAPP", "data.table")) %dopar% {
  #                        for(file in 1:length(files)){


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

              ##################################
              #setup order of EIC evaluation: 1. most abundant isotopologue (MAiso) of main adduct (main_adduct); 2. less abundant isotopologues (LAisos) of main adduct if peak has been found in "1.";
              #3. most abundant isotopologue of other adducts (screen_adducts) if peak has been found in "1."; 4. less abundant isotopologues of main adduct if peak has been found in "3."
              ##################################
              for (adduct.run in c("main_adduct", "screen_adducts")) {
                for (iso.run in c("MAiso", "LAisos")) {



                  Bureau <- NULL

                  if (iso.run == "MAiso") {
                    if (adduct.run == "main_adduct") {
                      .CompCol_xic <- CompCol[isoabb == 100 &
                                                FileName == sub(pattern = "(.*)\\..*$",
                                                                replacement = "\\1",
                                                                basename(files[file])) &
                                                adduct == main_adduct]

                    } else if (adduct.run == "screen_adducts") {
                      if(nrow(ALL.Isos.perfile) > 0){
                        .CompCol_xic <- CompCol[isoabb == 100 &
                                                  FileName == sub(pattern = "(.*)\\..*$",
                                                                  replacement = "\\1",
                                                                  basename(files[file])) &
                                                  adduct != main_adduct]

                        .CompCol_xic <-
                          na.omit(.CompCol_xic[unique(MA.Isos[peaks.PpP > Min.PointsperPeak &
                                                                !is.na(peaks.PpP), c("molecule", "FileName")]),
                                               on = .(molecule, FileName),
                                               allow.cartesian = TRUE], cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))
                      } else .CompCol_xic <- data.table(NULL)
                    }

                  } else if (iso.run == "LAisos") {
                    if(nrow(MA.Isos) > 0){
                      if (adduct.run == "main_adduct") {
                        .CompCol_xic <- na.omit(CompCol[isoabb < 100 &
                                                  FileName == sub(pattern = "(.*)\\..*$",
                                                                  replacement = "\\1",
                                                                  basename(files[file])) &
                                                  adduct == main_adduct], cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))
                        #tt <- .CompCol_xic
                        #return(tt)
                      } else {
                        .CompCol_xic <- CompCol[isoabb < 100 &
                                                  FileName == sub(pattern = "(.*)\\..*$",
                                                                  replacement = "\\1",
                                                                  basename(files[file])) &
                                                  adduct != main_adduct]
                      }

                      .CompCol_xic <-
                        na.omit(.CompCol_xic[unique(MA.Isos[peaks.PpP > Min.PointsperPeak &
                                                              !is.na(peaks.PpP), c("molecule", "adduct", "FileName")]),
                                             on = .(molecule, adduct, FileName),
                                             allow.cartesian = TRUE], cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))
                    } else .CompCol_xic <- data.table(NULL)
                  }



                  if(nrow(.CompCol_xic) > 0){

                    ##################################
                    #extract EICs, add according file names and create empty list for storage of peak information
                    ##################################
                    .ChromData <- MSnbase::chromatogram(
                      .raw_data,
                      rt = as.matrix(unname(.CompCol_xic[, c("StartTime.EIC", "EndTime.EIC")])),
                      mz = as.matrix(unname(.CompCol_xic[, .(MinMz = eic_mzmin - 0.0001,
                                                             MaxMz = eic_mzmax + 0.0001)][, .(MinMz, MaxMz)])),
                      missing = 0,
                      msLevel = 1L,
                      aggregationFun = "max"#,
                      #BPPARAM = SnowParam(detectCores()-1)
                    )

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
                    future.apply::future_lapply(seq(length(.ChromData)), function(i,
                                                               CompCol_xic = .CompCol_xic,
                                                               raw_data = .raw_data,
                                                               ChromData = .ChromData) {

                        ##################################
                        #prepare list for collecting information on one peak
                        ##################################
                        Drawer_fill <- as.list(rep(NA, 9))
                        names(Drawer_fill) <-
                          c(
                            "molecule",
                            "adduct",
                            "isoabb",
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
                        Drawer_fill[["isoabb"]] <- CompCol_xic[i]$isoabb
                        Drawer_fill[["user.rt"]] <- CompCol_xic[i]$user.rt
                        Drawer_fill[["FileName"]] <- raw_data@phenoData@data[["sample_name"]]
                        Drawer_fill[["Grp"]] <- raw_data@phenoData@data[["sample_group"]]
                        Drawer_fill[["RT.v"]] <- paste(as.character(unname(ChromData[[i]]@rtime)), collapse = ",")
                        Drawer_fill[["Intensities.v"]] <- paste(as.character(unname(ChromData[[i]]@intensity)), collapse = ",")

                        ##################################
                        #prepare table with smoothed an spike depleted EICs
                        ##################################
                        EIC.dt <- mzRAPP:::get_EIC_table(rt = unname(ChromData[[i]]@rtime),
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
                                                                  paste0(CompCol_xic[i]$main_adduct), #paste0(main_adduct),######################
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
                                                                   paste0(CompCol_xic[i]$main_adduct),#paste0(main_adduct),
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

                            #automatic boundary imputation
                            if(manual_bound == FALSE){
                              l.peaks <- cutout_peaks(
                                int = EIC.dt[!is.na(int_wo_spikes)]$int_smooth,
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
                                ifelse(which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) - 1 <= 1,
                                       1,
                                       which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) - 1)
                              }),
                              r = sapply(M0_peaks$peaks.EndTime, function(val) {
                                ifelse(
                                  which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) + 1 >= length(EIC.dt[!is.na(int_wo_spikes)]$rt),
                                  length(EIC.dt[!is.na(int_wo_spikes)]$rt),
                                  which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val)) + 1
                                )
                              }),
                              M0.grp = M0_peaks$peaks.M0.grp, #ifelse(iso.run == "LAisos", paste0(M0_peaks$peaks.M0.grp), NA),
                              #main_adduct.grp = NA, #ifelse(iso.run == "MAiso", M0_peaks$peaks.main_adduct.grp, NA),
                              MoreArgs = list(
                                int = EIC.dt[!is.na(int_wo_spikes)]$int_smooth,
                                rt = EIC.dt[!is.na(int_wo_spikes)]$rt,
                                Min.PpP = Min.PointsperPeak,
                                peak.spotting.factor. = peak.spotting.factor,
                                Integration_baseL_factor. = Integration_baseL_factor,
                                Min.Res. = Min.Res
                              ),
                              SIMPLIFY = FALSE
                            )


                            if (!is.null(l.peaks)) {
                              l.peaks <- data.table::rbindlist(l.peaks, use.names = TRUE)
                              l.peaks$idx <- 1:nrow(l.peaks)
                            }
                          } else if (nrow(M0_peaks) == 0) {
                            l.peaks <- NULL
                          }

                          if (!is.null(l.peaks)) {
                            if (nrow(l.peaks) > 0 & length(l.peaks) > 1) {

                              ##################################
                              #get start and end time for detected peaks at height of integration baseline and add them to the table
                              ##################################

                              if (iso.run == "MAiso" & adduct.run == "main_adduct" & manual_bound == TRUE){

                                l.peaks <- l.peaks[, StartTime = rtmin]
                                l.peaks <- l.peaks[, EndTime = rtmax]

                              } else{
                               l.peaks <- l.peaks[l.peaks[, .(StartTime = GetFWXM(
                                     EIC.dt[rt >= rtmin &
                                              rt <= rtmax &
                                              !is.na(int_wo_spikes)]$rt,
                                     EIC.dt[rt >= rtmin &
                                              rt <= rtmax &
                                              !is.na(int_wo_spikes)]$int_smooth,
                                     min(EIC.dt[rt >= rtmin &
                                                  rt <= rtmax &
                                                  !is.na(int_wo_spikes)]$int_smooth),
                                     ifelse(iso.run == "MAiso", Integration_baseL_factor, 0),
                                     peak_borders = TRUE
                                   )[1],
                                 EndTime = GetFWXM(
                                   EIC.dt[rt >= rtmin &
                                            rt <= rtmax &
                                            !is.na(int_wo_spikes)]$rt,
                                   EIC.dt[rt >= rtmin &
                                            rt <= rtmax &
                                            !is.na(int_wo_spikes)]$int_smooth,
                                   min(EIC.dt[rt >= rtmin &
                                                rt <= rtmax &
                                                !is.na(int_wo_spikes)]$int_smooth),
                                   ifelse(iso.run == "MAiso", Integration_baseL_factor, 0),
                                   peak_borders = TRUE
                                 )[2]), by = .(idx)], on = .(idx)]
                              }

                              l.peaks[, StartTime := ifelse(is.na(StartTime), as.double(rtmin), as.double(StartTime))]
                              l.peaks[, EndTime := ifelse(is.na(EndTime), as.double(rtmax), as.double(EndTime))]


                              suppressWarnings(
                                raw_data_lim <- raw_data %>%
                                  xcms::filterRt(rt = c(min(l.peaks$StartTime), max(l.peaks$EndTime))) %>%
                                  xcms::filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                              )

                              l.peaks.mz_list <- list()
                              length(l.peaks.mz_list) <- nrow(l.peaks)
                              nc <- 1

                              while(nc <= nrow(l.peaks)){

                                suppressWarnings(
                                  raw_data_lim1 <- raw_data_lim %>%
                                    xcms::filterRt(rt = unlist(unname(l.peaks[nc, c("StartTime", "EndTime")]))) #%>%
                                  #filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                                )

                                suppressWarnings(
                                l.peaks.mz_list[[nc]] <- list(mz = xcms::mz(raw_data_lim1),
                                                              int = xcms::intensity(raw_data_lim1))

                                )

                                if(length(l.peaks.mz_list[[nc]][["mz"]]) > 1){
                                  highstInt_idx <-
                                    mapply(which.max,
                                           l.peaks.mz_list[[1]][["int"]],
                                           SIMPLIFY = FALSE
                                    )


                                  l.peaks.mz_list[[nc]][["int"]] <-
                                    lapply(1:length(highstInt_idx),
                                           function(x,
                                                    li = l.peaks.mz_list[[1]][["int"]],
                                                    idx = highstInt_idx){
                                             return(li[[x]][idx[[x]]])
                                           })
                                  l.peaks.mz_list[[nc]][["mz"]] <-
                                    lapply(1:length(highstInt_idx),
                                           function(x,
                                                    li = l.peaks.mz_list[[1]][["mz"]],
                                                    idx = highstInt_idx){
                                             return(li[[x]][idx[[x]]])
                                           })
                                }
                                nc <- nc + 1
                              }

                             #assign("list_for_testing", l.peaks.mz_list, envir = .GlobalEnv)

                              tryCatch(

                              ##################################
                              #extract different variables from detected peaks and add them to the table
                              suppressWarnings(
                              ##################################
                              l.peaks <- l.peaks[l.peaks[, .(
                                PpP = sum(EIC.dt[!is.na(int_wo_spikes) &
                                                   rt >= StartTime &
                                                   rt <= EndTime]$int > 0),

                                mz_accurate = weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])),

                                mz_accuracy_abs = abs(weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])) - CompCol_xic[i]$mz),

                                mz_accuracy_ppm = 1e6*abs(weighted.mean(unlist(l.peaks.mz_list[[idx]][["mz"]]), unlist(l.peaks.mz_list[[idx]][["int"]])) - CompCol_xic[i]$mz) / CompCol_xic[i]$mz,

                                mz_span_abs = max(unlist(l.peaks.mz_list[[idx]][["mz"]])) - min(unlist(l.peaks.mz_list[[idx]][["mz"]])),

                                mz_span_ppm = 1e6*(max(unlist(l.peaks.mz_list[[idx]][["mz"]])) - min(unlist(l.peaks.mz_list[[idx]][["mz"]]))) / mean(unlist(l.peaks.mz_list[[idx]][["mz"]])),

                                mz_min = min(unlist(l.peaks.mz_list[[idx]][["mz"]]), na.rm = TRUE),

                                mz_max = max(unlist(l.peaks.mz_list[[idx]][["mz"]]), na.rm = TRUE),

                                #mz_interference = {

                                 # #mz_min_t <- CompCol_xic[i]$mz_acc - (CompCol_xic[i]$mz_acc - CompCol_xic[i]$eic_mzmin) * 3
                                #  #mz_max_t <- CompCol_xic[i]$mz_acc + abs(CompCol_xic[i]$mz_acc - CompCol_xic[i]$eic_mzmax) * 3

                                 # rdl_extended1 <- rdl_extended %>%
                                #    filterRt(rt = c(StartTime, EndTime)) #%>%
                                    #filterMz(mz = c(mz_min_t, mz_max_t))


                                 # suppressWarnings(
                                #    raw_data_lim1 <- raw_data_lim %>%
                                ##      filterRt(rt = c(StartTime, EndTime)) #%>%
                                    #filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                              #    )



                                  #highest_mp <- sum(unlist(lapply(intensity(raw_data_lim), max)))
                                  #summed_mp <- sum(unlist(lapply(intensity(rdl_extended), sum)))

                                  #summed_mp > 2 * highest_mp


                                #  suppressWarnings(
                                #    EIC.spec_targets <- lapply(seq(length(raw_data_lim1)), function(x,
                                #                                                                   mz_lim = mz(raw_data_lim1),
                                #                                                                   int_lim = intensity(raw_data_lim1),
                                #                                                                   mz_lim_ext = mz(rdl_extended1),
                                #                                                                   int_lim_ext = intensity(rdl_extended1)){

                                #      if(length(int_lim[[x]]) == 0){return(c(mz = 0, int = 0, mz_if = 0, int_if = 0))}

                              #        wmi <- which.max(int_lim[[x]])
                              #        mz_val <- mz_lim[[x]][wmi]
                              #        int_val <- max(int_lim[[x]])#

                                #      if(int_val >= max(int_lim_ext[[x]])){return(c(mz = mz_val,int = int_val, mz_if = 0, int_if = 0))}

                                 #     wmi_ext <- which.max(int_lim_ext[[x]])
                                #      mz_val_ext <- mz_lim_ext[[x]][wmi_ext]
                                #      int_val_ext <- max(int_lim_ext[[x]])

                                #      return(c(mz = mz_val, int = int_val, mz_if = mz_val_ext, int_if = int_val_ext))

                                #    })
                                #  )

                              #    interference_table <- as.data.table(do.call(rbind, EIC.spec_targets))

                               #   if(sum(interference_table$int) * 2 < sum(interference_table$int_if)){
                              #      interference_table$delta_mz <- abs(interference_table$mz - interference_table$mz_if)
                              #      as.double(min(interference_table$delta_mz, na.rm = TRUE))
                              #    } else as.double(NA)

                               # },

                                FW25M = as.double(
                                  GetFWXM(
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             !is.na(int_wo_spikes)]$rt,
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             !is.na(int_wo_spikes)]$int,
                                    0,
                                    0.25,
                                    return_diff = TRUE
                                  )
                                ),

                                FW50M = as.double(
                                  GetFWXM(
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             !is.na(int_wo_spikes)]$rt,
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             !is.na(int_wo_spikes)]$int,
                                    0,
                                    0.50,
                                    return_diff = TRUE
                                  )
                                ),

                                FW75M = as.double(
                                  GetFWXM(
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             !is.na(int_wo_spikes)]$rt,
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime &
                                             !is.na(int_wo_spikes)]$int,
                                    0,
                                    0.75,
                                    return_diff = TRUE
                                  )
                                ),

                              data_rate = mean(diff(EIC.dt[rt >= StartTime &
                                                   rt <= EndTime]$rt)),

                              rt_raw = EIC.dt[rt >= StartTime &
                                                   rt <= EndTime &
                                                   int == max(EIC.dt[rt >= StartTime &
                                                                       rt <= EndTime]$int)]$rt,

                                zigZag_IDX = as.double(GetZigzagIDX(
                                  EIC.dt[rt >= StartTime &
                                           rt <= EndTime]$int,
                                  max(EIC.dt[rt >= StartTime &
                                               rt <= EndTime]$int)
                                )),

                                sharpness = as.double(GetSharpness(EIC.dt[rt >= StartTime &
                                                                            rt <= EndTime]$int)),

                                height = max(EIC.dt[rt >= StartTime &
                                                      rt <= EndTime]$int),

                                area = DescTools::AUC(
                                  c(StartTime,
                                    EIC.dt[rt > StartTime &
                                             rt < EndTime]$rt,
                                    EndTime),
                                  c(0,
                                    EIC.dt[rt > StartTime &
                                             rt < EndTime]$int,
                                    0),
                                  method = "trapezoid"
                                ),

                                cor_w_M0 = ifelse(iso.run == "LAisos", suppressWarnings(
                                  cor(
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime]$int,
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime]$M0_int,
                                    method = "pearson",
                                    use = "complete.obs"
                                  )
                                ), NA),

                                cor_w_main_add = ifelse(iso.run == "MAiso" & adduct.run == "screen_adducts", suppressWarnings(
                                  cor(
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime]$int,
                                    EIC.dt[rt >= StartTime &
                                             rt <= EndTime]$M0_int,
                                    method = "pearson",
                                    use = "complete.obs",
                                  )
                                ), NA),

                              manual_int = manual_bound


                              ), by = .(idx)], on = .(idx)]
                              ), error = function(e) {assign("table_for_testing", EIC.dt, envir = .GlobalEnv)
                                print(l.peaks)
                                print(paste0(Drawer_fill[["molecule"]]) )
                              })#}


                            } else {
                              l.peaks <- NULL
                            }

                          } else {
                            l.peaks <- NULL
                          }

                          Drawer_fill[["peaks"]] <- l.peaks
                        }
                        Drawer_closed <-
                          data.frame(Drawer_fill, stringsAsFactors = FALSE)
                        return(Drawer_closed)#,
                        #error = function(e) print(e))
                        ##include error
                      })

                  }
                  ##################################
                  #first combine peaks from all compounds into one table then peaks from all adducts and isotopologues into one table
                  ##################################
                  if (iso.run == "MAiso") {

                    MA.Isos <- data.table::rbindlist(Bureau, fill = TRUE, use.names = TRUE)
                    if(nrow(MA.Isos) > 0 & !is.null(MA.Isos) & "peaks.PpP"  %in% colnames(MA.Isos)){
                      MA.Isos <- clean_peak_assignments(MA.Isos[peaks.PpP >= Min.PointsperPeak & peaks.mz_accuracy_ppm < max.mz.diff_ppm])
                    }
                    if (adduct.run == "screen_adducts"){
                      if(nrow(MA.Isos) > 0 & !is.null(MA.Isos) & "peaks.cor_w_main_add"  %in% colnames(MA.Isos)) MA.Isos <- MA.Isos[peaks.cor_w_main_add >= Min.cor.w.main_adduct]
                    }
                  } else if (iso.run == "LAisos"){
                    LA.Isos <- data.table::rbindlist(Bureau, fill = TRUE, use.names = TRUE)
                    if(nrow(LA.Isos) > 0 & !is.null(LA.Isos) & "peaks.cor_w_M0"  %in% colnames(LA.Isos)) LA.Isos <- LA.Isos[peaks.cor_w_M0 >= Min.cor.w.M0 &
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



  Result <- data.table::rbindlist(Output, fill = TRUE, use.names = TRUE)


  #get rid of double isos
  Result <- unique(Result, by = c("molecule", "isoabb", "adduct", "peaks.M0.grp", "FileName"))

  #get rid of double cross isos
  Result <- clean_peak_assignments(Result)

  #Result <- Result[(!is.na(peaks.FW25M) | !is.na(peaks.unres.e) | !is.na(peaks.unres.s)) & !is.na(peaks.FW75M)]

  Result <- Result[peaks.FW50M > 1.5 * peaks.data_rate]

  Result$IDX <- seq.int(nrow(Result))

  Result <- predict_Iso(Result,
                        "FileName",
                        c("molecule", "adduct", "peaks.M0.grp"),
                        "isoabb",
                        flag_extremes = TRUE
  )

  Result <- Result[isoabb_ol == FALSE]

  Result <-
    Result[Result[!is.na(peaks.idx) &
                    (isoabb == 100 |
                       (peaks.cor_w_M0 > Min.cor.w.M0 & isoabb_ol == FALSE)), .(Iso_count = length(unique(isoabb))),
                  by = .(molecule, adduct, FileName, peaks.M0.grp)],
           on = .(molecule, adduct, FileName, peaks.M0.grp)]

  Result <- Result[Iso_count >= Min.iso.count]






  #get rid of double peaks
  #Result <- clean_peak_assignments(Result)



  if(return_unsuc_searches == TRUE){
    Result <-
      Result[CompCol_all, on = .(molecule, adduct, isoabb, FileName), allow.cartesian = TRUE]
  } else {
    Result <-
      CompCol_all[, !"user.rt"][Result[!is.na(peaks.idx)], on = .(molecule, adduct, isoabb, FileName), nomatch = NULL]
  }









  grp_tmp <- Grps[,.(samples_per_group = .N), by =.(sample_group)]
  colnames(grp_tmp)[1] <- "Grp"
  Result <- Result[grp_tmp, on = .(Grp)]







  Result$IDX <- seq.int(nrow(Result))

  return(Result)

}
