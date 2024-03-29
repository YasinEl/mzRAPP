#' find_bench_peaks
#'
#' @description Takes a the output of \code{\link{get_ROIs}} and detects and filters peak candidates.
#'
#' @param files vector with file paths
#' @param Grps data frame with two columns: one for filen ames without .mzML (sample_name) and one for their respective sample group affiliations (sample_group).
#' @param CompCol_all output from function \code{\link{get_ROIs}}
#' @param Min.PointsperPeak minimum number of points per peak for a peak to be considered
#' @param peak.spotting.factor this parameter is ignored when user.rtmin/user.rtmax are given in the CompCol_all table. Relative height to the highest point of the EIC above which points should be considered during peak detection process. e.g. 0.001 corresponds to 0.1\% of the maximum.
#' @param Integration_baseL_factor relative peak height factor upon which points should be considered to be part of the peak. 0.1 would correspond to 10\% of the peak maximum.
#' @param plan see \code{\link{plan}}
#' @param Min.cor.w.main_adduct Minimum pearson correlation coefficient between main_adduct and other adducts for other adducts to be retained
#' @param Min.cor.w.M0 Minimum pearson correlation coefficient between highest isotopologues and lower isotopologues for lower isotopologues to be retained.
#' @param Min.iso.count Minimum number of isotopotologues per compound to be kept in the final output. Has to be more than one.
#' @param remove_isoab_outliers Should isotopologues be removed if they differ from predicted values by more than 35\% (TRUE/FALSE)
#' @param return_unsuc_searches Should unsuccessful searches be returned (TRUE/FALSE)
#' @param max.rt.diff_sec maximum difference between user.rt in position of peak maximum in seconds
#' @param max.mz.diff_ppm maximum difference between intensity weighted mz of a peak and the calculated mz of the expected ion species in ppm
#' @param max_bias_area maximal allowed area bias for isotopologues to be excepted
#' @param max_bias_height maximal allowed height bias for isotopologues to be excepted
#' @param area_height_bias_diff maximal allowed difference between height and area bias
#'
#'
#' @return data table with peak variables extracted from found peaks.
#'
#' @details \strong{molecule:} name of molecule
#' @details \strong{adduct:} adduct type
#' @details \strong{isoab:} theoretic relative abundance as predicted via enviPat
#' @details \strong{FileName:} sample name
#' @details \strong{eic_mzmin:} lowest mz value in extracted ROI
#' @details \strong{eic_mzmax:} highest mz value in extracted ROI
#' @details \strong{formula:} molecular formula
#' @details \strong{charge:} ion charge
#' @details \strong{mz_ex:} exact mass as predicted via enviPat
#' @details \strong{Grp:} sample group
#' @details \strong{peaks.rtmin:} peak start time (s)
#' @details \strong{peaks.rtmax:} peak end time (s)
#' @details \strong{peaks.PpP:} scans per peak
#' @details \strong{peaks.mz_accurate:} peak mz calculated as intensity weighted average
#' @details \strong{peaks.mz_accuracy_abs:} absolute mz accuracy as compared to mz_ex
#' @details \strong{peaks.mz_accuracy_ppm:} relative mz accuracy as compared to mz_ex
#' @details \strong{peaks.mz_span_abs:} absolute difference between the highest and lowest mz value recorded over chromatographic peak
#' @details \strong{peaks.mz_span_ppm:} relative difference between the highest and lowest mz value recorded over chromatographic peak
#' @details \strong{peaks.mz_min:} lowest mz value recorded over chromatographic peak
#' @details \strong{peaks.mz_max:} highest mz value recorded over chromatographic peak
#' @details \strong{peaks.FW25M:} chromatographic peak width at 25% of the maximum (s)
#' @details \strong{peaks.FW50M:} chromatographic peak width at 50% of the maximum (s)
#' @details \strong{peaks.FW75M:} chromatographic peak width at 75% of the maximum (s)
#' @details \strong{peaks.data_rate:} mean differences between scans (s)
#' @details \strong{peaks.rt_raw:} position of the highest intensity (s)
#' @details \strong{peaks.zigZag_IDX:} peak zigzag index as calculated in Zhang, W., Zhao, P.X. Quality evaluation of extracted ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics 15, S5 (2014). 10.1186/1471-2105-15-S11-S5 (R function taken from Chetnik,K. et al. (2020) MetaClean: a machine learning-based classifier for reduced false positive peak detection in untargeted LC–MS metabolomics data. Metabolomics, 16, 117.)
#' @details \strong{peaks.sharpness:} peak sharpness as calculated in Zhang, W., Zhao, P.X. Quality evaluation of extracted ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics 15, S5 (2014). 10.1186/1471-2105-15-S11-S5 (R function taken from Chetnik,K. et al. (2020) MetaClean: a machine learning-based classifier for reduced false positive peak detection in untargeted LC–MS metabolomics data. Metabolomics, 16, 117.)
#' @details \strong{peaks.jaggedness:} peak jaggedness as calculated in Eshghi,S.T. et al. Quality assessment and interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics.  (R function taken from Chetnik,K. et al. (2020) MetaClean: a machine learning-based classifier for reduced false positive peak detection in untargeted LC–MS metabolomics data. Metabolomics, 16, 117.)
#' @details \strong{peaks.symmetry:} peak symmetry as calculated in Eshghi,S.T. et al. Quality assessment and interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics.  (R function taken from Chetnik,K. et al. (2020) MetaClean: a machine learning-based classifier for reduced false positive peak detection in untargeted LC–MS metabolomics data. Metabolomics, 16, 117.)
#' @details \strong{peaks.rt_neighbors:} reports whether extension of integration boundaries in the RT dimension in left or right increases the calculated chromatographic peak area by more than 20%
#' @details \strong{peaks.mz_neighbors:} ratio between the chromatographic peak area and the chromatographic peak area calculated after increasing the width of the mz-extraction window by 4 * max.mz.diff_ppm (worst allowed mass accuracy)
#' @details \strong{peaks.height:} highest intensity of the peak
#' @details \strong{peaks.area:} chromatographic peak area
#' @details \strong{peaks.cor_w_M0:} pearson correlation coefficient between most abundant isotopologue (isoab = 100) and lower isotopologues
#' @details \strong{peaks.cor_w_main_add:} pearson correlation coefficient between most abundant isotopologue of main_adduct (isoab = 100) and most abundant isotopologues of other adducts of the same compound
#' @details \strong{peaks.manual_int:} True if user.rtmin and user.rtmax were provided
#' @details \strong{Intensities.v:} intensity vector of extracted chromatogram
#' @details \strong{RT.v:} retention time vector of extracted chromatogram
#' @details \strong{ExpectedArea:} Predicted chromatrographic peak area for lower isotopologues as calculated from most abundant isotopologue of the same molecule and adduct
#' @details \strong{ErrorRel_A:} relative difference between ExpectedArea and peaks.area
#' @details \strong{ErrorAbs_A:} absolute difference between ExpectedArea and peaks.area
#' @details \strong{ExpectedHeight:} predicted chromatrographic peak height for lower isotopologues as calculated from most abundant isotopologue of the same molecule and adduct
#' @details \strong{ErrorRel_H:} relative difference between ExpectedHeight and peaks.height
#' @details \strong{ErrorAbs_H:} absolute difference between ExpectedHeight and peaks.height
#' @details \strong{isoab_ol:} true if isotopologue abundance is considered to be too far off as compared to predicted isoab
#' @details \strong{Iso_count:} isotopologue count per file, molecule and adduct
#' @details \strong{samples_per_group:} number of samples per group
#' @details \strong{iso_id:} id for specific isotopologue
#' @details \strong{rt_raw_span:} Max RT difference within a given isotopologue of a given molecule and adduct
#'
#' @importFrom data.table data.table as.data.table is.data.table
#'
#' @export

find_bench_peaks <- function(files,
                           Grps,
                           CompCol_all,
                           Min.PointsperPeak = 10,
                           peak.spotting.factor = 0.001,
                           Integration_baseL_factor = 0.1,
                           #Min.Res = 60,
                           plan = "multiprocess",
                           Min.cor.w.main_adduct = 0.8,
                           Min.cor.w.M0 = 0.85,
                           Min.iso.count = 2,
                           remove_isoab_outliers = TRUE,
                           return_unsuc_searches = FALSE,
                           max.rt.diff_sec = 20,
                           max.mz.diff_ppm = 5,
                           max_bias_area = 35,
                           max_bias_height = 30,
                           area_height_bias_diff = 30)
{



  #Conditions upon which to throw error
  if(!isTRUE(is.data.frame(CompCol_all))){stop("CompCol_all has to be a data frame and/or data table!")}
  if(!isTRUE(is.data.table(CompCol_all))){CompCol_all <- as.data.table(CompCol_all)}


  if(!is.character(CompCol_all$molecule)) {CompCol_all$molecule <- as.character(CompCol_all$molecule)}
  if(!isTRUE(is.data.frame(Grps))){stop("Grps has to be a data frame and/or data table!")}
  if(!isTRUE(is.data.table(Grps))){Grps <- as.data.table(CompCol_all)}
  missing_cols <- setdiff(c("sample_name", "sample_group"), colnames(Grps))
  if(length(missing_cols) > 0){stop(paste0("Sample_group table is lacking columns: ", paste(missing_cols, collapse = ", ")))}
  Grps <- Grps[, sample_name := tools::file_path_sans_ext(basename(sample_name))]

  if(length(files[!file.exists(files)] > 0)) stop(paste0("It seems like some of your mzML files do not exist, cannot be accessed or contain spelling errors! Specificly:", paste(files[!file.exists(files)], collapse = ", ")))

  if(length(intersect(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)), Grps$sample_name)) == 0){
    stop("Filenames in your sample_group file are not the same as those of your uploaded mzML files!")
  }





  missing_cols <- setdiff(c("eic_mzmin", "eic_mzmax"), colnames(CompCol_all))
  if(length(missing_cols) > 0){

    CompCol_all[, eic_mzmin := mz_ex - (mz_ex * max.mz.diff_ppm * 1E-6)]
    CompCol_all[, eic_mzmax := mz_ex + (mz_ex * max.mz.diff_ppm * 1E-6)]
    CompCol_all[, FileName := tools::file_path_sans_ext(basename(FileName))]

  }

  missing_cols <- setdiff(c("StartTime.EIC", "EndTime.EIC"), colnames(CompCol_all))

  if(length(missing_cols) > 0 && all(c("user.rtmin", "user.rtmax") %in% colnames(CompCol_all))){
    maxrt <- max(CompCol_all$user.rtmax, na.rm = TRUE)
    CompCol_all[, rownames := rownames(CompCol_all)]
    CompCol_all <- CompCol_all[, .(StartTime.EIC = max(c(1, user.rtmin - (user.rtmax - user.rtmin) * 3)),
                                     EndTime.EIC = min(c(user.rtmax + (user.rtmax - user.rtmin) * 3, maxrt+1))),
                                 by = rownames(CompCol_all)][CompCol_all, on = .(rownames)]

    CompCol_all <- CompCol_all[, !"rownames"]

  }

  if(all(c("user.rtmin", "user.rtmax") %in% colnames(CompCol_all))){
    colnames(CompCol_all)[which(names(CompCol_all) == "user.rtmin")] <- "rtmin"
    colnames(CompCol_all)[which(names(CompCol_all) == "user.rtmax")] <- "rtmax"
  }

  CompCol <-
    stats::na.omit(CompCol_all,
            cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))


  ##################################
  #prepare parallel processing
  ##################################
  doFuture::registerDoFuture()

  if(plan == "multiprocess" && future::supportsMulticore()){
    future::plan(future::multicore)
  } else if(plan == "multiprocess") {
    future::plan(future::multisession)
  } else {
    future::plan(plan)
  }

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
              #3. most abundant isotopologue of other adducts (screen_adducts) if peak has been found in "1." and "2."; 4. less abundant isotopologues of screen adduct if peak has been found in "3."
              ##################################
              for (adduct.run in c("main_adduct", "screen_adducts")) {
                for (iso.run in c("MAiso", "LAisos")) {


                  Bureau <- NULL
                  if(adduct.run == "main_adduct" & iso.run == "MAiso" | nrow(MA.Isos) > 0 & "peaks.PpP" %in% colnames(MA.Isos)){
                    .CompCol_xic <- Limit_Target_list(CompCol, MA.Isos, iso.run, adduct.run, files[file], Min.PointsperPeak)
                  } else .CompCol_xic <- data.table(NULL)


                  if(nrow(.CompCol_xic) > 0){
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
                        Drawer_fill <- as.list(rep(NA, 8))
                        names(Drawer_fill) <-
                          c(
                            "molecule",
                            "adduct",
                            "isoab",
                            #"user.rt",
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

                        #Drawer_fill[["user.rt"]] <- if("user.rt" %in% colnames(CompCol_xic)){CompCol_xic[i]$user.rt}else{NA}

                        Drawer_fill[["FileName"]] <- raw_data@phenoData@data[["sample_name"]]
                        Drawer_fill[["Grp"]] <- raw_data@phenoData@data[["sample_group"]]
                        Drawer_fill[["RT.v"]] <- paste(as.character(unname(ChromData[[i]]@rtime)), collapse = ",")
                        Drawer_fill[["Intensities.v"]] <- paste(as.character(unname(ChromData[[i]]@intensity)), collapse = ",")


                        ##################################
                        #prepare table with smoothed and spike depleted EICs
                        ##################################
                        EIC.dt <- get_EIMatches_BM_NPPpeaks(rt = unname(ChromData[[i]]@rtime),
                                                            int = unname(ChromData[[i]]@intensity),
                                                            Min.PpP = Min.PointsperPeak)


                        if (sum(EIC.dt$int > 0)) {

                          if (iso.run == "LAisos" |
                              (adduct.run == "screen_adducts" & iso.run == "MAiso")) {

                            ##################################
                            #add EIC of highest isotopologue (for lower isotopologues of an adduct) or EIC of main adduct (for most abundant isotopologue of other adducts) to EIC.dt
                            ##################################

                            rt_v_m0 <- as.numeric(unlist(strsplit(
                             MA.Isos[molecule == CompCol_xic[i]$molecule &
                                        adduct == ifelse(iso.run == "MAiso",
                                                         paste0(CompCol_xic[i]$main_adduct),
                                                         paste0(CompCol_xic[i]$adduct)) &
                                        FileName == raw_data@phenoData@data[["sample_name"]]][1]$RT.v,
                              split = ","
                          )))

                            if(nrow(EIC.dt) != length(as.numeric(unlist(strsplit(
                              MA.Isos[molecule == CompCol_xic[i]$molecule &
                                      adduct == ifelse(iso.run == "MAiso",
                                                       paste0(CompCol_xic[i]$main_adduct),
                                                       paste0(CompCol_xic[i]$adduct)) &
                                      FileName == raw_data@phenoData@data[["sample_name"]]][1]$Intensities.v,
                              split = ","
                            )))[!duplicated(rt_v_m0)])){
                              stop(paste0("Some problem with ", CompCol_xic[i]$molecule))
                            }

                            EIC.dt[, M0_int :=
                                     as.numeric(unlist(strsplit(
                                       MA.Isos[molecule == CompCol_xic[i]$molecule &
                                                 adduct == ifelse(iso.run == "MAiso",
                                                                  paste0(CompCol_xic[i]$main_adduct),
                                                                  paste0(CompCol_xic[i]$adduct)) &
                                                 FileName == raw_data@phenoData@data[["sample_name"]]][1]$Intensities.v,
                                       split = ","
                                     )))[!duplicated(rt_v_m0)]]


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


                          manual_bound <- FALSE
                          if (iso.run == "MAiso" & adduct.run == "main_adduct" || nrow(M0_peaks) > 0) {
                            #check for manual boundaries
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
                #            if(manual_bound == FALSE){
                #              l.peaks <- cutout_peaks(
                #                int = EIC.dt[!is.na(int_wo_spikes)]$int,
                #                rt = EIC.dt[!is.na(int_wo_spikes)]$rt,
                #                Min.PpP = Min.PointsperPeak,
                #                peak.spotting.factor. = peak.spotting.factor,
                #                Integration_baseL_factor. = Integration_baseL_factor,
                #                Min.Res. = 1,#Min.Res,
                #                l = if(is.null(CompCol_xic[i]$user.rtmin)){1} else{if(!is.na(as.numeric(CompCol_xic[i]$user.rtmin))){
                #                  which.min(abs(CompCol_xic[i]$user.rtmin - EIC.dt[!is.na(as.numeric(int_wo_spikes))]$rt))}else {1}},
                #                r = if(is.null(CompCol_xic[i]$user.rtmax)){length(EIC.dt[!is.na(int_wo_spikes)]$rt)} else{
                #                  if(!is.na(CompCol_xic[i]$user.rtmax)){
                #                  which.min(abs(CompCol_xic[i]$user.rtmax - EIC.dt[!is.na(int_wo_spikes)]$rt))}else{length(EIC.dt[!is.na(int_wo_spikes)]$rt)}}
                #              )
                #            }

                            ##################################
                            #extract peaks from rt range of justifying peaks (highest isotopologue for lower isotopologues of each adduct; highest isotopologue of main
                            #adduct for other highest isotopologue of other adducts)
                            ##################################
                #          } else if (nrow(M0_peaks) > 0)  {
#
#                            l.peaks <- mapply(
#                              cutout_peaks,
#                              l = sapply(M0_peaks$peaks.StartTime, function(val) {
#                                which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val))
#                              }),
#                              r = sapply(M0_peaks$peaks.EndTime, function(val) {
#                                which.min(abs(EIC.dt[!is.na(int_wo_spikes)]$rt - val))
#                              }),
#                              M0.grp = M0_peaks$peaks.M0.grp, #ifelse(iso.run == "LAisos", paste0(M0_peaks$peaks.M0.grp), NA),
#                              MoreArgs = list(
#                                int = EIC.dt[!is.na(int_wo_spikes)]$int,
#                                rt = EIC.dt[!is.na(int_wo_spikes)]$rt,
#                                Min.PpP = Min.PointsperPeak,
#                                peak.spotting.factor. = peak.spotting.factor,
#                                Integration_baseL_factor. = Integration_baseL_factor,
#                                Min.Res. = 1#Min.Res
#                              ),
#                              SIMPLIFY = FALSE
#                            )
#
#
#                            if (!is.null(l.peaks)) {
#                              l.peaks <- rbindlist(l.peaks, use.names = TRUE)
##                              l.peaks$idx <- 1:nrow(l.peaks)
#                            }
                          }

                          if(!exists("l.peaks")){
                            l.peaks <- data.table(NULL)
                          }


                          if (!is.null(l.peaks)) {
                            if (nrow(l.peaks) > 0 & length(l.peaks) > 1) {
                              ##################################
                              #get start and end time for detected peaks and add them to the table
                              ##################################

                              Integration_baseL_factor_set <- Integration_baseL_factor


                              l.peaks <- l.peaks[l.peaks[, .(
                                StartTime = GetFWXM(
                                     EIC.dt[rt >= rtmin &
                                              rt <= rtmax &
                                              !is.na(int_wo_spikes)]$rt,
                                     EIC.dt[rt >= rtmin &
                                              rt <= rtmax &
                                              !is.na(int_wo_spikes)]$int,
                                     min(EIC.dt[rt >= rtmin &
                                                  rt <= rtmax &
                                                  !is.na(int_wo_spikes)]$int),
                                     Integration_baseL_factor_set,
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
                                   Integration_baseL_factor_set,
                                   peak_borders = TRUE
                                 )[2]), by = .(idx)], on = .(idx)]

                              l.peaks <- l.peaks[!is.na(StartTime) & !is.na(EndTime)]

                              if(nrow(l.peaks) < 1){
                                l.peaks <- NULL
                                Drawer_fill[["peaks"]] <- l.peaks
                                Drawer_closed <- data.frame(Drawer_fill, stringsAsFactors = FALSE)
                                return(Drawer_closed)
                              }

                              if(manual_bound == FALSE || sum(EIC.dt[rt > l.peaks$StartTime & rt < l.peaks$EndTime]$int) > 0){
                              ##################################
                              #get information on mz peaks for each chromatographic peak
                              ##################################
                              l.peaks.mz_list <- Get_MZ_list(l.peaks, raw_data, CompCol_xic[i], EIC.dt, max.mz.diff_ppm)
                              ##################################
                              #add additional variables for each chromatographic peak
                              ##################################
                              l.peaks <- Get_peak_vars(l.peaks, EIC.dt, CompCol_xic[i], l.peaks.mz_list, iso.run, adduct.run, manual_bound)
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
                    MA.Isos <- data.table::rbindlist(Bureau, fill = TRUE, use.names = TRUE)
                    if(!("peaks.PpP" %in% colnames(MA.Isos))){
                      MA.Isos[, peaks.PpP := 0]
                    }

                    if(!("peaks.mz_accuracy_ppm" %in% colnames(MA.Isos))){
                      MA.Isos[, peaks.mz_accuracy_ppm := 5000]
                    }
                    if(nrow(MA.Isos) > 0 & !is.null(MA.Isos) & "peaks.PpP"  %in% colnames(MA.Isos)){
                      MA.Isos <- MA.Isos[peaks.PpP >= Min.PointsperPeak & peaks.mz_accuracy_ppm < max.mz.diff_ppm]
                      MA.Isos <- clean_peak_assignments(MA.Isos)
                      }
                    if (adduct.run == "screen_adducts"){
                      if(nrow(MA.Isos) > 0 & !is.null(MA.Isos) & "peaks.cor_w_main_add"  %in% colnames(MA.Isos)) MA.Isos <- MA.Isos[peaks.cor_w_main_add >= Min.cor.w.main_adduct]
                    }
                  } else if (iso.run == "LAisos"){
                    LA.Isos <- data.table::rbindlist(Bureau, fill = TRUE, use.names = TRUE)
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


  Result <- data.table::rbindlist(Output, fill = TRUE, use.names = TRUE)
  Result <- unique(Result, by = c("molecule", "isoab", "adduct", "peaks.M0.grp", "FileName"))



  if(!("peaks.FW50M" %in% colnames(Result))){stop("No peaks were confirmed for benchmark!")}

  #get rid of double cross isos
  Result <- clean_peak_assignments(Result)
  Result$IDX <- seq.int(nrow(Result))
  Result <- predict_Iso(Result,
                        "FileName",
                        c("molecule", "adduct", "peaks.M0.grp"),
                        "isoab",
                        flag_extremes = TRUE,
                        max_bias_area = max_bias_area,
                        max_bias_height = max_bias_height,
                        area_height_bias_diff = area_height_bias_diff
  )

  if(remove_isoab_outliers == TRUE){
    Result <- Result[isoab_ol == FALSE]
  }

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



  #Remove Features if only observed in one sample (if more than one sample is present in benchmark)
  if(length(unique(Result$FileName)) > 1){
    Result <- Result[, feature_id := .GRP, by = c('molecule', 'adduct', 'isoab')]
    Result[, ft_count := .N, by = .(feature_id)]
    Result <- Result[ft_count > 1, !c("ft_count", "feature_id")]
  }

  #Remove molecules for which only one isotopologue is present
  Result[, iso_count2 := .N, by =.(molecule, adduct, FileName)]
  Result <- Result[iso_count2 > 1, !c("iso_count2")]

  #Remove molecules if most abundant isotopologue is not present
  Result[, maIso := any(isoab == "100"), by =.(molecule, adduct, FileName)]
  Result <- Result[maIso == TRUE, !c("maIso")]





  Result$IDX <- seq.int(nrow(Result))


  if(any(duplicated(Result[, c("molecule", "isoab", "adduct", "FileName")]))){
    Results_ia100 <- Result[isoab == 100]


    Results_ia100 <- Results_ia100[, rt_diff := .(peaks.rt_raw - user.rt), by = .(molecule, adduct, isoab, FileName)]
    Results_ia100 <- Results_ia100[, min_rt_diff := .(min(rt_diff)), by = .(molecule, adduct, isoab, FileName)]
    Results_ia100 <- Results_ia100[rt_diff == min_rt_diff][, !c("rt_diff", "min_rt_diff")]

    Result <- Result[Results_ia100[, c("molecule", "adduct", "FileName", "peaks.M0.grp")], on =.(molecule, adduct, FileName, peaks.M0.grp), nomatch = NULL]

  }

  Result[, rt_raw_span := .(max(peaks.rt_raw) - min(peaks.rt_raw)), by = .(molecule, adduct, isoab)]

    sort_vct <- intersect(colnames(Result),
                          c("IDX",	"molecule",	"adduct",	"main_adduct",	"isoab",	"FileName",	"Grp",	"samples_per_group",
                            "formula",	"charge",	"Iso_count",	"mz_ex",	"peaks.idx",	"peaks.rtmin",	"peaks.rtmax",
                            "peaks.M0.grp",	"peaks.StartTime",	"peaks.EndTime",	"peaks.PpP",	"peaks.mz_accurate",
                            "peaks.mz_accuracy_abs",	"peaks.mz_accuracy_ppm",	"peaks.mz_span_abs",	"peaks.mz_span_ppm",
                            "peaks.mz_min",	"peaks.mz_max",	"peaks.FW25M",	"peaks.FW50M",	"peaks.FW75M",	"peaks.data_rate", "peaks.rt_weig",
                            "peaks.rt_raw", "peaks.symmetry", "peaks.jaggedness",	"peaks.zigZag_IDX",	"peaks.sharpness",	"peaks.height",	"peaks.area",
                            "peaks.cor_w_M0",	"peaks.cor_w_main_add",	"peaks.manual_int", "rt_raw_span", "peaks.rt_neighbors", "peaks.mz_neighbors",
                            "ExpectedArea",	"ErrorRel_A", "ErrorAbs_A",	"ExpectedHeight",	"ErrorRel_H",	"ErrorAbs_H",	"isoab_ol",	"Intensities.v",	"RT.v")
    )

    data.table::setcolorder(Result, c(sort_vct, setdiff(colnames(Result), sort_vct)))





  return(Result)

}
