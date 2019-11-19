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
                           Min.cor.w.M0 = 0.85,
                           Min.iso.count = 2,
                           return_unsuc_searches = FALSE)
{
  CompCol <-
    na.omit(CompCol_all,
            cols = c("eic_mzmin", "eic_mzmax", "StartTime.EIC", "EndTime.EIC"))





  ##################################
  #prepare parallel processing
  ##################################
  doFuture::registerDoFuture()
  future::plan(plan)
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
                                               allow.cartesian = TRUE])
                      } else .CompCol_xic <- data.table(NULL)
                    }

                  } else if (iso.run == "LAisos") {
                    if(nrow(MA.Isos) > 0){
                      if (adduct.run == "main_adduct") {
                        .CompCol_xic <- CompCol[isoabb < 100 &
                                                  FileName == sub(pattern = "(.*)\\..*$",
                                                                  replacement = "\\1",
                                                                  basename(files[file])) &
                                                  adduct == main_adduct]
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
                                             allow.cartesian = TRUE])
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
                      missing = 0
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
                      lapply(seq(length(.ChromData)), function(i,
                                                               CompCol_xic = .CompCol_xic,
                                                               raw_data = .raw_data,
                                                               ChromData = .ChromData) {


                        ##################################
                        #prepare list for collecting information on one peak
                        ##################################
                        Drawer_fill <- as.list(rep(NA, 8))
                        names(Drawer_fill) <-
                          c(
                            "molecule",
                            "adduct",
                            "isoabb",
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
                        Drawer_fill[["FileName"]] <-
                          raw_data@phenoData@data[["sample_name"]]
                        Drawer_fill[["Grp"]] <-
                          raw_data@phenoData@data[["sample_group"]]
                        Drawer_fill[["RT.v"]] <-
                          paste(as.character(unname(ChromData[[i]]@rtime)), collapse = ",")
                        Drawer_fill[["Intensities.v"]] <-
                          paste(as.character(unname(ChromData[[i]]@intensity)), collapse = ",")


                        #print(as.character(CompCol_xic[i]$molecule))
                        #print(as.character(CompCol_xic[i]$adduct))
                        #print(CompCol_xic[i]$isoabb)
                        #print(raw_data@phenoData@data[["sample_name"]])
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
                          if (iso.run == "MAiso" & adduct.run == "main_adduct") {
                            l.peaks <- cutout_peaks(
                              int = EIC.dt[!is.na(int_wo_spikes)]$int_smooth,
                              rt = EIC.dt[!is.na(int_wo_spikes)]$rt,
                              Min.PpP = Min.PointsperPeak,
                              peak.spotting.factor. = peak.spotting.factor,
                              Integration_baseL_factor. = Integration_baseL_factor,
                              Min.Res. = Min.Res
                            )


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
                              main_adduct.grp = NA, #ifelse(iso.run == "MAiso", M0_peaks$peaks.main_adduct.grp, NA),
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
                              l.peaks <- l.peaks[l.peaks[, .(StartTime = as.double(ifelse(
                                !is.na(
                                  GetFWXM(
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
                                  )[1]
                                ),


                                GetFWXM(
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
                                as.double(rtmin)
                              )) ,

                              EndTime = as.double(ifelse(
                                !is.na(
                                  GetFWXM(
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
                                  )[2]
                                ),

                                GetFWXM(
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
                                )[2],
                                as.double(rtmax)
                              ))), by = .(idx)], on = .(idx)]


#print("nexttt:")
#print(l.peaks)
#print(nrow(l.peaks))
#print(apply(l.peaks[, c("rt", "StartTime", "EndTime")], 1, function(peak_row){
#  if(is.na(peak_row[1]) | is.na(peak_row[2] | is.na(peak_row[3]))) {return(peak_row[2])} else if(
#    (peak_row[1] - peak_row[2])/(peak_row[3] - peak_row[1]) > 1.5/1) {return(2*peak_row[1] - peak_row[3])
#  } else {return(peak_row[2])}
#}))
#print(apply(l.peaks[, c("rt", "StartTime", "EndTime")], 1, function(peak_row){
#  if(is.na(peak_row[1]) | is.na(peak_row[2] | is.na(peak_row[3]))) {return(peak_row[3])} else if(
#    (peak_row[3] - peak_row[1])/(peak_row[1] - peak_row[2]) > 1.5/1) {return(2*peak_row[1] - peak_row[2])} else {return(peak_row[3])}
#}))
#                              l.peaks$StartTime <- apply(l.peaks[, c("rt", "StartTime", "EndTime")], 1, function(peak_row){
#                                if(is.na(peak_row[1]) | is.na(peak_row[2] | is.na(peak_row[3]))) {return(peak_row[2])} else if(
#                                  (peak_row[1] - peak_row[2])/(peak_row[3] - peak_row[1]) > 1.5/1) {return(2*peak_row[1] - peak_row[3])
#                                  } else {return(peak_row[2])}
#                                  })

#                              l.peaks$EndTime <- apply(l.peaks[, c("rt", "StartTime", "EndTime")], 1, function(peak_row){
#                                if(is.na(peak_row[1]) | is.na(peak_row[2] | is.na(peak_row[3]))) {return(peak_row[3])} else if(
#                                (peak_row[3] - peak_row[1])/(peak_row[1] - peak_row[2]) > 1.5/1) {return(2*peak_row[1] - peak_row[2])} else {return(peak_row[3])}
#                              })



                              ##################################
                              #extract different variables from detected peaks and add them to the table
                              suppressWarnings(
                              ##################################
                              l.peaks <- l.peaks[l.peaks[, .(
                                PpP = sum(EIC.dt[!is.na(int_wo_spikes) &
                                                   rt >= StartTime &
                                                   rt <= EndTime]$int > 0),

                                mz_accurate = {
                                  suppressWarnings(
                                    raw_data_lim <- raw_data %>%
                                      filterRt(rt = c(StartTime, EndTime)) %>%
                                      filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                                  )

                                  weighted.mean(unlist(mz(raw_data_lim)), unlist(intensity(raw_data_lim)))
                                },

                                mz_accuracy_abs = {
                                  suppressWarnings(
                                    raw_data_lim <- raw_data %>%
                                      filterRt(rt = c(StartTime, EndTime)) %>%
                                      filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                                  )

                                  abs(weighted.mean(unlist(mz(raw_data_lim)), unlist(intensity(raw_data_lim))) - CompCol_xic[i]$mz)
                                },

                                mz_accuracy_ppm = {
                                  suppressWarnings(
                                    raw_data_lim <- raw_data %>%
                                      filterRt(rt = c(StartTime, EndTime)) %>%
                                      filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                                  )

                                  1e6*abs(weighted.mean(unlist(mz(raw_data_lim)), unlist(intensity(raw_data_lim))) - CompCol_xic[i]$mz) / CompCol_xic[i]$mz

                                },

                                mz_span_abs = {
                                  suppressWarnings(
                                    raw_data_lim <- raw_data %>%
                                      filterRt(rt = c(StartTime, EndTime)) %>%
                                      filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                                  )

                                  max(unlist(mz(raw_data_lim))) - min(unlist(mz(raw_data_lim)))
                                },

                                mz_span_ppm = {
                                  suppressWarnings(
                                    raw_data_lim <- raw_data %>%
                                      filterRt(rt = c(StartTime, EndTime)) %>%
                                      filterMz(mz = c(CompCol_xic[i]$eic_mzmin - 0.0001, CompCol_xic[i]$eic_mzmax + 0.0001))
                                  )

                                  1e6*(max(unlist(mz(raw_data_lim))) - min(unlist(mz(raw_data_lim)))) / mean(unlist(mz(raw_data_lim)))
                                },

                                FW25M = as.double(
                                  GetFWXM(
                                    EIC.dt[rt >= rtmin &
                                             rt <= rtmax &
                                             !is.na(int_wo_spikes)]$rt,
                                    EIC.dt[rt >= rtmin &
                                             rt <= rtmax &
                                             !is.na(int_wo_spikes)]$int_smooth,
                                    baseL,
                                    0.25,
                                    return_diff = TRUE
                                  )
                                ),

                                FW50M = as.double(
                                  GetFWXM(
                                    EIC.dt[rt >= rtmin &
                                             rt <= rtmax &
                                             !is.na(int_wo_spikes)]$rt,
                                    EIC.dt[rt >= rtmin &
                                             rt <= rtmax &
                                             !is.na(int_wo_spikes)]$int_smooth,
                                    baseL,
                                    0.50,
                                    return_diff = TRUE
                                  )
                                ),

                                FW75M = as.double(
                                  GetFWXM(
                                    EIC.dt[rt >= rtmin &
                                             rt <= rtmax &
                                             !is.na(int_wo_spikes)]$rt,
                                    EIC.dt[rt >= rtmin &
                                             rt <= rtmax &
                                             !is.na(int_wo_spikes)]$int_smooth,
                                    baseL,
                                    0.75,
                                    return_diff = TRUE
                                  )
                                ),

                                rt.unsm = EIC.dt[rt >= StartTime &
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
                                    use = "complete.obs"
                                  )
                                ), NA)


                              ), by = .(idx)], on = .(idx)]
                              )#}
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
                        return(Drawer_closed)
                      })

                  }
                  ##################################
                  #first combine peaks from all compounds into one table then peaks from all adducts and isotopologues into one table
                  ##################################
                  if (iso.run == "MAiso") {

                    MA.Isos <- data.table::rbindlist(Bureau, fill = TRUE, use.names = TRUE)
                    if(nrow(MA.Isos) > 0 & !is.null(MA.Isos)) MA.Isos[peaks.PpP >= Min.PointsperPeak]
                    if (adduct.run == "screen_adducts"){
                      if(nrow(MA.Isos) > 0 & !is.null(MA.Isos)) MA.Isos <- MA.Isos[peaks.cor_w_main_add >= Min.cor.w.main_adduct & peaks.PpP >= Min.PointsperPeak]
                    }
                  } else if (iso.run == "LAisos"){
                    LA.Isos <- data.table::rbindlist(Bureau, fill = TRUE, use.names = TRUE)
                    if(nrow(LA.Isos) > 0 & !is.null(LA.Isos)) LA.Isos <- LA.Isos[peaks.cor_w_M0 >= Min.cor.w.M0 & peaks.PpP >= Min.PointsperPeak]
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

  #still have to deal with double isotopologues per M0.grp

  Result <- Result[!is.na(peaks.FW50M)]

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

  if(return_unsuc_searches == TRUE){
    Result <-
      Result[CompCol_all, on = .(molecule, adduct, isoabb, FileName), allow.cartesian = TRUE]
  } else {
    Result <-
      CompCol_all[Result[!is.na(peaks.idx)], on = .(molecule, adduct, isoabb, FileName), nomatch = NULL]
  }

  grp_tmp <- Grps[,.(samples_per_group = .N), by =.(sample_group)]
  colnames(grp_tmp)[1] <- "Grp"
  Result <- Result[grp_tmp, on = .(Grp)]

  Result$IDX <- seq.int(nrow(Result))

  return(Result)

}
