#' @title LazyPeakIntegration_par
#' @description Takes a list of named m/z values and rough integration bondaries in the retention time dimension. Chromatograms implied are
#' extracted from a list of mzML files. A peak is picked from each chromatogram. Multiple variables are extracted from each peak...
#'
#' @param files vector containing mzML file paths
#' @param Grps dataframe with two columns; one with file name, one with sample group
#' @param CompCol dataframe containing names, sumformulas, m/z values and integration boundaries
#' @param Min.PpP minimum number of points per peak for a peak candidate
#' @param ppm maxmimum m/z deviation from the theortical mass
#' @param Nr.of.acceptedMissingScans maxmium number of consecutive missing data points (see \code{\link{GetConsecScanBorders}})
#' @param Cluster number of cores to be used in parallel
#'
#' @return The output from \code{\link{rbindlist}}
#' @export
#'
#' @import data.table
#' @import foreach
#' @importFrom BiocParallel SnowParam
#' @importFrom parallel detectCores
#' @importFrom MSnbase readMSData chromatogram sampleNames
#' @importFrom DescTools AUC IsOdd
#' @importFrom signal sgolayfilt
#' @importFrom stats weighted.mean
#' @examples
#' \dontrun{
#' peak_candidadtes <- LazyPeakIntegration(files, Grps_all, CompColred, 10, 5, 2)
#' }
#'
LazyPeakIntegration_par <- function(files, Grps, CompCol_all, Min.PpP = 5, ppm, Nr.of.acceptedMissingScans, PredictedRT = FALSE, Cluster = makeCluster(detectCores() - 2))
{


  CompCol <- na.omit(CompCol_all, cols = c("eic_mzmin", "eic_mzmax", "StartTime.XIC", "EndTime.XIC"))

  cl <- Cluster

  registerDoParallel(cl)



#  registerDoSEQ()

  Output <- foreach(file = 1:length(files), .packages = c("lazypeaks", "data.table")) %dopar%{


#  for(file in 1:length(files)){


    #read mzML files
    raw_data <- readMSData(files = files[file],
                           pdata = new("NAnnotatedDataFrame", Grps[sample_name == sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files[file]))]),
                           msLevel. = 1,
                           mode = "onDisk")


    #initiate fractioned extraction of mz traces: First most abundant isotopologues (xx=1); Second less abundand isotopologues if most abundant one was found before (xx=2)
    for(xx in 1:2){ #1. run for extraction of most abundand isotopologues, second for other isotopologues

      if(xx == 1){
        CompCol_xic <- CompCol[isoabb == 100 & FileName == sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files[file]))]

      } else{


        CompCol_xic <- CompCol[isoabb < 100 & FileName == sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files[file]))]


        CompCol_xic <- na.omit(CompCol_xic[MA.Isos[PpP > 0 & !is.na(PpP), c("molecule", "adduct")],
                                           on =.(molecule, adduct),
                                           allow.cartesian= TRUE])

        }

#write.csv(CompCol_xic, paste0("file", file, "_xx", xx, "compcol.csv"))
      #extract chromatograms
      ChromData <- chromatogram(raw_data,
                                rt = as.matrix(unname(CompCol_xic[, c("StartTime.XIC", "EndTime.XIC")])),
                                mz = as.matrix(unname(CompCol_xic[, .(MinMz = eic_mzmin - 0.0001,
                                                                      MaxMz = eic_mzmax + 0.0001)][,.(MinMz, MaxMz)])),
                                missing = 0)#,
#########
#return(ChromData)
      ##########
      #add file names
      sampleNames(ChromData) <- basename(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files[file])))


      #prepare empty list to store variables to be extracted from all peaks
      Bureau <- as.list(seq_len(nrow(CompCol_xic)))


      #loop through chromatograms to pick peaks and extract different variables from them
      for(i in 1:length(ChromData)){

        #prepare list to store variables from one peak
        Drawer_fill <- as.list(rep(NA, 32))


        #name variable-slots in prepared list
        names(Drawer_fill) <- c("molecule", "adduct", "isoabb", "FileName", "Grp", "RT", "StartTime", "EndTime", "BaseLine", "SpikeIndicator", "PpP", "trend.left", "trend.right", "trend.left.f", "trend.right.f", "Feature", "Degenerate",
                                "Height", "FW10M", "FW25M", "FWHM", "FW75M", "Area", "Zigzag_idx", "Sharpness", "Spike_peak", "MissingBase", "Second_Iso","Double_peak", "Break_point", "Intensities.v", "RT.v")


        #variable: index number of peak
        #Drawer_fill[["IDX"]] <- i #+ nrow(MA.Isos)# + nrow(Output)

        #variable: molecule name
        Drawer_fill[["molecule"]] <- unlist(unname(CompCol_xic[i, "molecule"]))

        #variable: adduct
        Drawer_fill[["adduct"]] <- unlist(unname(CompCol_xic[i, "adduct"]))

        #variable: theoretical isotopic abundance
        Drawer_fill[["isoabb"]] <- unlist(unname(CompCol_xic[i, "isoabb"]))

        #variable: file name
        Drawer_fill[["FileName"]] <- raw_data@phenoData@data[["sample_name"]]

        #variable: introduced sample group
        Drawer_fill[["Grp"]] <- raw_data@phenoData@data[["sample_group"]]

        #vector: intensity
        int.v <- unname(ChromData[[i]]@intensity)
        Drawer_fill[["Intensities.v"]] <- paste(as.character(int.v), collapse=",")

        #vector: retention time
        rt.v <- unname(ChromData[[i]]@rtime)
        Drawer_fill[["RT.v"]] <- paste(as.character(rt.v), collapse=",")

        if(sum(int.v > 0)){

          #variable: base line
          Drawer_fill[["BaseLine"]] <- min(int.v)

          #peak picking
          if(xx==1 & PredictedRT == FALSE){ #for most abundand isotopologues (highest number of consecutive points above base line; exact RT not known)

            int <- unname(ChromData[[i]]@intensity)
            rt <- unname(ChromData[[i]]@rtime)

            lim <- cutout_peak(rt.v, int.v, Min.PpP, mean(min(rt.v), max(rt.v)), startTime = NA, endTime = NA)

          } else if(xx==1 & PredictedRT == TRUE){ #for most abundand isotopologues (highest number of consecutive points above base line; exact RT known)

            #get RT from most abundand isotopologue
            if(!is.na(match("roi_overlaps_max", colnames(CompCol_xic)))){

              if(CompCol_xic[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                             adduct == unname(unlist(Drawer_fill[["adduct"]])) &
                             isoabb == unname(unlist(Drawer_fill[["isoabb"]]))][["roi_overlaps_max_sc"]] > 1){

                preferedRT <- CompCol_xic[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                                            adduct == unname(unlist(Drawer_fill[["adduct"]])) &
                                            isoabb == unname(unlist(Drawer_fill[["isoabb"]]))][["roi_overlaps_max"]]
              } else{

                preferedRT <- CompCol_xic[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                                            adduct == unname(unlist(Drawer_fill[["adduct"]])) &
                                            isoabb == unname(unlist(Drawer_fill[["isoabb"]]))][["rt"]]
              }
            } else {

              preferedRT <- CompCol_xic[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                                          adduct == unname(unlist(Drawer_fill[["adduct"]])) &
                                          isoabb == unname(unlist(Drawer_fill[["isoabb"]]))][["rt"]]

            }

            lim <- cutout_peak(rt.v, int.v, Min.PpP, preferedRT, startTime = NA, endTime = NA)

            Drawer_fill[["BaseLine"]] <- lim[4]

            } else if(xx == 2){ #for less abundant isotopologues (highest number of consecutive points coeluting with most abundand isotopologue)

            #get RT from most abundand isotopologue
            preferedRT <- MA.Isos[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                                    adduct == unname(unlist(Drawer_fill[["adduct"]]))][["RT"]]

            startTime <- MA.Isos[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                                   adduct == unname(unlist(Drawer_fill[["adduct"]]))][["StartTime"]]

            endTime <- MA.Isos[molecule == unname(unlist(Drawer_fill[["molecule"]])) &
                                 adduct == unname(unlist(Drawer_fill[["adduct"]]))][["EndTime"]]

            lim <- cutout_peak(rt.v, int.v, Min.PpP, preferedRT, startTime = startTime, endTime = endTime)

            Drawer_fill[["BaseLine"]] <- lim[4]

            Drawer_fill[["SpikeIndicator"]] <- lim[5]
          }

          if(sum(lim) != 0){

            #variable: theoretical mz
            Drawer_fill[["MzTheoretical"]] <- (ChromData[[i]]@filterMz[2] + ChromData[[i]]@filterMz[1]) / 2

            #get intensity vector for picked peak
            cint.v_tmp <- BaselToNA(Vector = int.v[(lim[1]+1):(lim[2]-1)],
                                    baseL = unlist(Drawer_fill[["BaseLine"]]),
                                    na.rm = TRUE)

            #get retention time vector for picked peak
            cint.v <- c(int.v[lim[1]], cint.v_tmp, int.v[lim[2]])

            #get retention time vector for picked peak
            crt.v_tmp <- BaselToNA(Vector = int.v[(lim[1]+1):(lim[2]-1)],
                                   baseL = unlist(Drawer_fill[["BaseLine"]]),
                                   na.rm = FALSE)

            crt.v_tmp <- c(int.v[lim[1]], crt.v_tmp, int.v[lim[2]])

            crt.v <- MirrorNAs(crt.v_tmp, rt.v[lim[1]:lim[2]], na.rm = TRUE)

            #variable: degenerate (whether missing scans were removed)
            Drawer_fill[["Degenerate"]] <- as.numeric(lim[3])

            #variable: start time of peak
            Drawer_fill[["StartTime"]] <- GetFWXM(rt.v[lim[1]:lim[2]], int.v[lim[1]:lim[2]], min(int.v), 0.1, peak_borders = TRUE)[1] #min(crt.v)

            #variable: end time of peak
            Drawer_fill[["EndTime"]] <- GetFWXM(rt.v[lim[1]:lim[2]], int.v[lim[1]:lim[2]], min(int.v), 0.1, peak_borders = TRUE)[2] #max(crt.v)

            #variable: number of points over peak
            Drawer_fill[["PpP"]] <- sum(cint.v > unlist(Drawer_fill[["BaseLine"]]))

          } else Drawer_fill[["PpP"]] <- 0

          if(unlist(Drawer_fill[["PpP"]]) >= Min.PpP){ #following variables are extracted only if the number of peak points exceeds Min.PpP

            #variable: peak height over base line
            Drawer_fill[["Height"]] <- max(cint.v) - unlist(Drawer_fill[["BaseLine"]])

            #variables: peak width at different heights above base line
            Drawer_fill[["FWHM"]] <- GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.5)[2] - GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.5)[1]
            Drawer_fill[["FW25M"]] <- GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.25)[2] - GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.25)[1]
            Drawer_fill[["FW10M"]] <- GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.1)[2] - GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.1)[1]
            Drawer_fill[["FW75M"]] <- GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.75)[2] - GetFWXM(crt.v, cint.v, unlist(Drawer_fill[["BaseLine"]]), 0.75)[1]

            #variable: peak height above base line
            crt.v[1] <- unlist(Drawer_fill[["StartTime"]])
            crt.v[length(crt.v)] <- unlist(Drawer_fill[["EndTime"]])

            Drawer_fill[["Area"]] <- AUC(crt.v, ceilingToX(cint.v - unlist(Drawer_fill[["BaseLine"]]), 0, 0), method = "trapezoid")

            #variable: ZigZag_index
            Zaeler_zzi = 0
            for(n in c(2:(length(cint.v)-1))){
              Zaeler_zzi <- Zaeler_zzi + (2 * cint.v[n] - cint.v[n - 1] - cint.v[n + 1])^2
            }
            Drawer_fill[["Zigzag_idx"]] <- Zaeler_zzi / (length(cint.v) * unlist(Drawer_fill[["Height"]])^2)

            #variable: Sharpness
            befAp = 0
            aftAp = 0
            ApPos <- which.max(cint.v)
            for(n in c(2:(length(cint.v)-1))){
              if(n <= ApPos){
                befAp <- befAp + (cint.v[n] - cint.v[n - 1]) / cint.v[n - 1]
              }

              if(n >= ApPos){
                aftAp <- aftAp + (cint.v[n] - cint.v[n + 1]) / cint.v[n + 1]
              }
            }
            Drawer_fill[["Sharpness"]] <- aftAp + befAp

            #variable: retention time of peak (determined by position of maximum)
            Drawer_fill[["RT"]] <- as.numeric(crt.v[which.max(cint.v)])

            #variable: retention time of peak (determined by position of maximum from smoothed peak)
            #SGOLn <-  ceiling(unlist(Drawer_fill[["PpP"]]) * 0.4)
            #if(SGOLn > 3){
            #  if(!IsOdd(SGOLn)){SGOLn = SGOLn + 1 }
            #  Drawer_fill[["RTsmoothed"]] <- as.numeric(crt.v[which.max(sgolayfilt(cint.v, 3, SGOLn))])
            #  Drawer_fill[["RTsmoweight"]] <- weighted.mean(crt.v, sgolayfilt(cint.v, 3, SGOLn))
            #}

            #variable: retention time of peak (determined by position of weigthed mean of intensities)
            #Drawer_fill[["RTweight"]] <- weighted.mean(crt.v, cint.v)

            #variable: difference of heighest to lowest mz value over peak in mz
            #Drawer_fill[["DeltaMz"]] <- ChromData[[i]]@mz[2] - ChromData[[i]]@mz[1]

            #variable: difference of heighest to lowest mz value over peak in ppm
            #Drawer_fill[["DeltaPpm"]] <- 1e6 * as.numeric(unlist(Drawer_fill[["DeltaMz"]])) / as.numeric(unlist(Drawer_fill[["MzTheoretical"]]))

            #consecutive increasing/decreasing points
            int.v_peak <- as.numeric(int.v[lim[1]:lim[2]])
            #write.csv(int.v_peak, "int.csv")
            if(length(int.v_peak[1:which.max(int.v_peak)]) > 2 & length(int.v_peak[which.max(int.v_peak):length(int.v_peak)]) > 2){

              Drawer_fill[["trend.left"]] <- length(which(diff(int.v_peak[1:which.max(int.v_peak)]) > 0)) / length(diff(int.v_peak[1:which.max(int.v_peak)]))
              Drawer_fill[["trend.right"]] <- length(which(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]) < 0)) / length(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]))

              #Drawer_fill[["trend.left.f"]] <- length(which(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]) > 0))# / length(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]))
              #Drawer_fill[["trend.right.f"]] <- length(which(diff(int.v_peak[1:which.max(int.v_peak)]) < 0))# / length(diff(int.v_peak[1:which.max(int.v_peak)]))

              Drawer_fill[["trend.left.f"]] <-  max(Rle(diff(int.v_peak[1:which.max(int.v_peak)]) < 0)@lengths[which(Rle(diff(int.v_peak[1:which.max(int.v_peak)]) < 0)@values)])# / length(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]))
              Drawer_fill[["trend.right.f"]] <- max(Rle(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]) > 0)@lengths[which(Rle(diff(int.v_peak[which.max(int.v_peak):length(int.v_peak)]) > 0)@values)])# / length(diff(int.v_peak[1:which.max(int.v_peak)]))

              }


            #check if height only because of spikes and if base present
            left.halve <- as.numeric(int.v_peak[1:which.max(int.v_peak)])
            right.halve <- as.numeric(int.v_peak[which.max(int.v_peak):length(int.v_peak)])



            #check for missing base

            #Drawer_fill[["MissingBase"]] <-  left.halve[2] > unname(unlist(Drawer_fill[["BaseLine"]])) + 0.3 * unname(unlist(Drawer_fill[["Height"]])) |
            #  rev(right.halve)[2] > unname(unlist(Drawer_fill[["BaseLine"]])) + 0.3 * unname(unlist(Drawer_fill[["Height"]]))
            Drawer_fill[["MissingBase"]] <-  max((left.halve[2] - unname(unlist(Drawer_fill[["BaseLine"]]))) / unname(unlist(Drawer_fill[["Height"]])),
              (rev(right.halve)[2] - unname(unlist(Drawer_fill[["BaseLine"]]))) / unname(unlist(Drawer_fill[["Height"]])))



            Spike_peak.rle <- Rle(int.v_peak > (unname(unlist(Drawer_fill[["Height"]])) - 0.45 * unname(unlist(Drawer_fill[["Height"]]))))

            left.spike = FALSE
            right.spike = FALSE

            if(length(diff(left.halve)[ diff(left.halve) < 0]) > 0){

              left.spike <- abs(min(diff(left.halve))) > 0.3 * unname(unlist(Drawer_fill[["Height"]]))

            }

            if(length(diff(right.halve)[ diff(right.halve) > 0]) > 0){

              right.spike <- abs(max(diff(right.halve))) > 0.3 * unname(unlist(Drawer_fill[["Height"]]))

            }

            Drawer_fill[["Spike_peak"]] <- max(Spike_peak.rle@lengths[which(Spike_peak.rle@values)]) <= 2 | left.spike == TRUE | right.spike == TRUE

            Drawer_fill[["Second_Iso"]] <- lim[6]

            Drawer_fill[["Double_peak"]] <- lim[7]

            Drawer_fill[["Break_point"]] <- lim[8]

          }}else{ #peaks falling below Min.PpP are set to zero for some variables (those peaks will not show up in the output table anyway)
            Drawer_fill[["PpP"]] <- 0
            Drawer_fill[["FWHM"]] <- 0
          }

        #Feature (TRUE/FALSE)    KANN EIGENTLICH GELÖSCHT WERDEN
        if(as.numeric(unlist(Drawer_fill[["PpP"]])) > Min.PpP & !is.na(unlist(Drawer_fill[["FWHM"]]))){Drawer_fill[["Feature"]] <- TRUE} else{Drawer_fill[["Feature"]] <- FALSE}

        #transform list with variables on one peak into data frame

        Drawer_closed <- data.frame(Drawer_fill, stringsAsFactors = FALSE)

        #put data frame into list for storage of all peaks
        Bureau[[i]] <- Drawer_closed
      }

      #transform storage lists into data tables (most abundant isotopologues and other isotopologues are handel seperately)
      if(xx == 1){ #generation of datatables for most abundant isotopologues



        MA.Isos <- data.table::rbindlist(Bureau, fill = TRUE)

        }else{ #generation of data tables for less abundant isotopologues

        Rest <- data.table::rbindlist(Bureau, fill = TRUE)

        #combining data tables from most abundant and less abundant isotopologues
        Output.perfile <- as.data.table(rbind(MA.Isos, Rest))

#return(Bureau)
        return(Output.perfile)

      }
    }
  }

  stopCluster(cl)
#########
#return(Output)
#########
  Output2 <- data.table::rbindlist(Output, fill = TRUE)

  Result <- Output2[PpP > Min.PpP][CompCol_all, on=.(molecule, adduct, isoabb, FileName)]


  Result$IDX <- seq.int(nrow(Result))

  return(Result)
}


