#' get_ROIs
#'
#' @description Takes a the output of \code{\link{get_mz_table}} and adds columns with information on regions of interest (ROI).
#'
#' @param files vector containing all mzML file paths
#' @param Target.table output of function \code{\link{get_mz_table}}
#' @param minCentroids minimum number of consecutive scans > 0 for a ROI to be picked up
#' @param AccurateMZtol mass accuracy (systematic error tolerance) in +/- ppm; this value is used to recognize detected ROIs as the expected mz values calculated in \code{\link{get_mz_table}}
#' @param PrecisionMZtol mass precision (random error tolerance) in +/- ppm; this value is used as for setting the maximum spread of scans within one ROI (equ. to "dev" argument in  xcms:::findmzROI)
#' @param plan see \code{\link{plan}}
#'
#'
#' @return data.table object with information on ROIs for each row in Target.table. additional columns from Target.table are retained
#' @export
#'
#'

get_ROIs <-
  function(files,
           Target.table,
           minCentroids = 4,
           AccurateMZtol = 5,
           PrecisionMZtol = 5,
           plan = "multiprocess"){


    if(!isTRUE(is.data.frame(Target.table))){stop(paste0("Target.table has to be a data frame and/or data table!"))}
    if(!isTRUE(is.data.table(Target.table))){Target.table <- as.data.table(Target.table)}

    missing_cols <- setdiff(c("StartTime.EIC", "EndTime.EIC"), colnames(Target.table))
    if(length(missing_cols) > 0 && all(c("user.rtmin", "user.rtmax") %in% colnames(Target.table))){
      maxrt <- max(Target.table$user.rtmax, na.rm = TRUE)
      Target.table <- Target.table[, .(molecule = molecule,
                                       adduct = adduct,
                                       isoab = isoab,
                                       StartTime.EIC = max(c(1, user.rtmin - (user.rtmax - user.rtmin) * 3)),
                                       EndTime.EIC = min(c(user.rtmax + (user.rtmax - user.rtmin) * 3, maxrt+1))),
                                   by = rownames(Target.table)][Target.table, on = .(molecule, adduct, isoab)]

      Target.table <- Target.table[, !"rownames"]

    }

    missing_cols <- setdiff(c("molecule", "adduct", "isoab", "mz_ex", "StartTime.EIC", "EndTime.EIC"), colnames(Target.table))
    if(length(missing_cols) > 0){stop(paste0("Target.table is lacking columns: ", paste(missing_cols, sep = ", ")))}

    if(any(duplicated(Target.table, cols = c("molecule", "adduct")))) stop(paste0("Your Target.table includes duplicates (some molecule - adduct combinations exceist more than once)!
                                                                                Please, make sure that names given in the column 'molecule' are unique or have different adducts
                                                                                in the column 'adduct'!" ))

    conflicting_cols <- intersect(c("eic_mzmin", "eic_mzmax", "mz_acc", "roi_rtmin", "roi_rtmax", "roi_scmin", "roi_scmax",
                                    "ROI_count", "roi_overlaps_max", "roi_overlaps_max_sc"), colnames(Target.table))
    if(length(conflicting_cols > 0)) stop(paste0("Target.table includes reserved column names! Specificly:", conflicting_cols))

    if(length(files[!file.exists(files)] > 0)) stop(paste0("It seems like some of your mzML files do not exist, cannot be accessed or contain spelling errors! Specificly:", files[!file.exists(files)]))



#    if(dplyr::intersect(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files), ))

    #if(length(files) < 2){stop("Please provide more than 1 mzML file.")}

    #if("user.rt" %in% colnames(Target.table)){

    # filter_table <- Target.table

    #  filter_table$mz <- round(filter_table$mz, 2)

    #   filter_table$user.rt <- round(filter_table$user.rt, 0)

    #  filter_table <- setorder(filter_table, "isoab")

    #  filter_table$dpl_mz <- duplicated(filter_table, by = c("user.rt", "mz"))

    #   checkk <<- filter_table

    #  if(nrow(filter_table[dpl_mz == TRUE & isoab == 100]) > 0) {

    #  warning(paste0("It seems like some of your target molecules are actually isotopologues of some of your other target molecules!
    #                 In order to resolve this issue some target molecules(", length(unique(filter_table[dpl_mz == TRUE & isoab == 100]$molecule)),
    #                 ")have been removed. Specificly: ",
    #                 paste(unique(filter_table[dpl_mz == TRUE & isoab == 100]$molecule), collapse = ", ")))

    #  Target.table <- Target.table[!(molecule %in% unique(filter_table[dpl_mz == TRUE & isoab == 100]$molecule))]

    #  }
    #
    # }



    if(!is.character(Target.table$molecule)) {Target.table$molecule <- as.character(Target.table$molecule)}

    ##################################
    #replicate rows for each file if that did not already happen before
    ##################################
    if(is.na(match("FileName", colnames(Target.table)))){
      files.dt <- data.table(FileName = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)))
      files.dt[,fileIdx:= seq(nrow(files.dt))]
      Target.table$fileIdx <- rep(1, nrow(Target.table))
      Target.table <- Target.table[files.dt, on=.(fileIdx<=fileIdx), allow.cartesian = TRUE]
      Target.table <- Target.table[, !"fileIdx"]
    } else {
      Target.table[, FileName := tools::file_path_sans_ext(basename(FileName))]
      Target.table <- Target.table[FileName %in% tools::file_path_sans_ext(basename(files))]
      if(nrow(Target.table) == 0){stop("It appears there are no user.rtmin and user.rtmax provided for the mzML files provided!")}

    }

    ##################################
    #initiate parallel processing
    ##################################
    `%dopar%` <- foreach::`%dopar%`
    doFuture::registerDoFuture()
    future::plan(plan)
    Output <- foreach::foreach(file = unique(Target.table$FileName), .packages = c("mzRAPP")) %dopar% {
      #for(file in seq(length(files))){

      ##################################
      #read mzML files into xcmsRaw objects and determine clostest scans to attempted start and end points of XICs
      ##################################
      .xr <- suppressWarnings(xcms::xcmsRaw(files[which(tools::file_path_sans_ext(basename(files)) == tools::file_path_sans_ext(basename(file)))], profstep=0))
      Target.table.wk <- Target.table[FileName == file]
      .Target.table.wk <- Target.table.wk[Target.table.wk[isoab == 100, .(StartXICScan = which.min(abs(.xr@scantime - min(StartTime.EIC))),
                                                                           EndXICScan = which.min(abs(.xr@scantime - max(EndTime.EIC)))),
                                                          by=.(molecule)],
                                          on = .(molecule)]


      matches_perfile_list <-
        lapply(unique(.Target.table.wk$molecule), function(molec, xr = .xr, Target.table.wk = .Target.table.wk){


          ##################################
          #find ROIs and set up ROI-table and Target.table for their join
          ##################################
          trash <- capture.output({
            suppressWarnings(
              ROI.list <- xcms:::findmzROI(xr,
                                           dev = PrecisionMZtol * 1E-6,
                                           minCentroids = minCentroids,
                                           scanrange = c(Target.table.wk[molecule == molec]$StartXICScan[1], Target.table.wk[molecule == molec]$EndXICScan[1]),
                                           #mzrange = c(Target.table.wk[molecule == molec]$mz_ex[1] - 10, Target.table.wk[molecule == molec]$mz_ex[1] + 10),
                                           prefilter = c(minCentroids,0),
                                           noise = 0)
            )})

          ROI.dt <- rbindlist(ROI.list)
          #if(molec == "Glutathione, reduced"){
          #roit <<- ROI.dt
          #}
          ROI.dt[, roi_id := 1:nrow(ROI.dt)]
          ROI.dt <- ROI.dt[, `:=` (rtmin = xr@scantime[scmin],
                                   rtmax = xr@scantime[scmax],
                                   mzlowerBD = mz,
                                   mzupperBD = mz)]

          Target.table.wk.molec <- Target.table.wk[molecule == molec]
          Target.table.wk.molec[, `:=` (mzlowerBD = mz_ex - mz_ex * AccurateMZtol * 1e-6,
                                        mzupperBD = mz_ex + mz_ex * AccurateMZtol * 1e-6)]

          ##################################
          #join ROI-table with Target.table; a ROI is joined to an expected EIC in the target table if ROIs calculated mz falls within the calculated range of mzlowerBD - mzupperBD
          ##################################
          setkey(Target.table.wk.molec, mzlowerBD, mzupperBD)
          setkey(ROI.dt, mzlowerBD, mzupperBD)
          mz.overlap <- foverlaps(ROI.dt,
                                  Target.table.wk.molec,
                                  nomatch = NULL)

          if(nrow(mz.overlap) != 0){

            #mol_tab <<- mz.overlap
            #if(any(duplicated(mz.overlap, by = c("adduct", "roi_id")))){stop(paste("Some ROIs are matching to more than one of your mass traces. ",
            #                                                                       "Please lower your mz accuracy/precision and/or check if you set the correct instrument resolution when generating your Target.table."))}


            ##################################
            #combine isolated ROIs if they were falling within the same the boundaries of the same expected EIC
            ##################################
            matches_summary <- mz.overlap[,.(eic_mzmin = as.double(min(mzmin)),
                                             eic_mzmax = as.double(max(mzmax))),#,
                                             #mz_acc = as.double(mean(mz)),
                                             #roi_rtmin = as.numeric(min(rtmin)),
                                             #roi_rtmax = as.numeric(max(rtmax)),
                                             #roi_scmin = min(scmin),
                                             #roi_scmax = max(scmax),
                                             #roi_int = sum(abs(intensity)),
                                             #roi_count = .N),
                                          by=.(molecule, adduct, isoab, FileName)]

            ##################################
            #find the rt region where most isotopologues overlap with the most abundant isotopologue to estimate(!) the rt of the compound
            ##################################
            #matches_summary <- matches_summary[matches_summary[,.(roi_overlaps_max = xr@scantime[which.min(abs(round(mzRAPP:::getOverlapWithLine1(roi_scmin, roi_scmax)) - xr@acquisitionNum))],
            #                                                      roi_overlaps_max_sc = which.min(abs(round(mzRAPP:::getOverlapWithLine1(roi_scmin, roi_scmax)) - xr@acquisitionNum))),
            #                                                   by=.(molecule, adduct, FileName)],
            #                                   on=.(molecule, adduct, FileName)]
            return(matches_summary)

          } else return(NULL)
        })
      return(rbindlist(matches_perfile_list))
    }

    future::plan("sequential")
    ##################################
    #collect and return cluster output
    ##################################
    matches <- rbindlist(Output)


    return(matches[Target.table, on=.(molecule, adduct, isoab, FileName), nomatch = NA])
  }
