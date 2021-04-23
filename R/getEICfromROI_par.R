#' get_ROIs
#'
#' @description Takes a the output of \code{\link{get_mz_table}} and adds columns with information on regions of interest (ROI).
#'
#' @param files vector containing all mzML file paths
#' @param Target.table output of function \code{\link{get_mz_table}}
#' @param minCentroids minimum number of consecutive scans > 0 for a ROI to be picked up (eq. to minCentroids argument in xcms:::findmzROI function)
#' @param AccurateMZtol mass accuracy (systematic error tolerance) in +/- ppm; this value is used to recognize detected ROIs as the expected mz values calculated in \code{\link{get_mz_table}}. If multiple ROIs fit the same benchmark peak they are combined.
#' @param PrecisionMZtol mass precision (random error tolerance) in +/- ppm; this value is used as for setting the maximum spread of scans within one ROI (equ. to "dev" argument * 1e-6 in xcms:::findmzROI)
#' @param plan see \code{\link{plan}}
#'
#' @details \strong{eic_mzmin:} lowest mz value detected in respective ROI
#' @details \strong{eic_mzmax:} highest mz value detected in respective ROI
#'
#' @importFrom data.table as.data.table setkey is.data.table
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
    if(any(duplicated(tools::file_path_sans_ext(basename(files))))){stop("Some of your selected files appear more than once!")}
    missing_cols <- setdiff(c("StartTime.EIC", "EndTime.EIC"), colnames(Target.table))
    if(length(missing_cols) > 0 && all(c("user.rtmin", "user.rtmax") %in% colnames(Target.table))){
      maxrt <- max(Target.table$user.rtmax, na.rm = TRUE)
      Target.table[, rownames := rownames(Target.table)]
      Target.table <- Target.table[, .(StartTime.EIC = max(c(1, user.rtmin - (user.rtmax - user.rtmin) * 3)),
                                       EndTime.EIC = min(c(user.rtmax + (user.rtmax - user.rtmin) * 3, maxrt+1))),
                                   by = rownames(Target.table)][Target.table, on = .(rownames)]

      Target.table <- Target.table[, !"rownames"]

    }

    missing_cols <- setdiff(c("molecule", "adduct", "isoab", "mz_ex", "StartTime.EIC", "EndTime.EIC", "user.rtmin", "user.rtmax"), colnames(Target.table))
    if(length(missing_cols) > 0){stop(paste0("Target.table is lacking columns: ", paste(missing_cols, sep = ", ")))}

    conflicting_cols <- intersect(c("eic_mzmin", "eic_mzmax", "mz_acc", "roi_rtmin", "roi_rtmax", "roi_scmin", "roi_scmax",
                                    "ROI_count", "roi_overlaps_max", "roi_overlaps_max_sc"), colnames(Target.table))
    if(length(conflicting_cols > 0)) stop(paste0("Target.table includes reserved column names! Specificly:", conflicting_cols))

    if(length(files[!file.exists(files)] > 0)) stop(paste0("It seems like some of your mzML files do not exist, cannot be accessed or contain spelling errors! Specificly:", files[!file.exists(files)]))

    if(!is.character(Target.table$molecule)) {Target.table$molecule <- as.character(Target.table$molecule)}

    ##################################
    #replicate rows for each file if that did not already happen before
    ##################################
    if(is.na(match("FileName", colnames(Target.table)))){
      files.dt <- data.table::data.table(FileName = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(files)))
      files.dt[,fileIdx:= seq(nrow(files.dt))]
      Target.table$fileIdx <- rep(1, nrow(Target.table))
      Target.table <- Target.table[files.dt, on=.(fileIdx<=fileIdx), allow.cartesian = TRUE]
      Target.table <- Target.table[, !"fileIdx"]
    } else {
      Target.table[, FileName := tools::file_path_sans_ext(basename(FileName))]
      Target.table <- Target.table[FileName %in% tools::file_path_sans_ext(basename(files))]
      if(nrow(Target.table) == 0){stop("It appears there are no user.rtmin and user.rtmax provided for the mzML files provided!")}

    }


    if(any(duplicated(Target.table, by = c("molecule", "adduct", "isoab", "FileName")))) stop(paste0("Your Target.table includes duplicates (some molecule - adduct combinations exist more than once per FileName)!
                                                                                Please, make sure that names given in the column 'molecule' are unique or have different adducts
                                                                                in the column 'adduct'!" ))


    ##################################
    #initiate parallel processing
    ##################################
    `%dopar%` <- foreach::`%dopar%`
    doFuture::registerDoFuture()


    if(plan == "multiprocess" && future::supportsMulticore()){
      future::plan(future::multicore)
    } else if(plan == "multiprocess") {
      future::plan(future::multisession)
    } else {
      future::plan(plan)
    }




    #future::plan(plan)
    Output <- foreach::foreach(file = unique(Target.table$FileName), .packages = c("mzRAPP")) %dopar% {
  #    for(file in files){

      ##################################
      #read mzML files into xcmsRaw objects and determine clostest scans to attempted start and end points of XICs
      ##################################
      .xr <- suppressWarnings(xcms::xcmsRaw(files[which(tools::file_path_sans_ext(basename(files)) == tools::file_path_sans_ext(basename(file)))], profstep=0))
      Target.table.wk <- Target.table[FileName == file]
      .Target.table.wk <- Target.table.wk[Target.table.wk[isoab == 100, .(StartXICScan = which.min(abs(.xr@scantime - min(user.rtmin))),
                                                                           EndXICScan = which.min(abs(.xr@scantime - max(user.rtmax)))),
                                                          by=.(molecule)],
                                          on = .(molecule)]


      matches_perfile_list <-
        lapply(unique(.Target.table.wk$molecule), function(molec, xr = .xr, Target.table.wk = .Target.table.wk){


          ##################################
          #find ROIs and set up ROI-table and Target.table for their join
          ##################################
          trash <- utils::capture.output({
            suppressWarnings(
              ROI.list <- xcms:::findmzROI(xr,
                                           dev = PrecisionMZtol * 1E-6,
                                           minCentroids = minCentroids,
                                           scanrange = c(Target.table.wk[molecule == molec]$StartXICScan[1], Target.table.wk[molecule == molec]$EndXICScan[1]),
                                           prefilter = c(minCentroids,0),
                                           noise = 0)
            )})
          ROI.dt <- data.table::rbindlist(ROI.list)
          if(nrow(ROI.dt) == 0) return(NULL)
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
          mz.overlap <- data.table::foverlaps(ROI.dt,
                                  Target.table.wk.molec,
                                  nomatch = NULL)

          if(nrow(mz.overlap) != 0){

            ##################################
            #combine isolated ROIs if they were falling within the same the boundaries of the same expected EIC
            ##################################
            matches_summary <- mz.overlap[,.(eic_mzmin = as.double(min(mzmin)),
                                             eic_mzmax = as.double(max(mzmax))),
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
      return(data.table::rbindlist(matches_perfile_list))
    }

    future::plan("sequential")
    ##################################
    #collect and return cluster output
    ##################################
    matches <- data.table::rbindlist(Output)


    return(matches[Target.table, on=.(molecule, adduct, isoab, FileName), nomatch = NA])
  }
