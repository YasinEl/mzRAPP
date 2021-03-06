#' SkylineTransitionList
#'
#' @description Takes the output of \code{\link{find_bench_peaks}} and generates a Skyline Transition list (automatically exported to working directory) which can then be imported to Skyline via
#' Skyline -> File -> Import -> Transition List
#'
#' @param BM output of \code{\link{find_bench_peaks}}
#' @param export_to_csv export output automatically to working directory
#'
#'
#' @return Skyline Transition List
#' @export
#'
#'
SkylineTransitionList <-
  function(BM, export_to_csv = TRUE){

    MassPrec <- round(max(BM$peaks.mz_span_ppm) / 2, 1)

    BM <- BM[, c("molecule", "adduct", "isoab", "peaks.mz_accurate", "charge")]
    BM$molecule <- as.character(BM$molecule)
    BM[, "Precursor Name" := paste0(molecule, "_", adduct, "_", round(isoab, 2))]

    BM <-  unique(BM[, c("Precursor Name", "charge")])[BM[, .(`Molecule List Name` = molecule,
                                                      `Precursor m/z` = mean(peaks.mz_accurate),
                                                      `Product m/z` = mean(peaks.mz_accurate)),
                                                  by = .(`Precursor Name`)], on = .(`Precursor Name`)]

    BM <- BM[, c("Precursor charge", "Product charge") := .(charge, charge)][, !"charge"]

    if(export_to_csv == TRUE){
      data.table::fwrite(unique(BM), file = "Skyline_Transition_List.csv", row.names = FALSE)
      message(paste0("Transition List has been saved to your working directory as ", getwd(), "/Skyline_Transition_List.csv"))
    }

    message(paste0("Please go to 'Skyline -> Settings -> Transition Settings -> Full-Scan -> Mass Accuracy' and set 'Precursor mass analyzer' to 'Centroided' and ",
                 "Mass Accuracy to about ", MassPrec, " ppm. You can then load this Transition list into Skyline via 'Skyline -> File -> Import -> Transition List...'."))

    return(unique(BM))

  }


#' SkylinePeakBoundaries
#' @description Takes the output of \code{\link{find_bench_peaks}} and generates a Skyline peak-boundaries file (automatically exported to working directory) which can then be imported to Skyline via
#' Skyline -> File -> Import -> Peak Boundaries... (after the required mzML files have been imported into Skyline using Skyline -> Import -> Results...)
#'
#' @param BM output of \code{\link{find_bench_peaks}}
#' @param export_to_csv export output automatically to working directory
#'
#' @return Skyline peak boundaries
#' @export
#'
#'
SkylinePeakBoundaries <-
  function(BM, export_to_csv = TRUE){

    BM <- BM[, c("molecule", "adduct", "isoab", "FileName", "peaks.StartTime", "peaks.EndTime")]
    BM[, peaks.StartTime := peaks.StartTime/60]
    BM[, peaks.EndTime := peaks.EndTime/60]
    BM[, molecule := as.character(molecule)]

    BM[order(BM$isoab, decreasing = TRUE),]

    BM[, "Peptide Modified Sequence" := paste0(molecule, "_", adduct, "_", round(isoab, 2))]

    files <- data.table::data.table("FileName" = sort(unique(BM$FileName)),
                        "i" = seq(length(unique(BM$FileName))))

    EICs <- data.table::data.table("Peptide Modified Sequence" = sort(unique(BM$`Peptide Modified Sequence`)),
                        "i" = rep(1, length(unique(BM$`Peptide Modified Sequence`))))

    Peak_Boundaries_Skyline <- EICs[files, on=.(i<=i), allow.cartesian = TRUE][, !"i"]

    Peak_Boundaries_Skyline <- BM[, c("FileName" ,"Peptide Modified Sequence", "peaks.StartTime", "peaks.EndTime")][
      Peak_Boundaries_Skyline,
      on = .(`Peptide Modified Sequence`, FileName),
      nomatch = NA
      ]


      Peak_Boundaries_Skyline[is.na(peaks.StartTime), peaks.StartTime := 0]
      Peak_Boundaries_Skyline[is.na(peaks.EndTime), peaks.EndTime := 0]

      colnames(Peak_Boundaries_Skyline) <- c("File Name", "Peptide Modified Sequence", "Min Start Time", "Max End Time")


      if(export_to_csv == TRUE){
        data.table::fwrite(Peak_Boundaries_Skyline, file = "Skyline_Peak_Boundaries.csv", row.names = FALSE)
        message(paste0("Peak Boundaries have been saved to your working directory as ", getwd(), "/Skyline_Peak_Boundaries.csv"))
      }

      message("After Transition List and mzML files have been loaded into Skyline you can apply these Peak Boundaries via 'Skyline -> File -> Import -> Peak Boundaries...'.")

    return(Peak_Boundaries_Skyline)
  }

