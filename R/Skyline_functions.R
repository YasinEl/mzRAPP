#' SkylineTransitionList
#'
#' @param BM
#'
#' @return
#' @export
#'
#' @examples
#'
SkylineTransitionList <-
  function(BM){

    MassPrec <- round(1e6 * max((BM$eic_mzmax - BM$eic_mzmin) / BM$mz_acc), 1)

    BM <- BM[, c("molecule", "adduct", "isoabb", "mz_acc", "charge")]
    BM[, "Precursor Name" := paste0(molecule, "_", adduct, "_", round(isoabb, 2))]

    BM <-  unique(BM[, c("Precursor Name", "charge")])[BM[, .(`Molecule List Name` = molecule,
                                                      `Precursor m/z` = mean(mz_acc),
                                                      `Product m/z` = mean(mz_acc)),
                                                  by = .(`Precursor Name`)], on = .(`Precursor Name`)]

    BM <- BM[, c("Precursor charge", "Product charge") := .(charge, charge)][, !"charge"]


    fwrite(BM, file = "Skyline_Transition_List.csv")

    print(paste0("Transition List has been saved as ", getwd(), "/Skyline_Transition_List.csv"))


    print(paste0("Please go to 'Skyline -> Settings -> Transition Settings -> Full-Scan -> Mass Accuracy' and set 'Precursor mass analyzer' to 'Centroided' and
                 Mass Accuracy to ", MassPrec, " ppm. You can then load this Transition list into Skyline via
                 'Skyline -> File -> Import -> Transition List...'."))
    return(unique(BM))

  }


#' SkylinePeakBoundaries
#'
#' @param BM
#'
#' @return
#' @export
#'
#' @examples
#'
SkylinePeakBoundaries <-
  function(BM){

    BM <- BM[, c("molecule", "adduct", "isoabb", "FileName", "peaks.StartTime", "peaks.EndTime")]
    BM$peaks.StartTime <- BM$peaks.StartTime / 60
    BM$peaks.EndTime <- BM$peaks.EndTime / 60

    BM[order(BM$isoabb, decreasing = TRUE),]

    BM[, "Peptide Modified Sequence" := paste0(molecule, "_", adduct, "_", round(isoabb, 2))]


    files <- data.table("FileName" = sort(unique(BM$FileName)),
                        "i" = seq(length(unique(BM$FileName))))

    EICs <- data.table("Peptide Modified Sequence" = sort(unique(BM$`Peptide Modified Sequence`)),
                        "i" = rep(1, length(unique(BM$`Peptide Modified Sequence`))))

    Peak_Boundaries_Skyline <- EICs[files, on=.(i<=i), allow.cartesian = TRUE][, !"i"]

    Peak_Boundaries_Skyline <- BM[, c("FileName" ,"Peptide Modified Sequence", "peaks.StartTime", "peaks.EndTime")][
      Peak_Boundaries_Skyline,
      on = .(`Peptide Modified Sequence`, FileName),
      nomatch = NA
      ]

    Peak_Boundaries_Skyline[is.na(peaks.StartTime)] <- 0
    Peak_Boundaries_Skyline[is.na(peaks.EndTime)] <- 0

    colnames(Peak_Boundaries_Skyline) <- c("File Name", "Peptide Modified Sequence", "Min Start Time", "Max End Time")

    fwrite(BM, file = "Skyline_Peak_Boundaries.csv",)

    print(paste0("Peak Boundaries have been saved as ", getwd(), "/Skyline_Peak_Boundaries.csv"))

    print("After Transition List and mzML files have been loaded into Skyline you can apply these Peak Boundaries via
          'Skyline -> File -> Import -> Peak Boundaries...'.")

    return(Peak_Boundaries_Skyline)
  }

