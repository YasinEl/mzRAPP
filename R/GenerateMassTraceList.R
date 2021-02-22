#' @title get_mz_table
#'
#' @description This is basically a wrapper-function around the enviPat package. It takes a table with columns "molecule", "SumForm_c" and
#' "adduct_c" and calculates theoretical m/z values and abundances for isotopologues down to a user-defined abundance threshold.
#'
#'
#' @param DT data.table with columns "molecule", "SumForm_c" and "adduct_c" (see details)
#' @param instrumentRes data frame with mz vs resolution dependence (see \code{\link{resolution_list}}) e.g. "resolution_list$`OTFusion,QExactiveHF_120000@200`"
#' @param RelInt_threshold relative abundance of the lowest isotopologue to be considered as percentage
#' @param stick_method method that should be used to calculate discrete m/z values from calculated profile pattern e.g. "intensoid" (see \code{\link{vdetect}})
#' @param adducts data frame containing adducts (see \code{\link{adducts}})
#' @param isotopes data frame containing isotopes (see \code{\link{isotopes}})
#' @param screening_adducts vector of adduct names to be added for all molecules (if not already in the provided DT)
#'
#' @details Make sure that molecular formulas in column "SumForm_c" only contain valid molecular formulas as described in \code{\link{check_chemform}}. Otherwise, the function might never complete. Additional columns in DT will be retained in the output of the function. However, the column names "adduct", "isoab", "formula", "charge" and "mz" are reserved.
#'
#' @import enviPat
#'
#' @importFrom data.table data.table as.data.table setDT melt.data.table
#'
#' @return data.table with columns "molecule", "formula", "adduct", "charge", "m/z" and "abundance"
#' @export
#'

get_mz_table <- function(DT, instrumentRes, RelInt_threshold = 0.05, stick_method = "intensoid", adducts, isotopes, screening_adducts = NULL)
{

  if(!isTRUE(is.data.frame(DT))){stop(paste0("DT has to be a data frame!"))}
  if(!isTRUE(is.data.table(DT))){DT <- as.data.table(DT)}

  missing_cols <- setdiff(c("molecule", "SumForm_c"), colnames(DT))
  if(length(missing_cols) > 0){stop(paste0("DT is lacking columns: ", paste0(missing_cols, sep = ", ")))}

  if(!("adduct_c" %in% colnames(DT))){
    DT[, adduct_c := main_adduct]
  }

  conflicting_cols <- intersect(c("adduct", "isoab", "formula", "charge", "mz" ,"mz_ex"), colnames(DT))
  if(length(conflicting_cols > 0)) stop(paste0("DT includes reserved column names! Specificly:", conflicting_cols))

  FileInfo = FALSE
  if(!is.na(match("FileName", colnames(DT)))){

    FileInfo = TRUE

    if(length(screening_adducts) > 0){
      screen_ad <- data.table(adduct_c = screening_adducts)
      screen_ad[, idx := seq(nrow(screen_ad))]

      DT_tmp <- data.table::copy(DT[adduct_c == main_adduct, !"adduct_c"])
      DT_tmp[, idx := seq(nrow(DT_tmp))]
      DT_tmp <- screen_ad[DT_tmp, on = .(idx <= idx), allow.cartesian = TRUE]
      DT_tmp <- DT_tmp[, !"idx"]

      DT <- unique(data.table::rbindlist(list(DT, DT_tmp), use.names = TRUE))
    }

    DT_fileInfo <- data.table::copy(DT[, !"SumForm_c"])
    colnames(DT_fileInfo)[grepl("adduct_c", colnames(DT_fileInfo))] <- "adduct"
    DT <- unique(DT[, c("molecule", "SumForm_c", "adduct_c")])

  }

  if(any(duplicated(DT, by = c("molecule", "adduct_c")))) stop(paste0("Your Target.table includes duplicates (some molecule - adduct combinations exist more than once per FileName)!
                                                                                Please, make sure that names given in the column 'molecule' are unique or have different adducts
                                                                                in the column 'adduct'!" ))



  if(!isTRUE(is.data.frame(instrumentRes))){stop(paste0("instrumentRes has to be a data frame!"))}
  if(!isTRUE(is.data.table(instrumentRes))){instrumentRes <- as.data.table(instrumentRes)}

  DT$molecule <- as.character(DT$molecule)
  DT$SumForm_c <- as.character(DT$SumForm_c)



  if(length(screening_adducts) > 0 & FileInfo == FALSE){
    screen_ad <- data.table(adduct_c = screening_adducts)
    screen_ad[, idx := seq(nrow(screen_ad))]

    DT_tmp <- data.table::copy(DT[adduct_c == main_adduct, !"adduct_c"])
    DT_tmp[, idx := seq(nrow(DT_tmp))]
    DT_tmp <- screen_ad[DT_tmp, on = .(idx <= idx), allow.cartesian = TRUE]
    DT_tmp <- DT_tmp[, !"idx"]

    DT <- unique(data.table::rbindlist(list(DT, DT_tmp), use.names = TRUE))
  }



  ##################################
  #load necessary data
  ##################################
  if(missing(adducts)){utils::data("adducts", envir = environment(), package = "enviPat")}
  if(missing(isotopes)){utils::data("isotopes", envir = environment(), package = "enviPat")}


  wrong_adducts <- setdiff(unique(DT$adduct_c), adducts$Name)
  if(length(wrong_adducts) > 0){stop(paste0("Some of your adducts are not valid: ", wrong_adducts, " Please only use adducts enabled in the envipat package. namely: ", adducts$Name))}

  #data("resolution_list", envir = environment())
  #df <- resolution_list$`OTFusion,QExactiveHF_120000@200`

  ##################################
  #check sum formulas and create working-table
  ##################################
  DTreg <- DT
  SF <- enviPat::check_chemform(isotopes,DT$SumForm_c)

  if(nrow(setDT(SF)[warning == TRUE]) > 0){stop(paste0("Some chemical formulas are not valid, namely ", paste0(setDT(SF)[warning == TRUE]$new_formula, sep = ", ")))}


  DT <- cbind(DT,SF)
  DT <- merge(DT, adducts, by.x="adduct_c", by.y="Name")

  ##################################
  #calculate new sum formulas from adducts
  ##################################
  DT$SumForm2_c <- mapply(enviPat::multiform,DT$new_formula,DT$Mult)
  DT$SumForm2_c <- mapply(function(formula1, formula2){if(formula2 != "FALSE") enviPat::mergeform(formula1, formula2) else formula1}, DT$SumForm2_c, DT$Formula_add)
  DT$SumForm2_c <- mapply(function(formula1, formula2){if(formula2 != "FALSE") enviPat::subform(formula1, formula2) else formula1}, DT$SumForm2_c, DT$Formula_ded)
  DT <- DT[!grepl(" ", SumForm2_c)]

  ##################################
  #check new sum formulas
  ##################################
  SF <- enviPat::check_chemform(isotopes,DT$SumForm2_c)


  if(nrow(setDT(SF)[warning == TRUE]) > 0){stop(paste0("Some chemical formulas are not valid, specificly ", paste0(setDT(SF)[warning == TRUE]$new_formula, sep = ", ")))}

  ##################################
  #calculate theoretical isotope pattern
  ##################################
  pattern <- enviPat::isopattern(isotopes,
                                 chemforms = SF$new_formula,
                                 plotit = FALSE,
                                 threshold = 0.01,
                                 charge = DT$Charge,
                                 algo = 1,
                                 rel_to = 0,
                                 verbose = FALSE)


  ##################################
  #find and delete molecular formulas out of resolution calibration range
  ##################################
  filter.vct <- lapply(seq(length(pattern)), function(x, pat = pattern, .instrumentRes = as.data.table(instrumentRes), .charge_vct = DT$Charge){
    isotopologue.table <- as.data.table(pat[x])
    colnames(isotopologue.table)[1:2] <- c("m/z", "abundance")

    if(nrow(isotopologue.table[`m/z` < min(.instrumentRes$`m/z`) | `m/z` > max(.instrumentRes$`m/z`) | `m/z` * abs(.charge_vct[x]) > max(.instrumentRes$`m/z`)]) > 0){
      return(FALSE)
    } else{ return(TRUE)}
  })

  filter.vct <- as.data.table(filter.vct)
  filter.vct <- suppressWarnings(melt.data.table(filter.vct))



  if(nrow(filter.vct[value == FALSE]) > 0){
    pattern <- pattern[filter.vct$value]
    DT <- DT[filter.vct$value]
    SF <- SF[filter.vct$value]

    warning(paste0("Some molecular formulas lead to m/z values which are outside the range of m/z values for which resolution values are provided in the enviPat package. ",
                   "Those formulas are excldued. If you need to avoid that please provide additional R values to the instrumentRes table. The following molecular formulas have been excluded: ",
                   paste(as.character(filter.vct[value == FALSE]$variable), collapse = ", ")), noBreaks. = TRUE)
  }


  ##################################
  #calculate theoretical profile at given resolution
  ##################################
  profile <- enviPat::envelope(pattern = pattern,
                               frac = 0.1,
                               dmz = "get",
                               env = "Gaussian",
                               plotit = FALSE,
                               verbose = FALSE,
                               resolution = enviPat::getR(checked = SF,
                                                          resmass = instrumentRes,#getR(t, resmass = resolution_list$instrumentRes),
                                                          nknots = 6,
                                                          spar = 0.1,
                                                          plotit = FALSE))

  ##################################
  #calculate discrete m/z values and abundandences
  ##################################
  trash <- utils::capture.output({
    sticks <- enviPat::vdetect(profiles = profile,
                               detect = stick_method,
                               plotit = FALSE)
  })

  ##################################
  #reformate into final output table
  ##################################
  Output <- lapply(sticks, function(x) {if(any(class(x)=="matrix")) as.data.frame(x) else x})
  Output <- lapply(Output, function(x) x[, c("m/z", "abundance")])
  Output <- mapply(cbind,
                   "molecule"=as.character(DT$molecule),
                   "formula"=DT$SumForm_c,
                   "adduct"=DT$adduct_c,
                   "charge"=DT$Charge,
                   Output,
                   SIMPLIFY=FALSE
  )
  Output <- as.data.table(do.call("rbind", Output))
  Output <- Output[, .(isoab = abundance/max(abundance) * 100,
                       formula = formula,
                       charge = charge,
                       mz_ex = `m/z`),
                   by=.(molecule, adduct)]

  if(length(DTreg) > 3){
    DTreg[, adduct := adduct_c]
    Output <- Output[DTreg[, !c("adduct_c", "SumForm_c")], on = .(molecule, adduct)]
  }

  Output <- stats::na.omit(Output, col = "mz_ex")
  Output <- Output[isoab > RelInt_threshold]


  if(FileInfo == TRUE){
    Output <- Output[DT_fileInfo, on = .(molecule, adduct), allow.cartesian = TRUE]
  }


  ##################################
  #Exclude adducts with wrong charge
  ##################################
  Output <- stats::na.omit(Output)

  min_charge <- min(Output[main_adduct == adduct]$charge)
  max_charge <- max(Output[main_adduct == adduct]$charge)

  if((!(min_charge < 0 & max_charge < 0) & max_charge < 0) | (!(min_charge > 0 & max_charge > 0) & min_charge > 0)){
    stop("You included main_adducts with positive AND negative polarity. Only one is allowed at the same time!")
  }

  if(min_charge < 0 & max_charge < 0){
    if(nrow(Output[charge > 0]) > 0){
      warning("Adducts with opposite polarity of main_adduct will be excluded.")
    }
    Output <- Output[charge < 0]
  }

  if(min_charge > 0 & max_charge > 0){
    if(nrow(Output[charge < 0]) > 0){
      warning("Adducts with opposite polarity of main_adduct will be excluded.")
    }
    Output <- Output[charge > 0]
  }


  return(Output)
}
