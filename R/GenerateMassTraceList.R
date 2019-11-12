#' @title getMZtable
#'
#' @description This is basically a wrapper-function around the enviPat package. It takes a table with columns "molecule", "SumForm_c" and
#' "adduct_c" and calculates theoretical m/z values and abundances for isotopologues down to a user-defined abundance threshold.
#'
#'
#' @param DT data.table with columns "molecule", "SumForm_c" and "adduct_c" (see details)
#' @param instrumentRes data frame with mz vs resolution dependence (see \code{\link{resolution_list}}) e.g. "resolution_list$`OTFusion,QExactiveHF_120000@200`"
#' @param RelInt_threshold relative abundance of the lowest isotopologue to be considered
#' @param stick_method method that should be used to calculate discrete m/z values from calculated profile pattern e.g. "intensoid" (see \code{\link{vdetect}})
#' @param adducts data frame with adducts (see \code{\link{adducts}})
#' @param isotopes data frame of isotopes (see \code{\link{isotopes}})
#'
#' @details Make sure that molecular formulars in column "SumForm_c" only contain valid molecular formulas as described in \code{\link{check_chemform}}. Otherwise function, might
#' @details never finish! Additional columns in DT will be retained in the output of the function. However, the column names "adduct", "isoabb", "formula", "charge" and "mz" are reserved.
#'
#'
#' @import enviPat data.table
#' @return data.table with columns "molecule", "formula", "adduct", "charge", "m/z" and "abundance"
#' @export
#'
#' @examples

getMZtable <- function(DT, instrumentRes, RelInt_threshold = 0.05, stick_method = "intensoid", adducts, isotopes)
{

  if(!isTRUE(is.data.frame(DT))){stop(paste0("DT has to be a data frame!"))}
  if(!isTRUE(is.data.table(DT))){DT <- as.data.table(DT)}

  missing_cols <- setdiff(c("molecule", "SumForm_c", "adduct_c"), colnames(DT))
  if(length(missing_cols) > 0){stop(paste0("DT is lacking columns: ", missing_cols))}

  conflicting_cols <- intersect(c("adduct", "isoabb", "formula", "charge", "mz"), colnames(DT))
  if(length(conflicting_cols > 0)) stop(paste0("DT includes reserved column names! Specificly:", conflicting_cols))

  DT$molecule <- as.character(DT$molecule)

  ##################################
  #load necessary data
  ##################################
  if(missing(adducts)){data("adducts", envir = environment())}
  if(missing(isotopes)){data("isotopes", envir = environment())}



  #data("resolution_list", envir = environment())
  #df <- resolution_list$`OTFusion,QExactiveHF_120000@200`


  ##################################
  #check sum formulas and create working-table
  ##################################
  DTreg <- DT
  SF <- check_chemform(isotopes,DT$SumForm_c)
  DT <- cbind(DT,SF)
  DT <- merge(DT, adducts, by.x="adduct_c", by.y="Name")


  ##################################
  #calculate new sum formulas from adducts
  ##################################
  DT$SumForm2_c <- mapply(multiform,DT$new_formula,DT$Mult)
  DT$SumForm2_c <- mapply(function(formula1, formula2){if(formula2 != "FALSE") mergeform(formula1, formula2) else formula1}, DT$SumForm2_c, DT$Formula_add)
  DT$SumForm2_c <- mapply(function(formula1, formula2){if(formula2 != "FALSE") subform(formula1, formula2) else formula1}, DT$SumForm2_c, DT$Formula_ded)


  ##################################
  #check new sum formulas
  ##################################
  SF <- check_chemform(isotopes,DT$SumForm2_c)


  ##################################
  #calculate theoretical isotope pattern
  ##################################
  pattern <- isopattern(isotopes,
                        chemforms = SF$new_formula,
                        plotit = FALSE,
                        threshold = RelInt_threshold,
                        charge = DT$Charge,
                        algo = 1,
                        rel_to = 0,
                        verbose = FALSE)


  ##################################
  #calculate theoretical profile at given resolution
  ##################################
  profile <- envelope(pattern = pattern,
                      frac = 0.1,
                      dmz = "get",
                      env = "Gaussian",
                      plotit = FALSE,
                      verbose = FALSE,
                      resolution = getR(checked = SF,
                                        resmass = instrumentRes,#getR(t, resmass = resolution_list$instrumentRes),
                                        nknots = 6,
                                        spar = 0.1,
                                        plotit = FALSE))


  ##################################
  #calculate discrete m/z values and abundandences
  ##################################
  sticks <- vdetect(profiles = profile,
                    detect = stick_method,
                    plotit = FALSE)


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
  Output <- Output[, .(isoabb = abundance/max(abundance) * 100,
                       formula = formula,
                       charge = charge,
                       mz = `m/z`),
                   by=.(molecule, adduct)]

  if(length(DTreg) > 3){
    DTreg[, adduct := adduct_c]
    Output <- Output[DTreg[, !c("adduct_c", "SumForm_c")], on = .(molecule, adduct)]
  }

  return(Output)
}
