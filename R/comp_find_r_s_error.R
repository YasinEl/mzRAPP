#' find_r_s_error
#'
#' @param peak_area_b
#' @param peak_area_ug
#' @param peak_height_b
#'
#' @return
#' @export
#'
#' @examples
find_r_s_error <- function(peak_area_b, peak_area_ug, peak_height_b, Connected){

  temp_dt <- data.table(peak_area_b, peak_area_ug, peak_height_b, Connected)

  temp_dt[, r_s_error := NA_character_]

  if (all(is.na(temp_dt$peak_area_ug))){
    first_found_ug_area <- NA
    first_found_ug_height <- NA
  } else {
    first_found_ug_area <- temp_dt[which.min(peak_area_ug), peak_area_b]
    first_found_ug_height <- temp_dt[which.min(peak_area_ug), peak_height_b]
  }

  #No UG Peaks where found
  if(is.na(first_found_ug_area)){
    temp_dt[, r_s_error := 'L']
  }

  #UG Peaks were found
  else {
    #20% of first found area
    temp_dt[, first_found_area_temp := first_found_ug_area*2]
    temp_dt[is.na(peak_area_ug), r_s_error := ifelse((peak_area_b > first_found_ug_area * 1.5 & peak_height_b > first_found_ug_height * 1.5),
                                                     'R',
                                                     'S')
            ]

  }
  temp_dt[is.na(r_s_error), r_s_error := 'F']

  temp_dt[Connected == FALSE]$r_s_error <- 'NC'


  return(temp_dt$r_s_error)
}






#' File_con_test
#'
#' @param FileName
#' @param feature_id
#'
#' @return
#' @export
#'
#' @examples
File_con_test <- function(FileName, feature_id){

  sub_tab <- data.table(FileName, feature_id)

  tt <- na.omit(sub_tab)

  if(length(unique(tt$FileName)) <= 1 |
     length(unique(tt$feature_id)) <= 1){return(rep(FALSE, nrow(sub_tab)))}

  df <- as.data.frame.matrix(table(tt))

  name.vct <-
    lapply(df, function(x, cn = rownames(df)){
      return(cn[x > 0])
    })

  for (x in unique(tt$FileName)){

    sub.vct <- lapply(name.vct, function(y, var = x){if(var %in% y){y}})
    sub.vct <- sub.vct[lengths(sub.vct) != 0]
    subst.vct <- Reduce(union, sub.vct)
    sub.vct <- lapply(name.vct, function(y, var = x){if(!(var %in% y)){y}})
    name.vct <- sub.vct[lengths(sub.vct) != 0]
    name.vct[[(length(name.vct) + 1)]] <- subst.vct

  }

  con_files <- unname(unlist(name.vct[which.max(lengths(name.vct))]))
  sub_tab$Connected <- FALSE
  sub_tab[FileName %in% con_files]$Connected <- TRUE

  return(sub_tab$Connected)
}






