#' import_ungrouped_openms
#'
#' @param file_list
#' @param options_dt
#'
#' @return
#' @export
#'
#' @examples
import_ungrouped_openms <- function(file_list, options_dt){

  print('start OpenMS ug import')

  if(is.null(file_list)){
    stop('No ungrouped files selected')
  }

  if(length(file_list) < 2){
    stop('There should be multiple files for the unaligned OpenMS output!')
  }



  doFuture::registerDoFuture()
  #for(zeit in c("multiprocess", "sequential")){

  future::plan("sequential")
  #future::plan(list("sequential", "multiprocess"))
  #future::plan(multiprocess(workers = 40))

  t <- Sys.time()
  Output <- list()
  Output <-
    foreach(i = 1:length(file_list),
            .packages = c("mzRAPP")) %dopar% {



                  file_path <- file_list[i]
                  file_name <- tools::file_path_sans_ext(basename(file_path))

              #Check if ug_table exists, if not: create
              #ug_table <- fread(file_path, integer64 = "numeric", fill = TRUE, comment.char = "")


                    cols <- max(count.fields(file_path, sep = ","))

                    suppressWarnings(
                    ug_table <- read.table(file_path,
                                 col.names = paste("V", seq(cols)),
                                 fill = TRUE,
                                 sep = ",",
                                 comment.char = "",
                                 row.names = NULL,
                                 colClasses = "character")
                   )

                    ug_table <- as.data.table(ug_table)
                    colnames(ug_table)[1:2] <- c("V.1", "rpl")
                    if(!("#FEATURE" %in% unlist(ug_table[,"V.1"]))) {stop(paste("There is no information on FEATURES in " , file_path))}
                    ug_table <- ug_table[V.1 == "#FEATURE" | V.1 == "FEATURE"]
                    ug_table <- ug_table[,1:length(as.character(ug_table[1,])[as.character(ug_table[1,]) != ""])]
                    colnames(ug_table) <- as.character(unlist(unname(ug_table[V.1 == "#FEATURE"])))
                    ug_table <- ug_table[!1 ,!1]
                    ug_table <- ug_table[, sample_name := file_name]
                    return(ug_table)
            }

  ug_table <- data.table::rbindlist(Output, fill = TRUE, use.names = TRUE)

  #}

  #Add files to ut dt if name is in options ug_samples
#  for (i in 1:length(file_list)){
#    file_path <- file_list[i]
#    file_name <- tools::file_path_sans_ext(basename(file_path))

    #Check if ug_table exists, if not: create
#    if(!exists("ug_table")){
      #ug_table <- fread(file_path, integer64 = "numeric", fill = TRUE, comment.char = "")


#      cols <- max(count.fields(file_path, sep = ","))

#      suppressWarnings(
#      ug_table <- read.table(file_path,
#                   col.names = paste("V", seq(cols)),
#                   fill = TRUE,
#                   sep = ",",
#                   comment.char = "",
##                   row.names = NULL,
 #                  colClasses = "character")
 #     )

#      ug_table <- as.data.table(ug_table)
#      colnames(ug_table)[1:2] <- c("V.1", "rpl")
#      if(!("#FEATURE" %in% unlist(ug_table[,"V.1"]))) {stop(paste("There is no information on FEATURES in " , file_path))}
#      ug_table <- ug_table[V.1 == "#FEATURE" | V.1 == "FEATURE"]
#      ug_table <- ug_table[,1:length(as.character(ug_table[1,])[as.character(ug_table[1,]) != ""])]
#      colnames(ug_table) <- as.character(unlist(unname(ug_table[V.1 == "#FEATURE"])))
#      ug_table <- ug_table[!1 ,!1]
#      ug_table <- ug_table[, sample_name := file_name]
#    } else if (exists("ug_table")){
      #temp_data <- fread(file_path, integer64 = "numeric", fill = TRUE, comment.char = "")

#      cols <- max(count.fields(file_path, sep = ","))
#      suppressWarnings(
#      temp_data <- read.table(file_path,
##                             col.names = paste("V", seq(cols)),
 #                            fill = TRUE,
 #                            sep = ",",
 #                            comment.char = "",
 #                            row.names = NULL,
#                             colClasses = "character")
#      )
#      colnames(temp_data)[1:2] <- c("V.1", "rpl")
##      temp_data <- as.data.table(temp_data)
 #     if(!("#FEATURE" %in% unlist(temp_data[,"V.1"]))) {stop(paste("There is no information on FEATURES in " , file_path))}
 #     temp_data <- temp_data[V.1 == "#FEATURE" | V.1 == "FEATURE"]
#      temp_data <- temp_data[,1:length(as.character(temp_data[1,])[as.character(temp_data[1,]) != ""])]
#      colnames(temp_data) <- as.character(unname(unlist(temp_data[V.1 == "#FEATURE"])))
 ##     temp_data <- temp_data[!1 ,!1]
 #     temp_data <- temp_data[, sample_name := file_name]
 #     ug_table <- rbind(ug_table, temp_data)
 #   }
 # }

  #Check if all columns defined in optionsframe are present
  ug_req_cols <- na.omit(options_dt$ug_columns)
  if(!all(ug_req_cols %in% colnames(ug_table))){
    cols_not_found <- setdiff(ug_req_cols, colnames(ug_table))
    stop('Columns defined in options but not present in OpenMS output: ', paste0(cols_not_found, sep = " - "))
  }

  #rename all columns for internal use according to option frame
  ug_table <- rename_columns_from_options(ug_table, options_dt, 'ug_columns', 'internal_columns')

  #Add a sample_id based on the sample_names in options_dt
  ug_table <- ug_table[options_dt, ':=' (sample_id = i.sample_id), on=c(sample_name = 'ug_samples')]

  #convert factors to numeric
  #ug_table <- ug_table[, 'peak_area' := as.numeric(levels(peak_area))[peak_area]]
  #ug_table <- ug_table[, 'mz' := as.numeric(levels(mz))[mz]]
  #ug_table <- ug_table[, 'rt' := as.numeric(levels(rt))[rt]]
  #ug_table <- ug_table[, 'rt_start' := as.numeric(levels(rt_start))[rt_start]]
  #ug_table <- ug_table[, 'rt_end' := as.numeric(levels(rt_end))[rt_end]]
  ug_table <- ug_table[, 'peak_area' := as.numeric(peak_area)]
  ug_table <- ug_table[, 'mz' := as.numeric(mz)]
  ug_table <- ug_table[, 'rt' := as.numeric(rt)]
  ug_table <- ug_table[, 'rt_start' := as.numeric(rt_start)]
  ug_table <- ug_table[, 'rt_end' := as.numeric(rt_end)]

  #Remove peaks where height and area are below 0
  ug_table <- ug_table[peak_area > 0] #height does not exist

  #Generate comp_id for each peak
  ug_table$comp_id <- seq.int(nrow(ug_table))

  #Add "_ug" as suffix to each column name
  colnames(ug_table) <- paste(colnames(ug_table), 'ug', sep = '_')

  print(paste0('Successful OpenMS ug import. No. of peaks imported: ', nrow(ug_table)))

  return(ug_table)
}





#' import_grouped_openms
#'
#' @param file_path
#' @param options_table
#'
#' @return
#' @export
#'
#' @examples
import_grouped_openms <- function(file_path, options_table){

  print('start OpenMS g import')

  if(is.null(file_path)){
    return(NULL)
    stop('No grouped file selected')
  }

  #Check if filetype is csv
  if(tools::file_ext(file_path) != 'csv'){
    stop('grouped dataset is not a valid csv file')
  }

  if(length(file_path) != 1){
    stop('There should only be one file for the aligned MS-DIAL output!')
  }

  #Import csv file
  ######################g_table <- fread(file_path)#########################

  cols <- max(count.fields(file_path, sep = ","))

  openms_exp <- read.table(file_path,
                           col.names = paste("V", seq(cols)),
                           fill = TRUE,
                           sep = ",",
                           comment.char = "",
                           row.names = NULL,
                           colClasses = "character")

  openms_exp <- as.data.table(openms_exp)

  if(nrow(openms_exp[V.1 == "#CONSENSUS"]) != 1){
    stop('There is no #CONSENSUS row in aligned OpenMS export!')
  }


  #generate g_table
  g_table <- openms_exp[V.1 == "#CONSENSUS" | V.1 == "CONSENSUS"]
  g_table <- g_table[, 1:sum(g_table[1,] != "")]
  colnames(g_table) <- as.character(unname(unlist(g_table[1,])))
  g_table <- g_table[!1,!1]

  #agenerate sample id table
  sampid <- openms_exp[V.1 == "#MAP" | V.1 == "MAP"]
  sampid <- sampid[, 1:sum(sampid[1,] != "")]
  colnames(sampid) <- as.character(unname(unlist(sampid[1,])))
  sampid <- sampid[!1,!1]
  sampid[, vars := paste0("intensity_", id)]
  sampid <- sampid[, sample_name := tools::file_path_sans_ext(basename(as.character(filename)))]

  #Check if all columns defined in optionsframe are present
  g_req_cols <- na.omit(options_table$g_columns)
  if(!all(g_req_cols %in% colnames(g_table))){
    cols_not_found <- setdiff(g_req_cols, colnames(g_table))
    stop('Columns defined in options but not present in aligned OpenMS export: ', paste0(cols_not_found, sep = " - "))
  }


  #Add feature_id for each row
  #g_table$feature_id <- seq.int(nrow(g_table))
  g_table[, feature_id := seq.int(nrow(g_table))]

  #transforming table from wide to long format, creating 1 peak-per-row format
  id_vars <- append(na.omit(options_table[['g_columns']]), 'feature_id')
  measure_vars <- sampid$vars

  g_table <- melt(g_table,
                  id.vars = id_vars,
                  measure.vars = measure_vars,
                  variable.name = 'vars',
                  value.name = 'peak_area'
                  )

  #convert factors to numeric
  g_table <- g_table[, 'peak_area' := as.numeric(as.character(peak_area))]
  g_table <- sampid[, c("sample_name", "vars")][g_table, on=.(vars)][, !"vars"]

  #rename all columns for internal use according to optiosn frame
  g_table <- rename_columns_from_options(g_table, options_table, 'g_columns', 'internal_columns')

  #Add a sample_id column based on the sample_names in options_dt
  g_table <- g_table[options_table, ':=' (sample_id = i.sample_id), on=c(sample_name = 'g_samples')]

  #convert factors to numeric
  g_table <- g_table[, 'mz' := as.numeric(mz)]
  g_table <- g_table[, 'rt' := as.numeric(rt)]

  #Remove peaks where area is below or equal 0
  g_table <- g_table[peak_area > 0]

  #Add comp_id for each peak
  g_table$comp_id <- seq.int(nrow(g_table))

  #Add "_g" as suffix to each column name
  colnames(g_table) <- paste(colnames(g_table), 'g', sep = '_')

  print(paste0('Successful OpenMS g import. No. of peaks imported: ', nrow(g_table)))

  return(g_table)
}