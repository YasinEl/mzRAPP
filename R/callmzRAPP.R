#' callmzRAPP
#'
#' @return shiny app
#' @export
#'
#' @importFrom plotly plotlyOutput
#' @importFrom shiny observeEvent renderUI reactiveVal reactive renderText isolate observe withProgress incProgress updateTabsetPanel showModal
#' modalDialog req updateSelectInput renderTable downloadHandler
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom stats median
#' @import shinyjs
#'


callmzRAPP <- function(){


  #load resolution list from enviPat
  rm(list = ls(), envir = environment())
  if(!("resolution_list" %in% ls())){utils::data("resolution_list", envir = environment(), package = "enviPat")}
  if(!("adducts" %in% ls())){utils::data("adducts", envir = environment(), package = "enviPat")}

  #setup choice vectors
  choice_vector_bench <- c(
    'Retention time [s]' = 'peaks.rt_raw',
    'Points per peak' = 'peaks.PpP',
    'Mean scan to scan time [s]' = 'peaks.data_rate',
    'Sharpness' = 'peaks.Sharpness',
    'Jaggedness' = 'peaks.Jaggedness',
    'Modality'  = 'peaks.Modality',
    'Symmetry' = 'peaks.Symmetry',
    'TPASR' = 'peaks.TPASR',
    'MinDivMax' = 'peaks.MinDivMax',
    'FW25M' = 'peaks.FW25M',
    'FW50M'= 'peaks.FW50M',
    'FW75M' = 'peaks.FW75M',
    'Zigzag index' = 'peaks.zigZag_IDX',
    'Height' = 'peaks.height',
    'Area' = 'peaks.area',
    'mz measured' = 'peaks.mz_accurate',
    'mz accuracy abs' = 'peaks.mz_accuracy_abs',
    'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm',
    'mz range (abs)' = 'peaks.mz_span_abs',
    'mz range [ppm]' = 'peaks.mz_span_ppm',
    'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0',
    'RT neighbors' = 'peaks.rt_neighbors',
    'mz neighbors' = 'peaks.mz_neighbors',
    'Max. RT shift [s]' = 'rt_raw_span',
    'Molecule' = 'molecule',
    'Filename' = 'FileName',
    'Adduct' = 'adduct',
    'Theoretic isotopic abundance' = 'isoab',
    'Introduced sample group' = 'Grp',
    'Predicted area' = 'ExpectedArea',
    'Error_predicted area [%]' = 'ErrorRel_A',
    'Error_predicted area (abs)' = 'ErrorAbs_A',
    'Predicted height' = 'ExpectedHeight',
    'Error_predicted height [%]' = 'ErrorRel_H',
    'Error_predicted height (abs)' = 'ErrorAbs_H'
  )

  choice_vector_comp <- c(
    'Retention time [sec]' = 'rt_b',
    'Points per peak' = 'peaks.PpP_b',
    'Mean scan to scan time [s]' = 'peaks.data_rate_b',
    'Sharpness' = 'peaks.Sharpness_b',
    'Jaggedness' = 'peaks.Jaggedness_b',
    'Modality'  = 'peaks.Modality_b',
    'Symmetry' = 'peaks.Symmetry_b',
    'TPASR' = 'peaks.TPASR_b',
    'MinDivMax' = 'peaks.MinDivMax_b',
    'FW25M' = 'peaks.FW25M_b',
    'FW50M'= 'peaks.FW50M_b',
    'FW75M' = 'peaks.FW75M_b',
    'Zigzag index' = 'peaks.zigZag_IDX_b',
    'log10(Height)' = 'peak_height_b',
    'log10(Area)' = 'peak_area_b',
    'mz measured' = 'mz_b',
    'mz accuracy abs' = 'peaks.mz_accuracy_abs_b',
    'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm_b',
    'mz range (abs)' = 'peaks.mz_span_abs_b',
    'mz range [ppm]' = 'peaks.mz_span_ppm_b',
    'RT neighbors' = 'peaks.rt_neighbors_b',
    'mz neighbors' = 'peaks.mz_neighbors_b',
    'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0_b',
    'Max. RT shift [s]' = 'rt_raw_span_b',
    'Molecule' = 'molecule_b',
    'Filename' = 'sample_name_b',
    'Adduct' = 'adduct_b',
    'Theoretic isotopic abundance' = 'isoab_b',
    'Introduced sample group' = 'Grp_b',
    'Predicted area' = 'ExpectedArea_b',
    'Error_predicted area [%]' = 'ErrorRel_A_b',
    'Error_predicted area (abs)' = 'ErrorAbs_A_b',
    'Predicted height' = 'ExpectedHeight_b',
    'Error_predicted height [%]' = 'ErrorRel_H_b',
    'Error_predicted height (abs)' = 'ErrorAbs_H_b'
  )

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "mzRAPP"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "sbmenu",
        shinydashboard::menuItem("Readme", tabName = "Readme"),
        shinydashboard::menuItem("Generate Benchmark", tabName = "gBM_p"),
        shinydashboard::menuItem("View Benchmark", tabName = "vBM_p"),
        shinydashboard::menuItem("Setup NPP assessment", tabName = "sNPP_p"),
        shinydashboard::menuItem("View NPP assessment", tabName = "vNPP_p")
      )
    ),
    shinydashboard::dashboardBody(
      useShinyjs(),
      shinyjs::extendShinyjs(text = 'shinyjs.scrolltop = function() {window.scrollTo(0, 0)};', functions = c("scrolltop")), #always start from top of panel
      shiny::tags$script(shiny::HTML("$('body').addClass('fixed');")),
      shiny::tags$script(shiny::HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")), #function from https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard?rq=1



      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "Readme",
                                #shiny::column(12,
                                  shiny::tags$div(
                                    class = "rmd-class",
                                    shiny::includeHTML(system.file("md","README.html", package = "mzRAPP", mustWork = TRUE))
                                  )
                                #)
        ),
        shinydashboard::tabItem(tabName = "gBM_p",

                shiny::fluidRow(
                  shiny::column(5,
                         shiny::strong("1. Select necessary files", style = "font-size:30px"),
                         shiny::p("(If dialog boxes do not appear after clicking please check behind your open windows.)"),
                         shiny::a("For more information about those files check the section 'Benchmark dataset generation' in the Readme.", onclick = "openTab('Readme')", href="#sBM_readme")

                  )
                ),

                shiny::fluidRow(
                    shiny::column(6,
                                  if((grepl("windows", .Platform$OS.type))){
                                  shiny::fluidRow(
                                    shiny::column(4,
                                                  shiny::actionButton(inputId = 'mzML_upload',
                                                                      label = 'Select mzML files',
                                                                      width = '100%')
                                    ),
                                    shiny::column(4,
                                                  shiny::actionButton(inputId = 'grps_upload',
                                                                      label = 'Select sample-group file',
                                                                      width = '100%')
                                    ),
                                    shiny::column(4,
                                                  shiny::actionButton(inputId = 'coi_upload',
                                                                      label = 'Select target file',
                                                                      width = '100%')
                                    )
                                  )

                                    } else {
                                      shiny::fluidRow(
                                        shiny::column(4,
                                        shinyFiles::shinyFilesButton(id = "mzML_upload",
                                                                     label = "Select mzML files" ,
                                                                     title = "Please select a file:",
                                                                     multiple = TRUE,
                                                                     buttonType = "default",
                                                                     class = NULL)
                                        ),
                                  shiny::column(4,
                                                shinyFiles::shinyFilesButton(id = "grps_upload",
                                                                             label = "Select sample-group file" ,
                                                                             title = "Please select a file:",
                                                                             multiple = FALSE,
                                                                             buttonType = "default",
                                                                             class = NULL)
                                  ),
                                  shiny::column(4,
                                                shinyFiles::shinyFilesButton(id = "coi_upload",
                                                                             label = "Select target file" ,
                                                                             title = "Please select a file:",
                                                                             multiple = FALSE,
                                                                             buttonType = "default",
                                                                             class = NULL)
                                  )
                                      )

                                    },
                                  shiny::fluidRow(
                                    shiny::column(4,
                                                  shiny::verbatimTextOutput(outputId = 'mzML_upload_files',placeholder = TRUE)
                                    ),
                                    shiny::column(4,
                                                  shiny::verbatimTextOutput(outputId = 'grps_upload_file',placeholder = TRUE)
                                    ),
                                    shiny::column(4,
                                                  shiny::verbatimTextOutput(outputId = 'coi_upload_file',placeholder = TRUE)
                                    )
                                  )
                    )

                ),

                shiny::fluidRow(shiny::column(6,shiny::br())),

                shiny::fluidRow(
                  shiny::column(
                    12,
                    style = "display: inline-flex;",
                    shinyWidgets::prettySwitch(inputId = 'use_envipat_res_list',
                                 label = 'Envipat resolution list | Custom resolution list',
                                 value = FALSE,
                                 width = '190px')
                  )
                ),

                shiny::fluidRow(
                  shiny::conditionalPanel(condition = "!input.use_envipat_res_list",
                                   shiny::column(4,
                                          shiny::selectInput(
                                            'resolution_drop',
                                            'Select instrument & resolution',
                                            c('------', names(resolution_list)),
                                            selected = '------',
                                            width = "100%"
                                          ))
                  ),
                  shiny::conditionalPanel(condition = "input.use_envipat_res_list",
                                          if(grepl("windows", .Platform$OS.type)){
                                                                  shiny::column(2, shiny::actionButton(inputId = 'custom_res_mz',
                                                                                                       label = 'Select Res vs mz table',
                                                                                                       width = '100%'))
                                          } else {
                                            shiny::column(2,
                                            shinyFiles::shinyFilesButton(id = "custom_res_mz",
                                                                         label = "Select Res vs mz table" ,
                                                                         title = "Select Res vs mz table:",
                                                                         multiple = FALSE,
                                                                         buttonType = "default",
                                                                         class = NULL)
                                            )

                                     }
                                     #     )
                  )
                ),

                shiny::fluidRow(
                  shiny::conditionalPanel(condition = "input.use_envipat_res_list",
                                   shiny::column(2, shiny::verbatimTextOutput(outputId = 'custom_res_mz_file', placeholder = TRUE)
                                   )
                  )
                ),

                shiny::fluidRow(
                    shiny::column(4,
                    shinyWidgets::multiInput(
                      inputId = "adduct_choice",
                      label = "Would you like to screen for additional adducts? (Adducts with polarity not presents in target file will be ignored):",
                      choices = c(adducts$Name),
                      width = "350px",
                      selected = c("M+H", "M-H"),
                      options = list(
                        non_selected_header = "Choose between:",
                        selected_header = "You have selected:"
                      )
                    )
                  )
                ),

                shiny::fluidRow(
                  shiny::column(6,
                         shiny::strong("2. Set parameters", style = "font-size:30px"),
                         shiny::p("mz precision corresponds to the highest tolerable difference of mass peaks along the mz dimension in ppm. Typical values for orbitraps range from 5 to 8. For some TOF instruments this can go up to 20 ppm."),
                         shiny::p("mz accuracy corresponds to the maximum allowed difference between the theoretical mz of an ion species and the intensity weighthed average of all mz values over a chromatographic peak in ppm. Those values are typicaly between 3 and 5 for orbitrap as well as TOF instruments."),
                         shiny::br()
                  )
                ),

                shiny::fluidRow(
                  shiny::column(2,shiny::numericInput('RelInt_Thresh_input', 'Lowest iso. to be considered [%]', 0.05, step = 0.01, max = 100)),
                  shiny::column(2,shiny::numericInput('min_PpP_input', 'Min. # of scans per peak', 10, step = 1, min = 5))
                ),

                shiny::fluidRow(
                  shiny::column(2,shiny::numericInput('percision_mz_tol_input', 'mz precision [ppm]', 5, step = 0.1)),
                  shiny::column(2,shiny::numericInput('accurate_MZ_tol_input', 'mz accuracy [ppm]', 5, step = 0.1))
                ),

                shiny::fluidRow(
                  shiny::column(2,shiny::selectInput('plan_input', 'Processing plan', c('multiprocess', 'sequential')))
                ),

                shiny::fluidRow(
                  shiny::column(6,
                         shiny::strong("3. Start benchmark generation", style = "font-size:30px"),
                         shiny::br(),
                         shiny::p("(depending on the number of files and compounds this can take some time (minutes to hours))")
                  )
                ),
                shiny::fluidRow(shiny::column(6,shiny::br())),
                shiny::fluidRow(
                  shiny::column(4,shiny::actionButton('generate_benchmark', 'Generate benchmark', style = "background-color: #2596be;color: white;padding:20px; font-size:150%"))
                ),
        ),

        shinydashboard::tabItem(tabName = "vBM_p",
                shiny::h2("Generated benchmark:"),
                shiny::h4(paste("In the following key data and overview graphics for the generated benchmark are provided. Please note that the benchmark dataset contains only peaks which were confirmable",
                         "via associated isotopologues. Peaks which can not be confirmed are removed. This is behavior is intended and missed peaks do not cause problems for subsequent",
                         "non-targeted data pre-processing evaluations as long as there are overall enough peaks in the benchmark.")),
                shiny::a("Click here for more information on how to proceed from here.", onclick = "openTab('Readme')", href="#vBMID"),

                shiny::br(),

                shiny::fluidRow(
                  shinydashboard::infoBoxOutput("size_info"),
                  shinydashboard::infoBoxOutput("chrom_info"),
                  shinydashboard::infoBoxOutput("mz_info")
                ),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),

                shiny::fluidRow(
                  shiny::column(7, offset = 1,
                         shiny::h4(paste("In the following interactive scatter plot and histogram different benchmark peak variables can be explored. In order to have a look at the actual peaks you can ",
                                  "click on the points in the scatter plot or export the benchmark to Skyline (button to the right) to get a more comprehensive overview. If you wish to adjust peak boundaries you have to repeat the benchmark ",
                                  "with filled out 'user.rtmin', 'user.rtmax' (and 'FileName') columns as described in the Readme (click the link below to get there).")),
                         shiny::a("Click here for more information on how to proceed if you are not satisfied with the current benchmark dataset.", onclick = "openTab('Readme')", href="#vBMID")
                  ),
                  shiny::column(3,
                         shiny::checkboxInput(inputId = 'Sky_add', label = 'Export only main adduct', value = TRUE, width = NULL),
                         shiny::checkboxInput(inputId = 'Sky_iso', label = 'Export only most abundant isotopologue', value = TRUE, width = NULL),
                         shiny::actionButton(inputId = 'Skyline_export',
                                      label = shiny::HTML('Export Skyline Transition list<br/>and peak boundaries'),
                                      width = '100%')
                  )),

                shiny::fluidRow(
                  shiny::column(6, offset = 1,
                         shiny::fluidRow(
                           shiny::column(1,
                                  #shinyWidgets::dropdownButton(shiny::br(""),
                                  #               tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                  #               circle = TRUE,
                                  #               width = 600,
                                  #               status = 'info',
                                  ##               icon = shiny::icon('question-circle'),
                                   #              size = 'sm'
                                  #)
                           )
                         ),

                         shiny::fluidRow(shiny::column(12, plotly::plotlyOutput('graph_area_bench_overview') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))),

                         shiny::fluidRow(
                           shiny::tags$style(shiny::HTML("#bench_overview_input_x+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                           shiny::tags$style(shiny::HTML("#bench_overview_input_y+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                           shiny::tags$style(shiny::HTML("#bench_overview_input_color+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                           shiny::column(3,
                                  shiny::selectInput('bench_overview_input_x',
                                              'x-axis',
                                              choice_vector_bench,
                                              selected = "ExpectedArea"
                                  )
                           ),
                           shiny::column(3,
                                  shiny::selectInput('bench_overview_input_y',
                                              'y-axis',
                                              choice_vector_bench,
                                              selected = 'peaks.area'
                                  )
                           ),
                           shiny::column(3,
                                  shiny::selectInput('bench_overview_input_color',
                                              'color-by',
                                              choice_vector_bench,
                                              selected = 'molecule'
                                  )
                           )

                         )
                  ),
                  shiny::column(4,
                         shiny::fluidRow(
                        #  shiny::column(1,
                        #         shinyWidgets::dropdownButton(
                        #            shiny::br('1'),
                        #            tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                        #            circle = TRUE,
                        #            width = 600,
                        #            status = 'info',
                        #            icon = shiny::icon('question-circle'),
                        #            size = 'sm'
                        #          )
                        #  )
                         ),

                         shiny::fluidRow(shiny::column(12,plotly::plotlyOutput('graph_area_bench_histo') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))),

                         shiny::fluidRow(
                           shiny::tags$style(shiny::HTML("#select_bench_histo+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                           shiny::column(8,
                                  shiny::selectInput('select_bench_histo',
                                              'Peak variable',
                                              choice_vector_bench,
                                              selected = 'peaks.PpP'
                                  )
                           )
                         )
                  )
                ),

                shiny::br()


        ),

        shinydashboard::tabItem(tabName = "sNPP_p",

                shiny::fluidRow(
                  shiny::column(
                    12, shiny::strong('1. Select used tool', style = "font-size:30px"), shiny::br(), shiny::br()
                  )
                ),

                shiny::fluidRow(
                  shiny::column(

                    12, shiny::selectInput('algorithm_input', 'Non-targeted tool used', c('---', 'XCMS', 'XCMS3', 'Metaboanalyst', 'SLAW', 'MS-DIAL', 'MZmine 2', 'El-MAVEN', 'OpenMS'), selected = '---')
                  )#'CompoundDiscoverer',
                ),

                shiny::fluidRow(
                  shiny::column(
                    12, shiny::strong('2. Select unaligned and aligned files', style = "font-size:30px"), shiny::br(),
                    shiny::a("For information on how to export non-targeted results please check section 'Exporting NPP outputs from different tools' of the Readme.",
                      onclick = "openTab('Readme')",
                      href="#sNPP_readme"),
                    shiny::br()
                  )
                ),
             #   shiny::conditionalPanel(condition = grepl("windows", .Platform$OS.type),
                                        shiny::fluidRow(
                                          if((grepl("windows", .Platform$OS.type))){
                                          shiny::column(
                                            12,
                                            style = "display: inline-flex;",
                                            shiny::actionButton(inputId = 'ug_upload',
                                                                label = 'Select unaligned file(s)',
                                                                width = '190px'),
                                            shiny::div(style = "width: 20px;"),
                                            shiny::actionButton(inputId = 'g_upload',
                                                                label = 'Select aligned file',
                                                                width = '190px')
                                          )
                                          } else {
                                            shiny::column(
                                              12,
                                              style = "display: inline-flex;",
                                              shinyFiles::shinyFilesButton(id = "ug_upload",
                                                                           label = "Select unaligned file(s)" ,
                                                                           title = "Select unaligned file(s)",
                                                                           multiple = TRUE,
                                                                           buttonType = "default",
                                                                           class = NULL),
                                              shiny::div(style = "width: 20px;"),
                                              shinyFiles::shinyFilesButton(id = "g_upload",
                                                                           label = "Select aligned file" ,
                                                                           title = "Select aligned file",
                                                                           multiple = FALSE,
                                                                           buttonType = "default",
                                                                           class = NULL)
                                            )
                                          }
                ),

                shiny::fluidRow(
                  shiny::column(
                    12,
                    style = "display: inline-flex;",
                    shiny::div(style='width:190px', shiny::verbatimTextOutput(outputId = 'ug_upload_files',placeholder = TRUE)),
                    shiny::div(style = "width: 20px;"),
                    shiny::div(style='width:190px', shiny::verbatimTextOutput(outputId = 'g_upload_file',placeholder = TRUE)),
                    shiny::div(style = "width: 20px;")
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    12, shiny::strong('3. Select benchmark', style = "font-size:30px"), shiny::br(), shiny::br()
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    12,
                    style = "display: inline-flex;",
                    shinyWidgets::prettySwitch(inputId = 'use_generated_benchmark',
                                 label = 'Use generated benchmark',
                                 value = FALSE,
                                 width = '190px'),
                    shiny::div(style = "width: 20px;"),
                    shinyWidgets::prettySwitch(inputId = 'use_generated_options',
                                 label = 'Use generated options',
                                 value = TRUE,
                                 width = '190px')
                  )
                ),



                                        shiny::fluidRow(
                                          shiny::column(
                                            12,
                                            style = "display: inline-flex;",
                                            shiny::div(style = "width:190px",
                                                       shiny::conditionalPanel(condition = "!input.use_generated_benchmark",
                                                                               if((grepl("windows", .Platform$OS.type))){
                                                                               shiny::actionButton(inputId = 'benchmark_upload',
                                                                                                   label = 'Select benchmark file',
                                                                                                   width = '190px')
                                                                               } else {
                                                                                 shinyFiles::shinyFilesButton(id = "benchmark_upload",
                                                                                                              label = "Select benchmark file" ,
                                                                                                              title = "Select benchmark file",
                                                                                                              multiple = FALSE,
                                                                                                              buttonType = "default",
                                                                                                              class = NULL)
                                                                               }

                                                       )
                                            ),
                                            shiny::div(style = "width: 20px;"),



                                                       shiny::conditionalPanel(condition = "!input.use_generated_options",
                                                                               if((grepl("windows", .Platform$OS.type))){
                                                                                 shiny::div(style = "width:190px",
                                                                               shiny::actionButton(inputId = 'options_upload',
                                                                                                   label = 'Select options files',
                                                                                                   width = '190px')
                                                                                 )

                                                                               }else {

                                                                                 shiny::div(style = "width:190px",
                                                                                 shinyFiles::shinyFilesButton(id = "options_upload",
                                                                                                              label = "Select options files" ,
                                                                                                              title = "Select options files",
                                                                                                              multiple = FALSE,
                                                                                                              buttonType = "default",
                                                                                                              class = NULL)
                                                                                 )

                                                                               }
                                                       )


                                          #  )
                                          )

                ),

                shiny::fluidRow(
                  shiny::column(
                    12,
                    style = "display: inline-flex;",
                    shiny::div(style="width:190px",
                        shiny::conditionalPanel(condition = "!input.use_generated_benchmark",
                                         shiny::div(style='width:190px', shiny::verbatimTextOutput(outputId = 'benchmark_upload_file',placeholder = TRUE)),
                        )
                    ),
                    shiny::div(style = "width: 20px;"),
                    shiny::div(style = "width:190px",
                        shiny::conditionalPanel(condition = "!input.use_generated_options",
                                         shiny::div(style='width:190px', shiny::verbatimTextOutput(outputId = 'options_upload_file',placeholder = TRUE))
                        )
                    )
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    12, shiny::strong('4. Start NPP assessment', style = "font-size:30px"), shiny::br(), shiny::br()
                  )
                ),

                shiny::fluidRow(shiny::column(6, shiny::br())),

                shiny::fluidRow(
                  shiny::column(12,
                         style = "display: inline-flex;",
                         shiny::actionButton('start_compare', 'Start assessment', style =
                                               "background-color: #2596be;color: white;padding:20px; font-size:150%")
                  )
                )

        ),

        shinydashboard::tabItem(tabName = "vNPP_p",
                shiny::h1('NPP assessment results'),
                shiny::h4(paste("Key performance measures are given for different stages of the NPP workflow. Empirical confidence intervals (alpha = 0.95) of calculated percentages are given in brackets (estimated via bootstrapping with R = 1000). For details on how individual performance measures are calculated please check the readme.")),
                shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#MetricsID"),

                shiny::br(),

                shiny::fluidRow(
                  shinydashboard::infoBoxOutput("PP_info"),
                  shinydashboard::infoBoxOutput("A_info"),
                  shinydashboard::infoBoxOutput("F_info")
                ),

                shiny::br(),

                shiny::fluidRow(
                shiny::downloadButton("report", "Generate report"), align="center"
                ),
                shiny::h4(paste("For more details please check the interactive plots below.")),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),
                shiny::column(10, offset = 1, shiny::h2("Distribution of found/not found peaks")),

         shiny::fluidRow(
           shiny::column(10, offset = 1,
                         shiny::h4("In the following interactive scatter plot and histogram the distribution of", shiny::tags$span(style="color:#82e0aa", "found") ,"/", shiny::tags$span(style="color:#ccd1d1", "not found"), "peaks can be investigated as a function of different benchmark peak variables.",
                                   "Points in the scatter plot can be clicked to inspect individual peaks. Regarding the sunburst plot, the inner donut corresponds to picked peaks while the outer donut classifies peaks in aligned features."),
                         shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Matching_peaks")
                         )
           ),
         shiny::br(),
         shiny::fluidRow(
           shiny::fluidRow(
               shiny::column(1, offset = 1,
                             #shinyWidgets::dropdownButton(
                             #  shiny::br('1'),
                            #   tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                            #   circle = TRUE,
                            #   width = 600,
                            #   status = 'info',
                            #   icon = shiny::icon('question-circle'),
                            #   size = 'sm'
                            # )
               ),
               shiny::column(8,
                             shinyWidgets::prettySwitch(inputId = 'PP_al_switch_dist',
                                                        label = 'after PeakPicking | Alignment',
                                                        slim = TRUE,
                                                        value = FALSE
                             )
               )

           ),
           shiny::column(6, offset = 1,


                         shiny::fluidRow(shiny::column(12,plotly::plotlyOutput('graph_area_3') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))),

                         shiny::fluidRow(
                           shiny::column(8,
                                         shiny::selectInput('graph_select_input',
                                                            'x-axis',
                                                            choice_vector_comp,
                                                            selected = 'peak_height_b'
                                         )
                           )
                         )
           ),
           shiny::column(4,
                         plotly::plotlyOutput('sunburst_pp') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))
         ),
         shiny::br(),

                shiny::fluidRow(
                  shiny::column(10, offset = 1,
                         shiny::fluidRow(
                           shiny::column(1,
                                  shinyWidgets::dropdownButton(shiny::br("Orange point refer to split peaks.\nPlotted NPP peak boundaries (after clicking on one of the points) refer to the peak picking step."),
                                                 tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                                 circle = TRUE,
                                                 width = 600,
                                                 status = 'info',
                                                 icon = shiny::icon('question-circle'),
                                                 size = 'sm'
                                  )
                           ),
                           shiny::column(11,
                                  shinyWidgets::prettySwitch(inputId = 'PP_al_switch_ov',
                                               label = 'after PeakPicking | Alignment',
                                               slim = TRUE,
                                               value = FALSE
                                  )
                           )
                         ),

                         shiny::fluidRow(shiny::column(12, plotly::plotlyOutput('overview_plot') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))),

                         shiny::fluidRow(
                           shiny::column(3,
                                  shiny::selectInput('overview_plot_input_x',
                                              'x-axis',
                                              choice_vector_comp
                                  )
                           ),
                           shiny::column(3,
                                  shiny::selectInput('overview_plot_input_y',
                                              'y-axis',
                                              choice_vector_comp,
                                              selected = 'peak_height_b'
                                  )
                           ),
                           shiny::column(3,
                                  shiny::selectInput('overview_plot_input_col',
                                              'color-by',
                                              c("F/NF", choice_vector_comp),
                                              selected = 'F/NF'
                                  )
                           )

                         )
                  )

                ),

                shiny::br(),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),

                shiny::column(10, offset = 1, shiny::h2("Quality of reported NPP peak abundances")),
                shiny::column(10, offset = 1,
                       shiny::h4(paste0("The quality of reported peak abundances is important to determine molecular compositions via isotopologue ratios or compare abundances between samples. Since the former can be predicted when the molecular formula is known it can be used to estimate the quality of peak abundances reported by NPP. To inspect peaks contributing to a ratio click on the edges or center of the line plot. Regarding the sunburst plot, the inner donut corresponds to picked peaks while the outer donut relates to aligned features.")),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Peak_quality")
                ),
                shiny::br(),
                shiny::column(10, offset = 1,

                       shiny::fluidRow(
                         shiny::column(1,
                                shinyWidgets::dropdownButton(shiny::br('Please note that plotted NPP-integration boundries always correspond to boundaries from the peak picking step.\nInc.>20%p corresponds to isotopologue ratio biases which increased by more than 20 percentage points as compared to the benchmark.\nInc.<20%p to IR which increased by less than 20%p. \nFeature Inc.>20%p means that the increase was <20%p at the peak picking step but increased then to >20%p after feature processing.'),
                                               tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                               circle = TRUE,
                                               status = 'info',
                                               icon = shiny::icon('question-circle'),
                                               size = 'sm',
                                               width = 600
                                )
                         )
                       ),

                       shiny::fluidRow(
                         shiny::column(7,plotly::plotlyOutput('graph_area_2') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)),
                         shiny::column(5,plotly::plotlyOutput('sunburst_pq') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))
                         )
                ),


                shiny::br(),
                shiny::br(),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),
                shiny::column(10, offset = 1, shiny::h2("Missing value classification")),

                shiny::column(8, offset = 2,
                       shiny::h4(paste0("The nature of missing values is of utmost importance for the choice of a fitting missing value imputation method. Since features defined in the benchmark could ",
                                 "be aligned incorrectly only benchmark peaks for which the alignment was confirmed via NPP are considered. Others are labeled as 'not confirmable' (NC).")),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Missing_values")
                ),
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(8, offset = 2,
                         shiny::fluidRow(
                           #shiny::column(1,
                           #       shinyWidgets::dropdownButton(shiny::br(""),
                           #                      tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                           #                      circle = TRUE,
                           #                      status = 'info',
                           #                      icon = shiny::icon('question-circle'),
                           #                      size = 'sm',
                           #                      width = 600
                           #       ),
                           #),
                           shiny::column(2,
                                  shinyWidgets::prettySwitch(inputId = 'PP_al_switch_hm',
                                               label = 'after PeakPicking | Alignment',
                                               slim = TRUE,
                                               value = FALSE)
                           ),
                           shiny::column(2, offset = 3,
                                  shinyWidgets::prettySwitch(inputId = 'PP_al_switch_hm_off',
                                               label = 'off by default (often long loading time)',
                                               value = FALSE)
                           )

                         ),
                         shiny::fluidRow(shiny::column(12,plotly::plotlyOutput('graph_area_1') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)))
                  ),

                ),

                shiny::br(),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),
                shiny::column(10, offset = 1, shiny::h2("Errors in the alignment process")),
                shiny::column(10, offset = 1,
                       shiny::h4(paste0("The alignment process is responsible for assembling peaks of different samples into features. mzRAPP is counting errors in the alignment processes by checking whether the alignment is performed symmetrically over different isotopologues of the same compound. This way alignment errors in the benchmark do not affect this count. Divergences from the benchmark which can not be confirmed as errors as described are counted separately (BM.div). Peaks which were matched from the peak picking step but not found in the aligned output are also counted separately (Lost/Lost_b.A)Regarding the sunburst plot, from inside to outside the donuts correspond to peaks found during peak detection, aligned/lost peaks, correct/incorrect alignments and error type.")),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Alignment_counting")
                ),
                shiny::br(),

         shiny::fluidRow(
           shiny::column(10, offset = 1,
                         shiny::column(6, shiny::tableOutput('error_count'), style="overflow-y:scroll; height:375px"),
                         shiny::column(6, plotly::plotlyOutput('sunburst_al') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))
                         )
         ),


         shiny::br(),



                shiny::fluidRow(
                  shiny::column(10, offset = 1,

                         shiny::fluidRow(
                           shiny::column(12,
                                  plotly::plotlyOutput('graph_hm_split') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)
                           )),

                         shiny::fluidRow(
                           shiny::column(4, shinyWidgets::pickerInput('mol_a', 'Molecule', c(), options = list(`live-search`=TRUE))),
                           shiny::column(3, shinyWidgets::pickerInput('add_a', 'Adduct', c(), options = list(`live-search`=TRUE)))
                           )
                         )
                  )
         )
        )
      )
    )





  server <- function (input, output, session) {
      observeEvent(input$sbmenu, { #always start from top of panel
        if(input$sbmenu != "Readme"){
          js$scrolltop()
        }
      })

    output$Readme_op <- renderUI({
      shiny::tags$iframe(seamless="seamless", src= system.file("md","README.html", package = "mzRAPP", mustWork = TRUE), width=800, height=800)
    })

    ##Reactive Values
    data_dir <- reactiveVal(getwd())
    benchmark_data <- reactiveVal(NULL)
    comparison_data <- reactiveVal(NULL)
    if(!grepl("windows",.Platform$OS.type)){
      volumes = shinyFiles::getVolumes()
    }

    ##File Filters for choice dialogues
    #mzML_filter <- matrix(c('mzML Files (*.mzML)', '*.mzML'), nrow = 1, ncol = 2)
    #csv_filter <- matrix(c('Files (*.csv, *.txt, *.Rda)', '*.csv;*.txt;*.Rda'), nrow = 1, ncol = 2)

    mzML_filter <- matrix(c('mzML Files', '.mzML',
                            'All Files', '*'), 2, 2, byrow = TRUE)
    csv_filter <- matrix(c('Supported Files', '.csv',
                           'Supported Files', '.txt',
                           'Supported Files', '.Rda',
                           'All Files','*'), 4, 2, byrow = TRUE)

    #File input reactives
    #Benchmark
    mzML_files <- shiny::eventReactive(input$mzML_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "mzML_upload", roots = volumes, session = session)
        if(!is.null(input$mzML_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$mzML_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$mzML_upload_files <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$mzML_upload_files <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if (input$mzML_upload == 0){return(NULL)}
        else if(grepl("windows",.Platform$OS.type)){
          files <- tcltk::tk_choose.files(caption = 'Select .mzML files', multi = TRUE, filters = mzML_filter)
          if (length(files) > 1){
            output$mzML_upload_files <- renderText(paste0(length(files), ' Files selected'))
          } else {
            output$mzML_upload_files <- renderText(paste0(basename(files)))
          }
          return(files)
        }


      }
    })



    grps_file <- shiny::eventReactive(input$grps_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "grps_upload", roots = volumes, session = session)
        if(!is.null(input$grps_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$grps_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$grps_upload_file <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$grps_upload_file <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if(input$grps_upload[1] == 0){return(NULL)}
        else if(grepl("windows",.Platform$OS.type)) {
          file <- paste(tcltk::tk_choose.files(caption = 'Select sample-group file', multi = FALSE, filters = csv_filter), collapse = " ")
          output$grps_upload_file <- renderText(paste0(basename(file)))
          return(file)
        }

      }
    })







    coi_file <- reactive({

    })
    coi_file <- shiny::eventReactive(input$coi_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "coi_upload", roots = volumes, session = session)
        if(!is.null(input$coi_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$coi_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$coi_upload_file <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$coi_upload_file <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if(input$coi_upload == 0){return(NULL)}
        else  if(grepl("windows",.Platform$OS.type)) {
          file <- paste(tcltk::tk_choose.files(caption = 'Select target file', multi = FALSE, filters = csv_filter), collapse = " ")
          output$coi_upload_file <- renderText(paste0(basename(file)))
          return(file)
        }

      }
    })




    res_file <- shiny::eventReactive(input$custom_res_mz, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "custom_res_mz", roots = volumes, session = session)
        if(!is.null(input$custom_res_mz)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$custom_res_mz)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$custom_res_mz_file <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$custom_res_mz_file <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if(input$custom_res_mz == 0){return(NULL)}
        else if(grepl("windows",.Platform$OS.type)){
          file <- paste(tcltk::tk_choose.files(caption = 'Select Res/mz file', multi = FALSE, filters = csv_filter),collapse = " ")
          output$custom_res_mz_file <- renderText(paste0(basename(file)))
          return(file)
        }

      }
    })


    observeEvent(input$Skyline_export, {


      benchmark_data <- isolate(benchmark_data())
      benchmark_data <- benchmark_data$PCal

      if(input$Sky_add == TRUE){
        benchmark_data <- benchmark_data[adduct == main_adduct]
      }
      if(input$Sky_iso == TRUE){
        benchmark_data <- benchmark_data[isoab == 100]
      }

      SkyTranList <- SkylineTransitionList(benchmark_data)
      SkyPeakBo <- SkylinePeakBoundaries(benchmark_data)

      cat(paste0("Please go to 'Skyline -> Settings -> Transition Settings -> Full-Scan -> Mass Accuracy' \nand set 'Precursor mass analyzer' to 'Centroided' and ",
                 "Mass Accuracy to ", round_woe(max(benchmark_data$peaks.mz_span_ppm) / 2, 1), " ppm. \nAlso make sure that 'Isotope peaks included:' is set to 'Count' and 'Peaks:' to '1'. \n",
                 "On the bottom of the window you should check 'Include all matching scans'.\n",
                 "Then go to 'Skyline -> Settings -> Transition Settings -> Filter' and make sure Ion types is set to 'p'.\n",
                 "\nYou can then load this Transition list into Skyline via 'Skyline -> File -> Import -> Transition List...'.\n",
                 "\nAfterwards you can import in your mzML files via 'File -> Import -> Results.' Make sure you that you do \nnot allow Skyline to change the names of files if asked.\n",
                 "\nFinally peak boundaries can be importet via 'Skyline -> File -> Import -> Peak Boundaries...'"),
          file="Skyline_Instructions_For_Import.txt",sep="\n")

      shinyWidgets::sendSweetAlert(session,
                     title = 'Files exported',
                     text = paste0('The Skyline Transition list and peak boundaries have been exported as csv files to your current working directoy (', getwd(), ').
                                     Also a txt file with instructions on how to import those files in Skyline and how to set parameters was exported to the same directory.'),
                     type = 'success',
                     closeOnClickOutside = FALSE,
                     showCloseButton = TRUE)

    })


    #Comparison
    ug_files <- shiny::eventReactive(input$ug_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "ug_upload", roots = volumes, session = session)
        if(!is.null(input$ug_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$ug_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$ug_upload_files <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$ug_upload_files <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if (input$ug_upload == 0){return(NULL)}
        else if(grepl("windows", .Platform$OS.type)){
          files <- tcltk::tk_choose.files(caption = 'Select unaligned file(s)', multi = TRUE, filters = csv_filter)
          if (length(files) > 1){
            output$ug_upload_files <- renderText(paste0(length(files), ' Files selected'))
          } else {
            output$ug_upload_files <- renderText(paste0(basename(files)))
          }
          return(files)
        }

      }
    })


    g_file <- shiny::eventReactive(input$g_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "g_upload", roots = volumes, session = session)
        if(!is.null(input$g_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$g_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$g_upload_file <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$g_upload_file <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if (input$g_upload == 0) {return(NULL)}
        else if(grepl("windows", .Platform$OS.type)){
          file <- paste(tcltk::tk_choose.files(caption = 'Select aligned file', multi = FALSE, filters = csv_filter), collapse = " ")
          output$g_upload_file <- renderText(paste0(basename(file)))

          output$g_upload_file <- renderText(paste0(basename(file)))
          return(file)
        }

      }
    })


    benchmark_file <- shiny::eventReactive(input$benchmark_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "benchmark_upload", roots = volumes, session = session)
        if(!is.null(input$benchmark_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$benchmark_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$benchmark_upload_file <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$benchmark_upload_file <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if (input$benchmark_upload == 0){return(NULL)}
        else if(grepl("windows", .Platform$OS.type)){
          file <- paste(tcltk::tk_choose.files(caption = 'Select benchmark file', multi = FALSE, filters = csv_filter), collapse = " ")
          output$benchmark_upload_file <- renderText(paste0(basename(file)))
          return(file)
        }

      }
    })


    options_file <- shiny::eventReactive(input$options_upload, {
      if(!grepl("windows",.Platform$OS.type)){
        shinyFiles::shinyFileChoose(input, "options_upload", roots = volumes, session = session)
        if(!is.null(input$options_upload)){
          # browser()
          files<-shinyFiles::parseFilePaths(volumes, input$options_upload)
          files <- unname(files$datapath)
          if (length(files) > 1){
            output$options_upload_file <- renderText(paste0(length(files), ' Files selected'))
          } else if (length(files == 1)){
            output$options_upload_file <- renderText(paste0(basename(files)))
          }
          return(files)
        }
      } else {

        if (input$options_upload == 0){return(NULL)}
        else if(grepl("windows", .Platform$OS.type)){
          file <- paste(tcltk::tk_choose.files(caption = 'Select options file', multi = FALSE, filters = csv_filter), collapse = " ")
          output$options_upload_file <- renderText(paste0(basename(file)))
          return(file)
        }

      }
    })


    #General Observers
    observe({mzML_files()})
    observe({grps_file()})
    observe({coi_file()})
    observe({res_file()})
    observe({ug_files()})
    observe({g_file()})
    observe({benchmark_file()})
    observe({options_file()})

    observeEvent(input$generate_benchmark, {
      disable("generate_benchmark")


      tryCatch({
        #Get Files from reactives
        #mzML
        if(is.null(mzML_files()) || length(mzML_files()) == 0){
          stop('no mzML files selected')
        } else {
          files <- mzML_files()
        }

        #grps
        if(is.null(grps_file()) || length(grps_file()) == 0){
          stop('No grps file selected')
        } else {
          grps <- data.table::fread(grps_file())

          missing_cols <- setdiff(c("sample_name", "sample_group"), colnames(grps))
          if(length(missing_cols) > 0){stop(paste0("Sample_group table is lacking columns: ", paste(missing_cols, collapse = ", ")))}

          if(nrow(grps[is.na(sample_name) | is.na(sample_group)]) > 0){
            stop("Values in sample_group table must not be empty! If you have only one group please add the same sample_group value for each sample!")
          }

        }

        #coi
        if(is.null(coi_file()) || length(coi_file()) == 0){
          stop('No target file selected')
        } else {
          targets <- data.table::fread(coi_file())

          missing_cols <- setdiff(c("molecule", "SumForm_c", "main_adduct"), colnames(targets))
          if(length(missing_cols) > 0){stop(paste0("Target.table is lacking columns: ", paste0(missing_cols, collapse = ", ")))}


          if(all(c("StartTime.EIC", "EndTime.EIC") %in% colnames(targets)) && (!is.numeric(targets$StartTime.EIC) | !is.numeric(targets$EndTime.EIC))){
            stop("Some values in your columns StartTime.EIC/EndTime.EIC are not numeric!")

            if(nrow(targets[StartTime.EIC < 0 | EndTime.EIC < 0 | EndTime.EIC < StartTime.EIC]) > 0){
              stop("Values for StartTime.EIC and EndTime.EIC cannot be lower than 0. Also values for StartTime.EIC have to be lower than those for EndTime.EIC!")
            }

          }

          if("user.rtmin" %in% colnames(targets) | "user.rtmax" %in% colnames(targets)){
            if(!all(c("user.rtmin", "user.rtmax") %in% colnames(targets))){
              stop("Your target table includes only one of the columns user.rtmin/user.rtmax. If one is there you also need the other!")
            }

            if(!is.numeric(targets$user.rtmin) | !is.numeric(targets$user.rtmax)){
              stop("Some values in your columns user.rtmin/user.rtmax are not numeric!")
            }

            if(all(c("StartTime.EIC", "EndTime.EIC") %in% colnames(targets)) && nrow(targets[user.rtmin < StartTime.EIC | user.rtmax > EndTime.EIC]) > 0){
              stop("Values for user.rtmin/user.rtmax cannot be lower/higher than values for StartTime.EIC/EndTime.EIC respectivly!")
            }

          }

          if(!is.character(targets$molecule)) {targets$molecule <- as.character(targets$molecule)}
        }

        #Resolution
        if(input$use_envipat_res_list == FALSE){
          if(input$resolution_drop == '------') {stop('Please select used Resolution')}
          res_input <- input$resolution_drop
          resolution_df <- as.data.frame(resolution_list[[res_input]])
        } else {
          if(is.null(res_file()) || length(res_file()) == 0){
            stop('No Resolution/mz file selected')
          }
          resolution_df <- data.table::fread(res_file())
          if(!all(c("m/z", "R") %in% c(colnames(resolution_df)))){stop('Columns in Res/mz file missing')}
          resolution_df <- stats::na.omit(resolution_df[, c("m/z", "R")])
          if(nrow(resolution_df) < 10){stop('Not enough values in Res/mz file. Please provide at least 10 (better more).')}
        }

        withProgress(message = 'Calculation in progress',
                     detail = "calculating isotopologues...", value = 0, {
                       starttime <- Sys.time()

                       ###################################################
                       message("Predicting isotopologues using enviPat")

                       MassTraces <- get_mz_table(targets,
                                                instrumentRes = resolution_df,
                                                RelInt_threshold = input$RelInt_Thresh_input,
                                                stick_method = "intensoid",
                                                screening_adducts = input$adduct_choice
                       )

                       ###################################################
                       incProgress(1/15, detail = "detecting ROIs...")

                       message("Finding regions of interest using xcms")

                       rois <- get_ROIs(files = files,
                                              Target.table = MassTraces,
                                              PrecisionMZtol = input$percision_mz_tol_input,
                                              plan = input$plan_input,
                                              minCentroids = 4,
                                              AccurateMZtol = input$accurate_MZ_tol_input
                       )

                       ################################################
                       incProgress(3/15, detail = "detecting peaks...")
                       message("Evaluating peaks in extracted ion chromatograms")

                       PCal <- find_bench_peaks(files = files,
                                              Grps = grps,
                                              plan = input$plan_input,
                                              CompCol_all = rois,
                                              Integration_baseL_factor = 0.05,
                                              Min.cor.w.M0 = 0.85,
                                              Min.PointsperPeak = input$min_PpP_input,
                                              max.mz.diff_ppm = input$accurate_MZ_tol_input
                       )

                       #####################################################
                       data.table::fwrite(PCal, file = "Peak_list.csv", row.names = FALSE)
                       message(paste0("Benchmark dataset has been exported to ", getwd(), "/Peak_list.csv"))
                       incProgress(15/15, detail = "Finished")


                     }) #End of With Progress

        benchmark_data(list(files = files, targets = targets, PCal = PCal))

        endtime <- Sys.time()


        proc.time <- diff(c(starttime, endtime))
        units(proc.time) <- "mins"


        shinyWidgets::sendSweetAlert(session,
                       title = 'Benchmark generated',
                       text = paste0('Benchmark geneneration has been finished in ', round(proc.time, 0), ' min. The output has been saved to your
                                  working directory as ', getwd(), '/Peak_list.csv. It can be used for reliability assessment of a no-targeted processing run in the section
                                     "Setup NPP assessment"'),
                       type = 'success',
                       closeOnClickOutside = FALSE,
                       showCloseButton = TRUE)

        enable("generate_benchmark")

        updateTabsetPanel(session, "sbmenu",selected = "vBM_p")

      },
      error=function(error_message){
        shinyWidgets::sendSweetAlert(session, title = 'Error', text = geterrmessage(), type = 'error', closeOnClickOutside = FALSE)
        print(error_message)
        enable("generate_benchmark")
        return(NULL)
      })
    })


    #Benchmark Plot Functions

    ###############################
    observeEvent({benchmark_data()}, {
      benchmark_data <- isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        benchmark_data <- benchmark_data$PCal

        output$size_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Benchmark size"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("# of molecules: ", length(unique(benchmark_data$molecule)),
                                            shiny::br(),
                                            "# of features: ", nrow(unique(benchmark_data, by = c("molecule", "adduct", "isoab"))),
                                            shiny::br(),
                                            "# of peaks: ", nrow(benchmark_data)
                                 ))), color = "navy", fill = TRUE)
        })


        output$chrom_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Peak rt-metrics"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Median FWHM [s]: ", round_woe(median(benchmark_data$peaks.FW50M, na.rm = TRUE),0),
                                            shiny::br(),
                                            "Median # of points: ", median(benchmark_data$peaks.PpP, na.rm = TRUE)
                                 ))), color = "blue", fill = TRUE)
        })


        output$mz_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Peak m/z-metrics"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Median mz accuracy [ppm] | abs: ", round_woe(median(benchmark_data$peaks.mz_accuracy_ppm, na.rm = TRUE),1), " | ",
                                            round_woe(median(benchmark_data$peaks.mz_accuracy_abs, na.rm = TRUE),4),
                                            shiny::br(),
                                            "Median mz range [ppm] | abs: ", round_woe(median(benchmark_data$peaks.mz_span_ppm, na.rm = TRUE),1), " | ",
                                            round_woe(median(benchmark_data$peaks.mz_span_abs, na.rm = TRUE),4)
                                 ))), color = "teal", fill = TRUE)
        })

      }
    })
    ################################




    #Benchmark overview plot
    observeEvent({benchmark_data(); input$bench_overview_input_x; input$bench_overview_input_y; input$bench_overview_input_color}, {
      benchmark_data <- isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        output$graph_area_bench_overview <- plotly::renderPlotly(plot_bench_overview(benchmark_data, input$bench_overview_input_x, input$bench_overview_input_y, input$bench_overview_input_color, choice_vector_bench))
      }
    })
    #Benchmark historgramm plot
    observeEvent({benchmark_data(); input$select_bench_histo}, {
      benchmark_data <- isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        output$graph_area_bench_histo <- plotly::renderPlotly(plot_bench_histo(benchmark_data, input$select_bench_histo, choice_vector_bench) %>%
                                                                plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.2)))
      }
    })


    observeEvent({plotly_click_wo_warnings(sc = "bench_scatter")}, {


      bm<-isolate(benchmark_data())
      bm <- bm[["PCal"]]
      event.data <- plotly_click_wo_warnings(sc = "bench_scatter")
      showModal(modalDialog(
        plotly::renderPlotly({
          plot_Peak(bm, IndexNumber = event.data$key)
        })
      ))
    })


    ##################
    ####COMPARISON####
    ##################

    comparison <- observeEvent(input$start_compare, {
      tryCatch({
        shinybusy::show_modal_spinner(spin='scaling-squares', text='Please wait while we make things traceable for you.')
        #####################
        #Import csv files
        #####################
        starttime <- Sys.time()

        if(input$algorithm_input == '---'){
          stop('Please select non-targeted tool used!')
        }
        if(input$use_generated_options == TRUE){
          options_path <- 'generate'
        } else {
          options_path <- options_file()
        }
        if (input$use_generated_benchmark == TRUE) {
          b_o_tables <- check_benchmark_input(benchmark_data()$PCal, options_path, from_csv = FALSE, input$algorithm_input)
          b_table = b_o_tables$b_table
          options_table <- b_o_tables$options_table
        } else {
          b_o_tables <- check_benchmark_input(benchmark_file(), options_path, from_csv = TRUE, input$algorithm_input)
          b_table = b_o_tables$b_table
          options_table <- b_o_tables$options_table
        }

        import_results <- check_nonTargeted_input(ug_files(), g_file(), options_table, input$algorithm_input)
        ug_table <- import_results$ug_table
        g_table <- import_results$g_table
        req(import_results)


        #####################
        #perform comparisons
        #####################
        comparison_ug_g <- compare_peaks(b_table, ug_table, g_table, input$algorithm_input)
        comparison_data(comparison_ug_g)

        endtime <- Sys.time()
        proc.time <- diff(c(starttime, endtime))
        units(proc.time) <- "secs"


        shinybusy::remove_modal_spinner()
        Sys.sleep(0.2) # Otherwise remove modal overwrites error modal
        shinyWidgets::sendSweetAlert(session,
                                     title = 'Assessment complete',
                                     text = paste0('Assessment has been finished in ', round(proc.time, 0), ' seconds. An overview is provided in panel "View NPP assessment"!'),
                                     type = 'success',
                                     closeOnClickOutside = FALSE,
                                     showCloseButton = TRUE)


        updateTabsetPanel(session, "sbmenu",selected = "vNPP_p")

      },
      error=function(error_message){
        shinybusy::remove_modal_spinner()
        Sys.sleep(0.2) # Otherwise remove modal overwirites error modal
        shinyWidgets::sendSweetAlert(session, title = 'Error', text = geterrmessage(), type = 'error', closeOnClickOutside = FALSE)
        print(error_message)
        return(NULL)
      })


    })
    ######
    #Comparison Plot functions
    #####################

    #EIC_plot
    observeEvent(comparison_data(),{
      comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data)){
        comp.dt <-  data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks, comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)
        updateSelectInput(session, 'mol_c', choices = as.character(unique(comp.dt$molecule_b)), selected = as.character(unique(comp.dt$molecule_b)[1]))
      }
    })
    observeEvent(input$mol_c, {
      comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data)){
        comp.dt <-  data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks, comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)
        updateSelectInput(session, 'add_c', choices = unique(comp.dt[molecule_b == input$mol_c]$adduct_b))
      }
    })
    observeEvent({input$mol_c; input$add_c}, {
      comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data)){
        comp.dt <-  data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks, comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)
        updateSelectInput(session, 'ia_c', choices = sort(round_woe(unique(comp.dt[molecule_b == input$mol_c & adduct_b == input$add_c]$isoab_b), 2), decreasing = TRUE))
      }
    })
    observeEvent({comparison_data(); input$mol_c; input$add_c; input$ia_c},{
      comparison_data <- isolate(comparison_data())
      if (!is.null(comparison_data)){
        output$graph_area_4 <- plotly::renderPlotly(plot_comp_peak_overview(comparison_data, input$mol_c, input$add_c, input$ia_c))
      }
    })



    #Scatter_plot
    observeEvent({comparison_data(); input$overview_plot_input_x; input$overview_plot_input_y; input$overview_plot_input_col; input$PP_al_switch_ov}, {
      if(!is.null(comparison_data())){
        output$overview_plot <- plotly::renderPlotly(plot_comp_scatter_plot(comparison_data(),
                                                                            input$overview_plot_input_x,
                                                                            input$overview_plot_input_y,
                                                                            input$overview_plot_input_col,
                                                                            choice_vector_comp,
                                                                            post_alignment = input$PP_al_switch_ov) %>%
                                                       plotly::event_register('plotly_click'))

      }
    })



    suppressWarnings(
    observeEvent(plotly_click_wo_warnings(sc = "scatter"), {

      comparison_data <- comparison_data()
      CE_plot <-  data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks[, Split_peak := FALSE], comparison_data$SplittedMatches_BM_NPPpeaks[present_in_found == FALSE][, Split_peak := TRUE], comparison_data$Unmatched_BM_NPPpeaks[, Split_peak := FALSE]), fill = TRUE)
      CE_plot <- CE_plot[, NPP_status := ifelse(!is.na(peak_area_ug), ifelse(Split_peak == "TRUE", 'Split', 'Found'), 'Not Found')]
      CE_plot <- unique(CE_plot, by = c("molecule_b", "adduct_b", "isoab_b", "sample_name_b"))

      event.data <- suppressWarnings(plotly::event_data("plotly_click", source = "scatter", priority = "event"))

      showModal(modalDialog(
        plotly::renderPlotly({
          plot_Peak(CE_plot, IndexNumber = event.data$key)
        })
      ))
    })
    )

    #R/S Heatmap Plot
    observeEvent({comparison_data(); input$PP_al_switch_hm; input$PP_al_switch_hm_off}, {
      if(!is.null(comparison_data())){
        if(input$PP_al_switch_hm_off == TRUE){
          output$graph_area_1 <- plotly::renderPlotly(plot_comp_missing_value_hm(comparison_data(), post_alignment = input$PP_al_switch_hm))
        } else {
          output$graph_area_1 <- plotly::renderPlotly(plot_comp_missing_value_hm(comparison_data(), post_alignment = input$PP_al_switch_hm, disable_plot = TRUE))

        }
      }
    })

    #Distribution of peaks plot
    observeEvent({comparison_data(); input$graph_select_input; input$PP_al_switch_dist}, {
      if(!is.null(comparison_data())){
        output$graph_area_3 <- plotly::renderPlotly(plot_comp_dist_of_found_peaks(comparison_data(), input$graph_select_input, choice_vector_comp = choice_vector_comp, post_alignment = input$PP_al_switch_dist))
      }
    })
    #Isotopologe prediction error
    observeEvent({comparison_data()}, {
      if(!is.null(comparison_data())){
        output$graph_area_2 <- plotly::renderPlotly(plot_comp_iso_pred_error(comparison_data(), post_alignment = FALSE) %>%
                                                      plotly::event_register('plotly_click'))
      }
    })


    ####

    observeEvent(plotly_click_wo_warnings(sc = "IRbias"), {

      comparison_data <- comparison_data()
      CE_plot <-  data.table::rbindlist(list(comparison_data$Matches_BM_NPPpeaks, comparison_data$Unmatched_BM_NPPpeaks), fill = TRUE)
      event.data <- suppressWarnings(plotly::event_data("plotly_click", source = "IRbias", priority = "event"))
      showModal(modalDialog(
        plotly::renderPlotly({
          plot_IR_peaks(CE_plot, plotly_key = event.data$key)
        })
      ))
    })


    ####





    #Results Text
    observeEvent(comparison_data(), {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){

        result_list <- derive_performance_metrics(comparison_data)

        output$PP_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Peak picking step"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Found peaks: ",
                                            result_list[["Before_alignment"]][["Found_peaks"]][["count"]],
                                            "/",
                                            result_list[["Benchmark"]][["BM_peaks"]],
                                            " (", top_to_x(round_woe(result_list[["Before_alignment"]][["Found_peaks"]][["CI"]][4],1)), " - ", round_woe(result_list[["Before_alignment"]][["Found_peaks"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Missing peaks: ",
                                            result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "/",
                                            result_list[["Before_alignment"]][["Missing_peaks"]][["Systematic"]] + result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "( ", top_to_x(round_woe(result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["CI"]][4],1)) , " - ",
                                            round_woe(result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Split peaks: ",
                                            result_list[["Before_alignment"]][["Split_peaks"]][["count"]],
                                            "/",
                                            result_list[["Before_alignment"]][["Found_peaks"]][["count"]] + result_list[["Before_alignment"]][["Split_peaks"]][["count"]],
                                            "(", top_to_x(round_woe(result_list[["Before_alignment"]][["Split_peaks"]][["CI"]][4], 1)), " - ", round_woe(result_list[["Before_alignment"]][["Split_peaks"]][["CI"]][5], 1),
                                            "%)",
                                            shiny::br(),
                                            "Degenerated IR: ",
                                            result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]],
                                            "/",
                                            result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]] + result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_below20pp"]],
                                            " (",
                                            top_to_x(round_woe(result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][4], 1)), " - ",
                                            round_woe(result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][5], 1),
                                            "%)"
                                 ))), color = "navy", fill = TRUE)
        })


        output$A_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Alignment step"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Errors: ", result_list[["Alignment"]][["Min.Errors"]][["count"]],
                                                   " / ", result_list[["Before_alignment"]][["Found_peaks"]][["count"]],
                                            " (", top_to_x(round_woe(result_list[["Alignment"]][["Min.Errors"]][["CI"]][4],1)), " - ",
                                            round_woe(result_list[["Alignment"]][["Min.Errors"]][["CI"]][5],1), "%)",
                                            shiny::br(),
                                            "BM divergences: ", result_list[["Alignment"]][["BM_divergences"]][["count"]],
                                            " / ", result_list[["Before_alignment"]][["Found_peaks"]][["count"]],
                                            " (", top_to_x(round_woe(result_list[["Alignment"]][["BM_divergences"]][["CI"]][4],1)), " - ",
                                            round_woe(result_list[["Alignment"]][["BM_divergences"]][["CI"]][5],1), "%)",
                                            shiny::br(),
                                            "Lost peaks: ", result_list[["Alignment"]][["Lost_b.A"]][["count"]],
                                            " / ", result_list[["Before_alignment"]][["Found_peaks"]][["count"]],
                                            " (", top_to_x(round_woe(result_list[["Alignment"]][["Lost_b.A"]][["CI"]][4],1)), " - ",
                                            round_woe(result_list[["Alignment"]][["Lost_b.A"]][["CI"]][5],1), "%)"
                                 ))), color = "blue", fill = TRUE)
        })


        output$F_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Post alignment"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Found peaks: ",
                                            result_list[["After_alignment"]][["Found_peaks"]][["count"]],
                                            "/",
                                            result_list[["Benchmark"]][["BM_peaks"]],
                                            " (",
                                            top_to_x(round_woe(result_list[["After_alignment"]][["Found_peaks"]][["CI"]][4],1)), " - ", round_woe(result_list[["After_alignment"]][["Found_peaks"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Missing peaks: ",
                                            result_list[["After_alignment"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "/",
                                            result_list[["After_alignment"]][["Missing_peaks"]][["Systematic"]] + result_list[["After_alignment"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "( ", top_to_x(round_woe(result_list[["After_alignment"]][["Missing_peaks"]][["Random"]][["CI"]][4],1)) , " - ",
                                            round_woe(result_list[["After_alignment"]][["Missing_peaks"]][["Random"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Degenerated IR: ",
                                            result_list[["After_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]],
                                            "/",
                                            result_list[["After_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]] + result_list[["After_alignment"]][["IR_quality"]][["Error_inc_below20pp"]],
                                            " (",
                                            top_to_x(round_woe(result_list[["After_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][4], 1)), " - ",
                                            round_woe(result_list[["After_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][5], 1),

                                            "%)"
                                 ))), color = "teal", fill = TRUE)
        })


        output$sunburst_pp <- plotly::renderPlotly(plot_sunburst_peaks(result_list, comparison_data))
        output$sunburst_pq <- plotly::renderPlotly(plot_sunburst_peakQuality(result_list, comparison_data))
        output$sunburst_al <- plotly::renderPlotly(plot_sunburst_alignment(result_list))

      }
    })
    #Alignment table
    observeEvent(comparison_data(), {
      if(!is.null(comparison_data())){
        output$error_count <- renderTable(comparison_data()$AlignmentErrors_per_moleculeAndAdduct[Min.errors > 0 | Lost_b.A > 0 | BM.div > 0])
      }
    })






    #Alignment error plot
    observeEvent(comparison_data(),{
      if(!is.null(comparison_data())){
        error_molecules <- unique(as.character(comparison_data()$AlignmentErrors_per_moleculeAndAdduct[Min.errors > 0 | Lost_b.A > 0 | BM.div > 0, Molecule]))
        no_error_molecules <- unique(as.character(comparison_data()$AlignmentErrors_per_moleculeAndAdduct[Min.errors == 0 & Lost_b.A == 0 & BM.div == 0, Molecule]))
        choices <- list('Errors:' = as.list(error_molecules), 'No errors:' = as.list(no_error_molecules))
        shinyWidgets::updatePickerInput(session = session, inputId = 'mol_a', choices = choices)
      }
    })
    observeEvent({comparison_data();input$mol_a}, {
      if(!is.null(comparison_data())){
        error_adducts <- as.character(comparison_data()$AlignmentErrors_per_moleculeAndAdduct[(Molecule == input$mol_a) & (Min.errors > 0 | Lost_b.A > 0 | BM.div > 0), Adduct])
        no_error_adducts <- as.character(comparison_data()$AlignmentErrors_per_moleculeAndAdduct[(Molecule == input$mol_a) & (Min.errors == 0 & Lost_b.A == 0 & BM.div == 0), Adduct])
        choices <- list('Errors:' = as.list(error_adducts), 'No errors:' = as.list(no_error_adducts))
        shinyWidgets::updatePickerInput(session = session, inputId = 'add_a', choices = choices)
      }
    })
    observeEvent(comparison_data(), {
      if(!is.null(comparison_data())){
        output$graph_hm_split <- plotly::renderPlotly(Alignment_error_plot(comparison_data(), mol = input$mol_a, add = input$add_a))
      }
    })


    output$report <- downloadHandler(

      filename = "report.html",
      content = function(file) {

        withProgress(message = 'Report is generated',
                     detail = "This takes some seconds...", value = 0, {
                       incProgress(4/15, detail = "This takes some seconds...")

        tempReport <- file.path(system.file("md","mzRAPP_report_template.Rmd", package = "mzRAPP", mustWork = TRUE))
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(comp_d = comparison_data())

        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        incProgress(15/15, detail = "Done.")
        })
      }
    )
    options(warn = 0)
  }



  shiny::shinyApp(ui, server)

}
