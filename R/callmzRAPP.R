#' callmzRAPP
#'
#' @return shiny app
#' @export
#'
#'

callmzRAPP <- function(){


  #load resolution list from enviPat
  rm(list = ls(), envir = environment())
  if(!("resolution_list" %in% ls())){data("resolution_list", envir = environment(), package = "enviPat")}


  #setup choice vectors
  choice_vector_bench <- c(
    'Retention time [s]' = 'peaks.rt_raw',
    'Points per peak' = 'peaks.PpP',
    'Mean scan to scan time [s]' = 'peaks.data_rate',
    'Sharpness' = 'peaks.sharpness',
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
    'Sharpness' = 'peaks.sharpness_b',
    'FW25M' = 'peaks.FW25M_b',
    'FW50M'= 'peaks.FW50M_b',
    'FW75M' = 'peaks.FW75M_b',
    'Zigzag index' = 'peaks.zigZag_IDX_b',
    'Height' = 'peak_height_b',
    'Area' = 'peak_area_b',
    'mz measured' = 'mz_b',
    'mz accuracy abs' = 'peaks.mz_accuracy_abs_b',
    'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm_b',
    'mz range (abs)' = 'peaks.mz_span_abs_b',
    'mz range [ppm]' = 'peaks.mz_span_ppm_b',
    'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0_b',
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
      extendShinyjs(text = 'shinyjs.scrolltop = function() {window.scrollTo(0, 0)};'), #always start from top of panel
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
                shiny::tags$div(
                  class = "rmd-class",
                  shiny::includeHTML(system.file("md","README.html", package = "mzRAPP", mustWork = TRUE))
                )
        ),
        shinydashboard::tabItem(tabName = "gBM_p",

                shiny::fluidRow(
                  shiny::column(5,
                         shiny::strong("1. Select necessary files", style = "font-size:30px"),
                         shiny::p("(please note that dialog boxes to select files might not be directly visible as they tend to open behind the R console. Try to minimize your windows if it dialog boxes do not appear after clicking.)"),
                         shiny::a("Click here for information on how to prepare csv files and set paramters.", onclick = "openTab('Readme')", href="#sBM_readme")

                  )
                ),

                shiny::fluidRow(
                  shiny::column(6,
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
                         ),
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
                                   shiny::column(2, shiny::actionButton(inputId = 'custom_res_mz',
                                                          label = 'Select Res vs mz table',
                                                          width = '100%'))
                  )
                ),

                shiny::fluidRow(
                  shiny::conditionalPanel(condition = "input.use_envipat_res_list",
                                   shiny::column(2, shiny::verbatimTextOutput(outputId = 'custom_res_mz', placeholder = TRUE)
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

                shiny::fluidRow(
                  shiny::column(2,shiny::actionButton('generate_benchmark', 'Generate benchmark', style = "background-color: #d2f8fa"))
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
                                  shinyWidgets::dropdownButton(shiny::br(""),
                                                 tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                                 circle = TRUE,
                                                 width = 600,
                                                 status = 'info',
                                                 icon = shiny::icon('question-circle'),
                                                 size = 'sm'
                                  )
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
                           shiny::column(1,
                                  shinyWidgets::dropdownButton(
                                    shiny::br('1'),
                                    tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                    circle = TRUE,
                                    width = 600,
                                    status = 'info',
                                    icon = shiny::icon('question-circle'),
                                    size = 'sm'
                                  )
                           )
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

                    12, shiny::selectInput('algorithm_input', 'Non-targeted tool used', c('---', 'XCMS', 'msDial', 'CompoundDiscoverer', 'mzMine', 'El-MAVEN', 'OpenMS'), selected = '---')
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    12, shiny::strong('2. Select unaligned and aligned files', style = "font-size:30px"), shiny::br(),
                    shiny::a("Click here for information on how to get unaligned and aligned files from different NPP tools.",
                      onclick = "openTab('Readme')",
                      href="#sNPP_readme"),
                    shiny::br()
                  )
                ),

                shiny::fluidRow(
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
                    12, shiny::strong('3. Select benchmark and/or options file', style = "font-size:30px"), shiny::br(), shiny::br()
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
                                         shiny::actionButton(inputId = 'benchmark_upload',
                                                      label = 'Select benchmark file',
                                                      width = '190px')
                        )
                    ),
                    shiny::div(style = "width: 20px;"),
                    shiny::div(style = "width:190px",
                        shiny::conditionalPanel(condition = "!input.use_generated_options",
                                         shiny::actionButton(inputId = 'options_upload',
                                                      label = 'Select options files',
                                                      width = '190px')
                        )
                    )
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

                shiny::fluidRow(
                  shiny::column(12,
                         style = "display: inline-flex;",
                         shiny::actionButton('start_compare', 'Start assessment', style =
                                        'height: 34px; allign: center; background-color: #d2f8fa')
                  )
                )

        ),

        shinydashboard::tabItem(tabName = "vNPP_p",
                shiny::h1('NPP assessment results'),
                shiny::h4(paste("Key performance measures are given for different stages of the NPP workflow. Empirical confindence intervals (alpha = 0.95) of calculated percentages are given in brackets",
                         "(estimated via bootstrapping with R = 1000). For details on how individual performance measures are calculated please check the original mzRAPP puplication or readme.")),
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
         #       shiny::fluidRow(
         #         # A static infoBox
         #         #shinydashboard::infoBox("New Orders", 10 * 2, icon = shiny::icon("credit-card")),
         #         # Dynamic infoBoxes
         #         shiny::column(4,
         #                plotly::plotlyOutput('sunburst_pp') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)
         #         ),
         #         shiny::column(4,
         #                plotly::plotlyOutput('sunburst_al') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)
         #         ),
         #         shiny::column(4,
         #                plotly::plotlyOutput('sunburst_ft') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)
         #         )
         #       ),
                shiny::h4(paste("For more details please check the interactive plots below.")),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),
                shiny::column(10, offset = 1, shiny::h2("Distribution of found/not found peaks")),
                shiny::column(9, offset = 1,
                              shiny::h4("In the following interactive scatter plot and histogram the distribution of", shiny::tags$span(style="color:blue", "found") ,"/", shiny::tags$span(style="color:red", "not found"), "peaks can be investigated as a function of different benchmark peak variables.",
                          "Points in the scatter plot can be clicked to inspect individual peaks."),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Matching_peaks")
                ),

                shiny::fluidRow(
                  shiny::column(6, offset = 1,
                         shiny::fluidRow(
                           shiny::column(1,
                                  shinyWidgets::dropdownButton(shiny::br(""),
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
                  ),
                  shiny::column(4,

                         shiny::fluidRow(
                           shiny::column(1,
                                  shinyWidgets::dropdownButton(
                                    shiny::br('1'),
                                    tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                    circle = TRUE,
                                    width = 600,
                                    status = 'info',
                                    icon = shiny::icon('question-circle'),
                                    size = 'sm'
                                  )
                           ),
                           shiny::column(11,
                                  shinyWidgets::prettySwitch(inputId = 'PP_al_switch_dist',
                                               label = 'after PeakPicking | Alignment',
                                               slim = TRUE,
                                               value = FALSE
                                  )
                           )
                         ),

                         shiny::fluidRow(shiny::column(12,plotly::plotlyOutput('graph_area_3') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4))),

                         shiny::fluidRow(
                           shiny::column(8,
                                  shiny::selectInput('graph_select_input',
                                              'x-axis',
                                              choice_vector_comp
                                  )
                           )
                         )
                  )
                ),

                shiny::br(),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),

                shiny::column(10, offset = 1, shiny::h2("Quality of reported NPP peak abundances")),
                shiny::column(8, offset = 2,
                       shiny::h4(paste0("The quality of reported peak abundances is important in order to determine molecular compositions via isotopologue ratios or compare abundances between ",
                                 "samples. Since the former can be predicted when the molecular formula is known it can be used to estimate the quality of peak abundances reported by NPP.",
                                 "In order to inspect peaks contributing to a ratio click on the plot edges.")),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Peak_quality")
                ),
                shiny::br(),
                shiny::column(8, offset = 2,

                       shiny::fluidRow(
                         shiny::column(1,
                                shinyWidgets::dropdownButton('',
                                               tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                               circle = TRUE,
                                               status = 'info',
                                               icon = shiny::icon('question-circle'),
                                               size = 'sm',
                                               width = 600
                                )
                         )
                       ),

                       shiny::fluidRow(shiny::column(12,plotly::plotlyOutput('graph_area_2') %>% shinycssloaders::withSpinner(color="#0dc5c1", type = 4)))
                ),

                shiny::br(),
                shiny::br(),

                shiny::fluidRow(shiny::column(10, offset = 1, shiny::tags$hr(style="border-color: darkgray;"))),

                shiny::br(),
                shiny::column(10, offset = 1, shiny::h2("The nature of missing values")),

                shiny::column(8, offset = 2,
                       shiny::h4(paste0("The nature of missing values is of outmost importance for the choice of a fitting missing value imputation method. Since features defined in the benchmark could ",
                                 "be aligned incorrectly only benchmark peaks for which the alignment was confirmed via NPP are considered. Others are labeled as 'not confirmable' (NC).")),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Missing_values")
                ),
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(8, offset = 2,
                         shiny::fluidRow(
                           shiny::column(1,
                                  shinyWidgets::dropdownButton('',
                                                 tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                                 circle = TRUE,
                                                 status = 'info',
                                                 icon = shiny::icon('question-circle'),
                                                 size = 'sm',
                                                 width = 600
                                  ),
                           ),
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
                shiny::column(10, offset = 1, shiny::h2("Errors in alignment process")),
                shiny::column(10, offset = 1,
                       shiny::h4(paste0("The alignment process is responsible for assembling peaks of different samples into features. mzRAPP is counting the minimum number ",
                                 "of errors by checking whether those assignments are performed symmetrically over different isotopologues of the same compound. This way ",
                                 "alignment errors in the benchmark do not affect this count.")),
                       shiny::a("Click here for more information.", onclick = "openTab('Readme')", href="#Alignment_counting")
                ),
                shiny::br(),

                shiny::fluidRow(
                  shiny::column(4),
                  shiny::column(1, shinyWidgets::dropdownButton('',
                                           tooltip = shinyWidgets::tooltipOptions(title = 'Click for description'),
                                           circle = TRUE,
                                           status = 'info',
                                           icon = shiny::icon('question-circle'),
                                           size = 'sm',
                                           width = 600
                  ))

                ),

                shiny::fluidRow(
                  shiny::column(3, offset = 1, shiny::tableOutput('error_count'), style="overflow-y:scroll; height:464px"),
                  shiny::column(7,

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


    ##File Filters for choice cialogues
    mzML_filter <- matrix(c('mzML Files (*.mzML)', '*.mzML'), nrow = 1, ncol = 2)
    csv_filter <- matrix(c('Files (*.csv, *.txt, *.Rda)', '*.csv;*.txt;*.Rda'), nrow = 1, ncol = 2)

    #File input reactives
    #Benchmark
    mzML_files <- reactive({
      if (input$mzML_upload == 0){return(NULL)}
      else {
        files <- tcltk::tk_choose.files(caption = 'Select .mzML files', multi = TRUE, filters = mzML_filter)
        if (length(files) > 1){
          output$mzML_upload_files <- renderText(paste0(length(files), ' Files selected'))
        } else {
          output$mzML_upload_files <- renderText(paste0(basename(files)))
        }
        return(files)
      }
    })

    grps_file <- reactive({
      if(input$grps_upload[1] == 0){return(NULL)}
      else {
        file <- paste(tcltk::tk_choose.files(caption = 'Select sample-group file', multi = FALSE, filters = csv_filter), collapse = " ")
        output$grps_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })

    coi_file <- reactive({
      if(input$coi_upload == 0){return(NULL)}
      else {
        file <- paste(tcltk::tk_choose.files(caption = 'Select target file', multi = FALSE, filters = csv_filter), collapse = " ")
        output$coi_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })

    res_file <- reactive({
      if(input$custom_res_mz == 0){return(NULL)}
      else {
        file <- paste(tcltk::tk_choose.files(caption = 'Select Res/mz file', multi = FALSE, filters = csv_filter),collapse = " ")
        output$custom_res_mz <- renderText(paste0(basename(file)))
        return(file)
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
    ug_files <- reactive({
      if (input$ug_upload == 0){return(NULL)}
      else {
        files <- tcltk::tk_choose.files(caption = 'Select unaligned file(s)', multi = TRUE, filters = csv_filter)
        if (length(files) > 1){
          output$ug_upload_files <- renderText(paste0(length(files), ' Files selected'))
        } else {
          output$ug_upload_files <- renderText(paste0(basename(files)))
        }
        return(files)
      }
    })
    g_file <- reactive({
      if (input$g_upload == 0){return(NULL)}
      else {
        file <- paste(tcltk::tk_choose.files(caption = 'Select aligned file', multi = FALSE, filters = csv_filter), collapse = " ")
        output$g_upload_file <- renderText(paste0(basename(file)))

        output$g_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })
    benchmark_file <- reactive({
      if (input$benchmark_upload == 0){return(NULL)}
      else {
        file <- paste(tcltk::tk_choose.files(caption = 'Select benchmark file', multi = FALSE, filters = csv_filter), collapse = " ")
        output$benchmark_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })
    options_file <- reactive({
      if (input$options_upload == 0){return(NULL)}
      else {
        file <- paste(tcltk::tk_choose.files(caption = 'Select options file', multi = FALSE, filters = csv_filter), collapse = " ")
        output$options_upload_file <- renderText(paste0(basename(file)))
        return(file)
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
          grps <- fread(grps_file())

          missing_cols <- setdiff(c("sample_name", "sample_group"), colnames(grps))
          if(length(missing_cols) > 0){stop(paste0("Sample_group table is lacking shiny::columns: ", paste(missing_cols, collapse = ", ")))}

          if(nrow(grps[is.na(sample_name) | is.na(sample_group)]) > 0){
            stop("Values in sample_group table must not be empty! If you have only one group please add the same sample_group value for each sample!")
          }

        }

        #coi
        if(is.null(coi_file()) || length(coi_file()) == 0){
          stop('No target file selected')
        } else {
          targets <- fread(coi_file())

          missing_cols <- setdiff(c("molecule", "SumForm_c", "adduct_c", "StartTime.EIC", "EndTime.EIC", "main_adduct"), colnames(targets))
          if(length(missing_cols) > 0){stop(paste0("Target.table is lacking shiny::columns: ", paste0(missing_cols, collapse = ", ")))}


          if(!is.numeric(targets$StartTime.EIC) | !is.numeric(targets$EndTime.EIC)){
            stop("Some values in your shiny::columns StartTime.EIC/EndTime.EIC are not numeric!")

            if(nrow(targets[StartTime.EIC < 0 | EndTime.EIC < 0 | EndTime.EIC < StartTime.EIC]) > 0){
              stop("Values for StartTime.EIC and EndTime.EIC cannot be lower than 0. Also values for StartTime.EIC have to be lower than those for EndTime.EIC!")
            }

          }

          if("user.rtmin" %in% colnames(targets) | "user.rtmax" %in% colnames(targets)){
            if(!all(c("user.rtmin", "user.rtmax") %in% colnames(targets))){
              stop("Your target table includes only one of the shiny::columns user.rtmin/user.rtmax. If one is there you also need the other!")
            }

            if(!is.numeric(targets$user.rtmin) | !is.numeric(targets$user.rtmax)){
              stop("Some values in your shiny::columns user.rtmin/user.rtmax are not numeric!")
            }

            if(nrow(targets[user.rtmin < StartTime.EIC | user.rtmax > EndTime.EIC]) > 0){
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
          resolution_df <- fread(res_file())
          if(!all(c("m/z", "R") %in% c(colnames(resolution_df)))){stop('Columns in Res/mz file missing')}
          resolution_df <- na.omit(resolution_df[, c("m/z", "R")])
          if(nrow(resolution_df) < 10){stop('Not enough values in Res/mz file. Please provide at least 10 (better more).')}
        }

        withProgress(message = 'Calculation in progress',
                     detail = "calculating isotopologues...", value = 0, {
                       starttime <- Sys.time()

                       ###################################################
                       message("Predicting isotopologues using enviPat")

                       MassTraces <- getMZtable(targets,
                                                instrumentRes = resolution_df,
                                                RelInt_threshold = input$RelInt_Thresh_input,
                                                stick_method = "intensoid"
                       )

                       ###################################################
                       incProgress(1/15, detail = "detecting ROIs...")

                       message("Finding regions of interest using xcms")

                       rois <- getROIsForEICs(files = files,
                                              Target.table = MassTraces,
                                              PrecisionMZtol = input$percision_mz_tol_input,
                                              plan = input$plan_input,
                                              minCentroids = 4,
                                              AccurateMZtol = input$accurate_MZ_tol_input
                       )

                       ################################################
                       incProgress(3/15, detail = "detecting peaks...")
                       message("Evaluating peaks in extracted ion chromatograms")

                       PCal <- findBenchPeaks(files = files,
                                              Grps = grps,
                                              plan = input$plan_input,
                                              CompCol_all = rois,
                                              Min.PointsperPeak = input$min_PpP_input,
                                              max.mz.diff_ppm = input$accurate_MZ_tol_input
                       )

                  #     if(any(duplicated(PCbp[, c("molecule", "isoab", "adduct", "FileName")]))){
                  #       warning("Duplicated peaks present")
                  #       fwrite(PCbp, "pcbp.csv")
                  #       #####################################################
                  #       incProgress(10/15, detail = "aligning peaks over samples...")
                  #       print('Start peak alignment')
#
##                         PCal <- align_PC(PCbp[Iso_count > 1],
 #                                         add = "main_adduct",
 #                                         plan = input$plan_input,
 #                                         pick_best = "rt_match"
 #                        )
#
#                       } else {
                        # PCal <- PCbp
#                       }
                       #####################################################
                       fwrite(PCal, file = "Peak_list.csv", row.names = FALSE)
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
                                  working directory as ', getwd(), '/Peak_list.csv. It can be inspected by importing it to Skyline with the instructions
                                     printed in the R console or directly used for reliability assessment of a no-targeted processing run in the section
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

    #Benchmark heatmap plot
    #observeEvent({benchmark_data()}, {
    #  benchmark_data <- isolate(benchmark_data())
    #  if(!is.null(benchmark_data)){
    #    plot_and_text <- plot_bench_heatmap(benchmark_data)
    #    output$graph_area_bench_hm <- plotly::renderPlotly(plot_and_text$p)
    #    output$results_text_b <- renderText(plot_and_text$t)
    #  }
    #})


    #Benchmark peak overview plot
    #observeEvent(benchmark_data(),{
    #  benchmark_data<-isolate(benchmark_data())
    #  if(!is.null(benchmark_data)){
    #    benchmark_data <- benchmark_data$PCal
    #    updateSelectInput(session, 'mol', choices = as.character(unique(benchmark_data$molecule)), selected = as.character(unique(benchmark_data$molecule)[1]))
    #  }
    #})
    #observeEvent(input$mol, {
    #  benchmark_data<-isolate(benchmark_data())
    #  if(!is.null(benchmark_data)){
    #    benchmark_data <- benchmark_data$PCal
    #    updateSelectInput(session, 'add', choices = unique(benchmark_data[molecule == input$mol]$adduct))
    #  }
    #})

    #observeEvent({input$mol; input$add}, {
    #  benchmark_data<-isolate(benchmark_data())
    #  if(!is.null(benchmark_data)){
    #    benchmark_data <- benchmark_data$PCal
    #    updateSelectInput(session, 'ia', choices = sort(round_woe(unique(benchmark_data[molecule == input$mol & adduct == input$add]$isoab), 2), decreasing = TRUE))
    #  }
    #})

    #observeEvent({benchmark_data(); input$mol; input$add; input$ia}, {
    #  benchmark_data<-isolate(benchmark_data())
    #  if(!is.null(benchmark_data)){
    #    output$graph_area_bench_peak_overview <- plotly::renderPlotly(plot_bench_peak_overview(benchmark_data, input$mol, input$add, input$ia)%>%
    #                                                                    plotly::event_register('plotly_click'))
    #  }
    #})


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
          b_o_tables <- import_benchmark(benchmark_data()$PCal, options_path, from_csv = FALSE, input$algorithm_input)
          b_table = b_o_tables$b_table
          options_table <- b_o_tables$options_table
        } else {
          b_o_tables <- import_benchmark(benchmark_file(), options_path, from_csv = TRUE, input$algorithm_input)
          b_table = b_o_tables$b_table
          options_table <- b_o_tables$options_table
        }

        import_results <- pick_algorithm(ug_files(), g_file(), options_table, input$algorithm_input)
        ug_table <- import_results$ug_table
        g_table <- import_results$g_table
        req(import_results)


        #####################
        #perform comparisons
        #####################
        comparison_ug_g <- compare_peaks(b_table, ug_table, g_table, input$algorithm_input)
        comparison_data(comparison_ug_g)

        #comp_data <<- comparison_ug_g

        endtime <- Sys.time()
        proc.time <- diff(c(starttime, endtime))
        units(proc.time) <- "secs"


        shinybusy::remove_modal_spinner()
        Sys.sleep(0.2) # Otherwise remove modal overwirites error modal
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
        comp.dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)
        updateSelectInput(session, 'mol_c', choices = as.character(unique(comp.dt$molecule_b)), selected = as.character(unique(comp.dt$molecule_b)[1]))
      }
    })
    observeEvent(input$mol_c, {
      comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data)){
        comp.dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)
        updateSelectInput(session, 'add_c', choices = unique(comp.dt[molecule_b == input$mol_c]$adduct_b))
      }
    })
    observeEvent({input$mol_c; input$add_c}, {
      comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data)){
        comp.dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)
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
      #comparison_data <- isolate(comparison_data())
      #print("start")
      if(!is.null(comparison_data())){
        #print("in")
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

#      delay(expr =({
      #Sys.time((0.1))
#        options(warn = storeWarn)
#      }) ,ms = 100)

      comparison_data <- comparison_data()
      CE_plot <-  rbindlist(list(comparison_data$c_table[, Split_peak := FALSE], comparison_data$split_table[present_in_found == FALSE][, Split_peak := TRUE], comparison_data$nf_b_table[, Split_peak := FALSE]), fill = TRUE)
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

    #Ditsribution of peaks plot
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
      CE_plot <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)
      event.data <- suppressWarnings(plotly::event_data("plotly_click", source = "IRbias", priority = "event"))
      #print(event.data)
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

        result_list <- generate_results_text(comparison_data)

        output$PP_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Peak picking step"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Found peaks: ",
                                            result_list[["Before_alignment"]][["Found_peaks"]][["count"]],
                                            "/",
                                            result_list[["Benchmark"]][["BM_peaks"]],
                                            " (", round_woe(result_list[["Before_alignment"]][["Found_peaks"]][["CI"]][4],1), " - ", round_woe(result_list[["Before_alignment"]][["Found_peaks"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Missing peaks (high): ",
                                            result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "/",
                                            result_list[["Before_alignment"]][["Missing_peaks"]][["Systematic"]] + result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "( ", round_woe(result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["CI"]][4],1) , " - ",
                                            round_woe(result_list[["Before_alignment"]][["Missing_peaks"]][["Random"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Split peaks: ",
                                            result_list[["Before_alignment"]][["Split_peaks"]][["count"]],
                                            "/",
                                            result_list[["Benchmark"]][["BM_peaks"]],
                                            "(", round_woe(result_list[["Before_alignment"]][["Split_peaks"]][["CI"]][4], 1), " - ", round_woe(result_list[["Before_alignment"]][["Split_peaks"]][["CI"]][5], 1),
                                            "%)",
                                            shiny::br(),
                                            "Degenerated IR: ",
                                            result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]],
                                            "/",
                                            result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]] + result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_below20pp"]],
                                            " (",
                                            round_woe(result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][4], 1), " - ",
                                            round_woe(result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][5], 1),
                                            #round_woe(result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]] /
                                            # (result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_above20pp"]] + result_list[["Before_alignment"]][["IR_quality"]][["Error_inc_below20pp"]]) *100,1),
                                            "%)"
                                 ))), color = "navy", fill = TRUE)
        })


        output$A_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Alignment step"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Min. errors: ", result_list[["Alignmnet"]][["Min.Errors"]][["count"]],
                                            " (", round_woe(result_list[["Alignmnet"]][["Min.Errors"]][["CI"]][4],1), " - ",
                                            round_woe(result_list[["Alignmnet"]][["Min.Errors"]][["CI"]][5],1), "%)",
                                            shiny::br(),
                                            "BM divergences: ", result_list[["Alignmnet"]][["BM_divergences"]][["count"]],
                                            " (", round_woe(result_list[["Alignmnet"]][["BM_divergences"]][["CI"]][4],1), " - ",
                                            round_woe(result_list[["Alignmnet"]][["BM_divergences"]][["CI"]][5],1), "%)",
                                            shiny::br(),
                                            "Lost peaks: ", result_list[["Alignmnet"]][["Lost_b.A"]][["count"]],
                                            " (", round_woe(result_list[["Alignmnet"]][["Lost_b.A"]][["CI"]][4],1), " - ",
                                            round_woe(result_list[["Alignmnet"]][["Lost_b.A"]][["CI"]][5],1), "%)"
                                 ))), color = "blue", fill = TRUE)
        })


        output$F_info <- shinydashboard::renderInfoBox({
          shinydashboard::infoBox(shiny::tags$p(style = "font-weight: bold; font-size: 110%","Post alignment"),
                  value = shiny::tags$p(style = "font-weight: normal; font-size: 100%;",
                                 shiny::HTML(paste("Found peaks: ",
                                            result_list[["After_alignmnet"]][["Found_peaks"]][["count"]],
                                            "/",
                                            result_list[["Benchmark"]][["BM_peaks"]],
                                            " (",
                                            round_woe(result_list[["After_alignmnet"]][["Found_peaks"]][["CI"]][4],1), " - ", round_woe(result_list[["After_alignmnet"]][["Found_peaks"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Missing peaks (high): ",
                                            result_list[["After_alignmnet"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "/",
                                            result_list[["After_alignmnet"]][["Missing_peaks"]][["Systematic"]] + result_list[["After_alignmnet"]][["Missing_peaks"]][["Random"]][["count"]],
                                            "( ", round_woe(result_list[["After_alignmnet"]][["Missing_peaks"]][["Random"]][["CI"]][4],1) , " - ",
                                            round_woe(result_list[["After_alignmnet"]][["Missing_peaks"]][["Random"]][["CI"]][5],1),
                                            "%)",
                                            shiny::br(),
                                            "Degenerated IR: ",
                                            result_list[["After_alignmnet"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]],
                                            "/",
                                            result_list[["After_alignmnet"]][["IR_quality"]][["Error_inc_above20pp"]][["count"]] + result_list[["After_alignmnet"]][["IR_quality"]][["Error_inc_below20pp"]],
                                            " (",
                                            round_woe(result_list[["After_alignmnet"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][4], 1), " - ",
                                            round_woe(result_list[["After_alignmnet"]][["IR_quality"]][["Error_inc_above20pp"]][["CI"]][5], 1),

                                            "%)"
                                 ))), color = "teal", fill = TRUE)
        })


        #output$sunburst_pp <- plotly::renderPlotly(plot_sunburst_peaks(result_list, comparison_data))
        #output$sunburst_al <- plotly::renderPlotly(plot_sunburst_peakQuality(result_list, comparison_data))
        #output$sunburst_ft <- plotly::renderPlotly(plot_sunburst_alignment(result_list))

      }
    })
    #Alignment table
    observeEvent(comparison_data(), {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$error_count <- renderTable(comparison_data()$ali_error_table[Min.errors > 0 | Lost_b.A > 0 | BM.div > 0])
      }
      #delay(expr =({
      #  options(warn = storeWarn)
      #}) ,ms = 100)
    })






    #Alignment error plot
    observeEvent(comparison_data(),{
      if(!is.null(comparison_data())){
        error_molecules <- unique(as.character(comparison_data()$ali_error_table[Min.errors > 0 | Lost_b.A > 0 | BM.div > 0, Molecule]))
        no_error_molecules <- unique(as.character(comparison_data()$ali_error_table[Min.errors == 0 & Lost_b.A == 0 | BM.div == 0, Molecule]))
        choices <- list('Errors:' = as.list(error_molecules), 'No errors:' = as.list(no_error_molecules))
        shinyWidgets::updatePickerInput(session = session, inputId = 'mol_a', choices = choices)
      }
    })
    observeEvent({comparison_data();input$mol_a}, {
      if(!is.null(comparison_data())){
        error_adducts <- as.character(comparison_data()$ali_error_table[(Molecule == input$mol_a) & (Min.errors > 0 | Lost_b.A > 0 | BM.div > 0), Adduct])
        no_error_adducts <- as.character(comparison_data()$ali_error_table[(Molecule == input$mol_a) & (Min.errors == 0 & Lost_b.A == 0 | BM.div == 0), Adduct])
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
