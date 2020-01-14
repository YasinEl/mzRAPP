#' callmzRAPP
#'
#' @return
#' @export
#'
#' @import enviPat shiny shinyjs data.table
#'
#' @examples
callmzRAPP <- function(){

  jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

  #library(shiny)
  #library(shinyWidgets)
  #library(shinyjs)
  #library(V8)
  #library(data.table)
  #library(DT)
  #library(tools)
  ##library(tictoc)
  #library(shinyFiles)
  #library(plotly)
  #library(dplyr)
  #library(enviPat)

  options(shiny.reactlog = TRUE)
  options(shiny.trace = F)
  options(shiny.maxRequestSize = 100000 * 1024 ^ 2)
  options(spinner.type = 4)

  data('resolution_list')

  ui <- tagList(shinyjs::useShinyjs(),
                shinyjs::extendShinyjs(text = jscode),
                shinyjs::inlineCSS(css),
                navbarPage(
    'mzRAPP',
    id = 'main_panel',
    tabPanel(
      'Generate benchmark',
      value = 'generate_benchmark_tab',
      mainPanel(
        fluidRow(
          column(4,
                 strong(
                   "1. Select necessary files:", style = "font-size:30px"
                 ),
                 br(),
                 p("(and choose resolution used)")
          )

        ),
        fluidRow(
          column(
            6,
            style = "display: inline-flex;",
            shinyFilesButton(
              'mzML_upload',
              'Select mzML files',
              title = 'Select mzML files',
              multiple = TRUE,
              style = "width:190px"
            ),
            div(style = "width: 20px;"),
            shinyFilesButton(
              'grps_upload',
              'Select sample-group file',
              title = 'Select sample-group file',
              multiple = TRUE,
              style = "width:190px"
            ),
            div(style = "width: 20px;"),
            shinyFilesButton(
              'coi_upload',
              'Select target file',
              title = 'Select target file',
              multiple = TRUE,
              style = "width:190px"
            )
          )

        ),
        fluidRow(
          column(4,
                 br()
          )
        ),

        fluidRow(
          column(
            5,
            style = "display: inline-flex;",

            #div(style = "width: 20px;"),
            selectInput(
              'resolution_drop',
              'Select instrument & resolution',
              names(resolution_list),
              selected = 'OTFusion,QExactiveHF_120000@200',
              width = "100%"
            )
          )

        ),
        fluidRow(
          column(6,
                 strong(
                   "2. Set parameters:", style = "font-size:30px"
                 ),
                 br(),
                 p("(Processing plan: multiprocess for parallel processing on
               Windows computers; sequential for serial processing; for other
                 options see help of future::plan)")
          )
        ),
        fluidRow(
          column(3,
            style = "display: inline-flex;",
            numericInput('RelInt_Thresh_input', 'Lowest iso. to be considered [%]', 0.05, step = 0.01, max = 100)
          ),
          column(3,
            style = "display: inline-flex;",
            numericInput('percision_mz_tol_input', 'mz precision [ppm]', 5, step = 0.1)
          )
        ),
        fluidRow(
          column(3,
            style = "display: inline-flex;",
            numericInput('min_centroids_input', 'Min. # of consecutive scans per ROI', 6, step = 1, min = 3)
          ),
          column(3,
            style = "display: inline-flex;",
            numericInput('accurate_MZ_tol_input', 'mz accuracy [ppm]', 5, step = 0.1)
          )
        ),
        fluidRow(
          column(3,
            style = "display: inline-flex;",
            numericInput('min_PpP_input', 'Min. # of scans per peak', 10, step = 1, min = 5)
          ),
          column(3,
            style = "display: inline-flex;",
            selectInput('plan_input', 'Processing plan', c('multiprocess', 'sequential', 'multicore', 'cluster', 'remote'))
          )
        ),
        fluidRow(
          column(6,
                 strong(
                   "3. Start benchmark generation:", style = "font-size:30px"
                 ),
                 br(),
                 p("(depending on the number of files and
                 compounds this can take some time (minutes to hours))")
          )
        ),
        fluidRow(
          column(
            6,
            style = "display: inline-flex;",
            actionButton('generate_benchmark', 'Generate benchmark', style = "background-color: #d2f8fa"),
            div(style = "width: 20px;"),
            textOutput('text')
          )
        ),
        tableOutput('debug_table')


      )

    ),
    tabPanel(
      'Benchmark overview',
      value = 'benchmark_results',
      mainPanel(
        width = "100%",
        fluidRow(column(12,
                        verbatimTextOutput('results_text_b')
        )),
        fluidRow(
          #column(6,
          #  style = 'padding:0px;margin:0px;display: inline-flex;',
          #  dropdownButton(
          #    br(
          ##      paste0("Here the chromatographic peak area of each detected benchmark peak (excluding the most abundant isotopologue of each molecular formula)
           #   is plotted against it", "'", "s predicted area. The prediction is made by multiplying the area of the most abundant isotopologue with the relative
           #   isotopic abundace of the peak to be predicted. Therefore each point in the plot corresponds to one peak in one sample.")
           #   ),
           #   tooltip = tooltipOptions(title = 'Click for description'),
           #   circle = TRUE,
           ##   status = 'info',
            #  icon = icon('question-circle'),
          #    size = 'sm'
          #  ),
          #  plotlyOutput('graph_area_bench_1', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
          #),

          #column(
          #  6,
          #  #offset = 7,
          #  style = 'padding:0px;margin:0px;display: inline-flex;',
          #  dropdownButton(
          #    br(
          #      'Same points as in the plot to the left. However, here the error of the prediction is plotted instead of the measured area itselfe.'),
          #    tooltip = tooltipOptions(title = 'Click for description'),
          #    circle = TRUE,
          #    status = 'info',
          ##    icon = icon('question-circle'),
           #   size = 'sm'
          #  ),
          #  plotlyOutput('graph_area_bench_2', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
          #),

          column(
            7,
            style = 'padding:0px;margin:0px;display: inline-flex;',
              dropdownButton(
                br(
                  paste0("Here the chromatographic peak area of each detected benchmark peak (excluding the most abundant isotopologue of each molecular formula)
               is plotted against its predicted area. The prediction is made by multiplying the area of the most abundant isotopologue with the relative
               isotopic abundace of the peak to be predicted. Therefore each point in the plot corresponds to one peak in one sample. Other peak variables
                can be plotted using the dropdown menues below the plot.")
               ),
               tooltip = tooltipOptions(title = 'Click for description'),
               circle = TRUE,
               width = 600,
               status = 'info',
              icon = icon('question-circle'),
                size = 'sm'
              ),            plotlyOutput('bench_plotxy', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
          ),
          column(5,
                 style = 'padding:0px;margin:0px;display: inline-flex;',
                 dropdownButton(
                   br(
                     paste0("Here the distributions of different chromatographic peak variables can be inspected.")
                   ),
                   tooltip = tooltipOptions(title = 'Click for description'),
                   circle = TRUE,
                   width = 600,
                   status = 'info',
                   icon = icon('question-circle'),
                   size = 'sm'
                 ),
                 plotlyOutput('bench_plotHisto', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")

                 )
        ),
        fluidRow(
          column(2, selectInput('bench_plotxy_input_x', 'x-axis',
                                choices = c(
                                  'Retention time [sec]' = 'peaks.rt',
                                  'Points per peak' = 'peaks.PpP',
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
                                  'Theoretic isotopic abundance' = 'isoabb',
                                  'Introduced sample group' = 'Grp',
                                  'Predicted area' = 'ExpectedArea',
                                  'Error_predicted area [%]' = 'ErrorRel_A',
                                  'Error_predicted area (abs)' = 'ErrorAbs_A',
                                  'Predicted height' = 'ExpectedHeight',
                                  'Error_predicted height [%]' = 'ErrorRel_H',
                                  'Error_predicted height (abs)' = 'ErrorAbs_H'
                                ), selected = 'ExpectedArea'
            )),
            column(2, selectInput('bench_plotxy_input_y', 'y-axis',
                                  choices = c(
                                    'Retention time [sec]' = 'peaks.rt',
                                    'Points per peak' = 'peaks.PpP',
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
                                    'Theoretic isotopic abundance' = 'isoabb',
                                    'Introduced sample group' = 'Grp',
                                    'Predicted area' = 'ExpectedArea',
                                    'Error_predicted area [%]' = 'ErrorRel_A',
                                    'Error_predicted area (abs)' = 'ErrorAbs_A',
                                    'Predicted height' = 'ExpectedHeight',
                                    'Error_predicted height [%]' = 'ErrorRel_H',
                                    'Error_predicted height (abs)' = 'ErrorAbs_H'
                                  ), selected = 'peaks.area'
                                  )
                   ),
          column(2, selectInput('bench_plotxy_input_color', 'Color-by',
                                choices = c(
                                  'Retention time [sec]' = 'peaks.rt',
                                  'Points per peak' = 'peaks.PpP',
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
                                  'Theoretic isotopic abundance' = 'isoabb',
                                  'Introduced sample group' = 'Grp',
                                  'Predicted area' = 'ExpectedArea',
                                  'Error_predicted area [%]' = 'ErrorRel_A',
                                  'Error_predicted area (abs)' = 'ErrorAbs_A',
                                  'Predicted height' = 'ExpectedHeight',
                                  'Error_predicted height [%]' = 'ErrorRel_H',
                                  'Error_predicted height (abs)' = 'ErrorAbs_H'
                                ), selected = 'molecule'
          )
          ),
          column(2),
          column(2, selectInput('bench_plotHisto', 'Peak variable',
                                choices = c(
                                  'Retention time [sec]' = 'peaks.rt',
                                  'Points per peak' = 'peaks.PpP',
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
                                  'Theoretic isotopic abundance' = 'isoabb',
                                  'Introduced sample group' = 'Grp',
                                  'Predicted area' = 'ExpectedArea',
                                  'Error_predicted area [%]' = 'ErrorRel_A',
                                  'Error_predicted area (abs)' = 'ErrorAbs_A',
                                  'Predicted height' = 'ExpectedHeight',
                                  'Error_predicted height [%]' = 'ErrorRel_H',
                                  'Error_predicted height (abs)' = 'ErrorAbs_H'
                                ), selected = 'peaks.PpP'
          )


        )
        ),
        fluidRow(
          #column(6,
          #  #offset = 0,
          #  style = 'padding:0px;margin:0px;display: inline-flex;',
          #  dropdownButton(
          #    br(
          #      "Individiual peaks depicted in the plots above can be plotted here by typing the IDX number of the peak in the textbox below.
          #    Simply slide with the mouse over the points in one of the plots above and take the IDX number from the tooltip box.
          #    The grey peak corresponds to the peak as it is predicted from the most abundant isotoplogue. The black line corresponds to the
          #    actually measured peak."
          #    ),
          #    tooltip = tooltipOptions(title = 'Click for description'),
          #    circle = TRUE,
          #    status = 'info',
          #    icon = icon('question-circle'),
          #    size = 'sm'
          #  ),
          #  plotlyOutput('graph_area_bench_3', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
          #),
          column(
            7,
            plotlyOutput('graph_area_bench_hm', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
          ),
          column(
            5,
            #offset = 7,
            style = 'padding:0px;margin:0px;display: inline-flex;',
            dropdownButton(
              br(
                "Here peaks corresponding to a specific molecule, adduct, isotopic abundance combination can be plotted over all samples"
              ),
              tooltip = tooltipOptions(title = 'Click for description'),
              circle = TRUE,
              width = 600,
              status = 'info',
              icon = icon('question-circle'),
              size = 'sm'
            ),
            plotlyOutput('graph_area_bench_4', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
          )

        ),
        fluidRow(
          column(7),
          #column(6, numericInput('index_number', 'Index Number', 1, step = 1)),
          tags$style(HTML("#mol+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
          tags$style(HTML("#add+ div>.selectize-d´ropdown {bottom: 100% !important;top:auto!important;}")),
          tags$style(HTML("#ia+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
          column(2, selectInput('mol', 'Molecule', c())),
          column(1, selectInput('add', 'Adduct', c())),
          column(1, selectInput('ia', 'Iso. Ab.', c())),
          column(1)
        )

      )
    ),
    tabPanel(
      'Assess UT data pre-processing',
      value = 'compare_tab',
      mainPanel(
        fluidRow(
          column(
            12, strong('1. Select Algorithm'), br(), br()
          )
        ),
        fluidRow(
          column(

            12, selectInput('algorithm_input', 'UT algorithm used', c('---', 'XCMS', 'msDial', 'CompoundDiscoverer', 'mzMine'), selected = '---')
          )
        ),
        fluidRow(
          column(
            12, strong('2. Select ungrouped and grouped files'), br(), br()
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            fileInput('ug_upload', 'Upload UT peaks (unaligned)', buttonLabel = 'Browse', multiple = TRUE),
            div(style = "width: 20px;"),
            fileInput('g_upload', 'Upload UT features (aligned peaks)', buttonLabel = 'Browse')
          )
        ),
        fluidRow(
          column(
            12, strong('3. Select benchmark and option files'), br(), br()
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            fileInput('benchmark_upload', 'Upload benchmark', buttonLabel = 'Browse'),
            div(style = "width: 20px;"),
            fileInput('options_upload', 'Upload Options File', buttonLabel = 'Browse')
          )
        ),
        fluidRow(
          column(
            12, strong('4. Select Main Feature Selection Method'), br(), br()
          )
        ),
        fluidRow(
          column(

            12, selectInput('main_feature_method_input', 'main feature method used', c('---', 'correct', 'match_iso_pattern'), selected = '---')
          )
        ),
        fluidRow(
          column(
            12, strong('5. Start!'), br(), br()
          )
        ),
        fluidRow(
          column(12,
                 style = "display: inline-flex;",
                 actionButton('start_compare', 'Start assessment', style =
                                'height: 34px; allign: center; background-color: #d2f8fa')
          )
        )
      )
    ),
    tabPanel(
      'Assessment results (peaks)',
      value = 'results_tab_peaks',
      mainPanel(
        width = '100%',
        fluidRow(column(12,
                        verbatimTextOutput('results_text')
        )),
        fluidRow(
          column(8,
                 dropdownButton(
                   br("texttext"),
                   tooltip = tooltipOptions(title = 'Click for description'),
                   circle = TRUE,
                   width = 600,
                   status = 'info',
                   icon = icon('question-circle'),
                   size = 'sm'
                 ),
                 plotlyOutput('overview_plot') %>% shinycssloaders::withSpinner(color="#0dc5c1")
          ),
          column(
            4,
            dropdownButton(
              br('Graph 3 Text'),
              tooltip = tooltipOptions(title = 'Click for description'),
              circle = TRUE,
              width = 600,
              status = 'info',
              icon = icon('question-circle'),
              size = 'sm'
            ),
            plotlyOutput('graph_area_3') %>% shinycssloaders::withSpinner(color="#0dc5c1")
          )
        ),
        fluidRow(
          column(3,
                 selectInput(
                   'overview_plot_input_x',
                   'x-axis',
                   c(
                     'Retention time [sec]' = 'rt_b',
                     'Points per peak' = 'peaks.PpP_b',
                     'Sharpness' = 'peaks.sharpness_b',
                     'FW25M' = 'peaks.FW25M_b',
                     'FW50M'= 'peaks.FW50M_b',
                     'FW75M' = 'peaks.FW75M_b',
                     'Zigzag index' = 'peaks.zigZag_IDX_b',
                     'Height' = 'peak_height_b',
                     'Area' = 'peak_area_b',
                     'mz measured' = 'peaks.mz_accurate_b',
                     'mz accuracy abs' = 'peaks.mz_accuracy_abs_b',
                     'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm_b',
                     'mz range (abs)' = 'peaks.mz_span_abs_b',
                     'mz range [ppm]' = 'peaks.mz_span_ppm_b',
                     'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0_b',
                     'Molecule' = 'molecule_b',
                     'Filename' = 'sample_name_b',
                     'Adduct' = 'adduct_b',
                     'Theoretic isotopic abundance' = 'isoabb_b',
                     'Introduced sample group' = 'Grp_b',
                     'Predicted area' = 'ExpectedArea_b',
                     'Error_predicted area [%]' = 'ErrorRel_A_b',
                     'Error_predicted area (abs)' = 'ErrorAbs_A_b',
                     'Predicted height' = 'ExpectedHeight_b',
                     'Error_predicted height [%]' = 'ErrorRel_H_b',
                     'Error_predicted height (abs)' = 'ErrorAbs_H_b'
                     )
                 )
          ),
          column(3,
                 selectInput(
                   'overview_plot_input_y',
                   'y-axis',
                   c(
                     'Retention time [sec]' = 'rt_b',
                     'Points per peak' = 'peaks.PpP_b',
                     'Sharpness' = 'peaks.sharpness_b',
                     'FW25M' = 'peaks.FW25M_b',
                     'FW50M'= 'peaks.FW50M_b',
                     'FW75M' = 'peaks.FW75M_b',
                     'Zigzag index' = 'peaks.zigZag_IDX_b',
                     'Height' = 'peak_height_b',
                     'Area' = 'peak_area_b',
                     'mz measured' = 'peaks.mz_accurate_b',
                     'mz accuracy abs' = 'peaks.mz_accuracy_abs_b',
                     'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm_b',
                     'mz range (abs)' = 'peaks.mz_span_abs_b',
                     'mz range [ppm]' = 'peaks.mz_span_ppm_b',
                     'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0_b',
                     'Molecule' = 'molecule_b',
                     'Filename' = 'sample_name_b',
                     'Adduct' = 'adduct_b',
                     'Theoretic isotopic abundance' = 'isoabb_b',
                     'Introduced sample group' = 'Grp_b',
                     'Predicted area' = 'ExpectedArea_b',
                     'Error_predicted area [%]' = 'ErrorRel_A_b',
                     'Error_predicted area (abs)' = 'ErrorAbs_A_b',
                     'Predicted height' = 'ExpectedHeight_b',
                     'Error_predicted height [%]' = 'ErrorRel_H_b',
                     'Error_predicted height (abs)' = 'ErrorAbs_H_b'
                     ),
                   selected = 'peak_height_b'
                 )
          ),
          column(2),
          column(
            4, selectInput(
              'graph_select_input',
              'Graph selection',
              c(
                'Retention time [sec]' = 'rt_b',
                'Points per peak' = 'peaks.PpP_b',
                'Sharpness' = 'peaks.sharpness_b',
                'FW25M' = 'peaks.FW25M_b',
                'FW50M'= 'peaks.FW50M_b',
                'FW75M' = 'peaks.FW75M_b',
                'Zigzag index' = 'peaks.zigZag_IDX_b',
                'Height' = 'peak_height_b',
                'Area' = 'peak_area_b',
                'mz measured' = 'peaks.mz_accurate_b',
                'mz accuracy abs' = 'peaks.mz_accuracy_abs_b',
                'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm_b',
                'mz range (abs)' = 'peaks.mz_span_abs_b',
                'mz range [ppm]' = 'peaks.mz_span_ppm_b',
                'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0_b',
                'Molecule' = 'molecule_b',
                'Filename' = 'sample_name_b',
                'Adduct' = 'adduct_b',
                'Theoretic isotopic abundance' = 'isoabb_b',
                'Introduced sample group' = 'Grp_b',
                'Predicted area' = 'ExpectedArea_b',
                'Error_predicted area [%]' = 'ErrorRel_A_b',
                'Error_predicted area (abs)' = 'ErrorAbs_A_b',
                'Predicted height' = 'ExpectedHeight_b',
                'Error_predicted height [%]' = 'ErrorRel_H_b',
                'Error_predicted height (abs)' = 'ErrorAbs_H_b'
                )
            )
          )
        ),

        fluidRow(
          column(
            4,
            dropdownButton(
              br(
                "Each column represents a benchmark feature and missed by the UT algorithm are color coded into random and systematic missing values.
              For simplification only benchmark features containing at least one missing value are shown.
                Note: This is not considering the alignment of the untargeted algorithm at all but only the peak detection!
                (R) random
                (S) systematic
                (F) found
                (NF) not found
                "
              ),
              tooltip = tooltipOptions(title = 'Click for description'),
              circle = TRUE,
              status = 'info',
              icon = icon('question-circle'),
              size = 'sm'
            ),
            plotlyOutput('graph_area_1') %>% shinycssloaders::withSpinner(color="#0dc5c1")
          ),

          column(
            4,
            dropdownButton(
              br('CV of benchmark peak areas as compared to the UT unaligned peaks. Each CV is calculated per molecule/adduct/isotopologue pair and over all
               replicates of a sample group. Only unaligned peak areas which were recovered in all replicates of a group were considered. Red represents
               CV increase by more than 10%p.'),
              tooltip = tooltipOptions(title = 'Click for description'),
              circle = TRUE,
              status = 'info',
              icon = icon('question-circle'),
              size = 'sm'
            ),
            plotlyOutput('graph_area_2') %>% shinycssloaders::withSpinner(color="#0dc5c1")
          ),
          column(
            4,
            dropdownButton(
              br('Here peaks corresponding to a specific molecule, adduct, isotopic abundance combination can be plotted over all samples. Solid lines correspond to
               to peak borders in the benchmark and dashed lines to peak borders set by the UT algorithm.'),
              tooltip = tooltipOptions(title = 'Click for description'),
              circle = TRUE,
              status = 'info',
              icon = icon('question-circle'),
              size = 'sm'
            ),
            plotlyOutput('graph_area_4') %>% shinycssloaders::withSpinner(color="#0dc5c1")
          )
        ),

        fluidRow(column(8),
                 ##Open Dropdowns upwards
                 tags$style(HTML("#mol_c+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                 tags$style(HTML("#add_c+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                 tags$style(HTML("#ia_c+ div>.selectize-dropdown {bottom: 100% !important;top:auto!important;}")),
                 ##
                 column(2, selectInput('mol_c', 'Molecule', c())),
                 column(1, selectInput('add_c', 'Adduct', c())),
                 column(1, selectInput('ia_c', 'Iso. Ab.', c()))
        )
      )
    ),
    tabPanel(
      'Assessment results (features)',
      value = 'results_tab_features',
      mainPanel(
        width = '100%',
        fluidRow(
          column(3, tableOutput('error_count')),

          column(4,
                 plotlyOutput('graph_hm_split') %>% shinycssloaders::withSpinner(color="#0dc5c1"))

        )
      )

    )

  ))

  choice_vector_bench <-
  c(
    'Retention time [sec]' = 'peaks.rt',
    'Points per peak' = 'peaks.PpP',
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
    'Theoretic isotopic abundance' = 'isoabb',
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
    'Sharpness' = 'peaks.sharpness_b',
    'FW25M' = 'peaks.FW25M_b',
    'FW50M'= 'peaks.FW50M_b',
    'FW75M' = 'peaks.FW75M_b',
    'Zigzag index' = 'peaks.zigZag_IDX_b',
    'Height' = 'peak_height_b',
    'Area' = 'peak_area_b',
    'mz measured' = 'peaks.mz_accurate_b',
    'mz accuracy abs' = 'peaks.mz_accuracy_abs_b',
    'mz accuracy [ppm]' = 'peaks.mz_accuracy_ppm_b',
    'mz range (abs)' = 'peaks.mz_span_abs_b',
    'mz range [ppm]' = 'peaks.mz_span_ppm_b',
    'Pearson cor. coef. with highest Iso.' = 'peaks.cor_w_M0_b',
    'Molecule' = 'molecule_b',
    'Filename' = 'sample_name_b',
    'Adduct' = 'adduct_b',
    'Theoretic isotopic abundance' = 'isoabb_b',
    'Introduced sample group' = 'Grp_b',
    'Predicted area' = 'ExpectedArea_b',
    'Error_predicted area [%]' = 'ErrorRel_A_b',
    'Error_predicted area (abs)' = 'ErrorAbs_A_b',
    'Predicted height' = 'ExpectedHeight_b',
    'Error_predicted height [%]' = 'ErrorRel_H_b',
    'Error_predicted height (abs)' = 'ErrorAbs_H_b'
  )

  server <- function (input, output, session) {

    shinyjs::js$disableTab('benchmark_results')
    shinyjs::js$disableTab('results_tab_features')
    shinyjs::js$disableTab('results_tab_peaks')

    volumes = getVolumes()
    observe({
      shinyFileChoose(input,
                      'mzML_upload',
                      roots = volumes,
                      session = session)
      shinyFileChoose(input,
                      'grps_upload',
                      roots = volumes,
                      session = session)
      shinyFileChoose(input, 'coi_upload', roots = volumes, session = session)
    })



    benchmark_data <- eventReactive(input$generate_benchmark, {

      shinyjs::disable('generate_benchmark')




      if (!is.null(input$mzML_upload)) {
        mzML_files <- parseFilePaths(volumes, input$mzML_upload)
        #print(mzML_files)
      }
      if (!is.null(input$grps_upload)) {
        grps_file <- parseFilePaths(volumes, input$grps_upload)
       # print(grps_file)
      }
      if (!is.null(input$coi_upload)) {
        coi_file <- parseFilePaths(volumes, input$coi_upload)
        #print(coi_file)
      }

      withProgress(message = 'Calculation in progress',

                   detail = "calculating isotopologue MZs...", value = 0, {



                     starttime <- Sys.time()
                     #######List of files#######
                     ###########################
                     files <-
                       mzML_files$datapath######list.files(input$mzML_upload$datapath, pattern = ".mzML", full.names = TRUE)#list.files("Z:/QE-HF/Yasin/Column Tests/RP_pos_YE_MM_MoMixCalib_12CMatrix oder 13C Matrix/centroided/13C", pattern = ".mzML", full.names = TRUE)## # par

                     #######Grps################
                     ##########################
                     grps <- fread(grps_file$datapath) # par

                     ###Compounds of interest###
                     ###########################

                     targets <- fread(coi_file$datapath) # par

                     if(!is.character(targets$molecule)) {targets$molecule <- as.character(targets$molecule)}

                     ####### PAR !
                     res_input <- input$resolution_drop
                     df <- resolution_list[[res_input]]


                     ######################OVERWRITE
                     #files <- list.files("Z:/QE-HF/Yasin/Column Tests/RP_pos_YE_MM_MoMixCalib_12CMatrix oder 13C Matrix/centroided/13C", pattern = ".mzML", full.names = TRUE)
                     #targets <- fread("Y:/Max/momix_hsst3_13C_compcol.csv")
                     #grps <- fread("Y:/Max/momix_hsst3_13C_grp.csv")

                     #df <- resolution_list$`OTFusion,QExactiveHF_120000@200`


                     #######################################

                     print('Start calculating isotopologue mz')

                     MassTraces <- getMZtable(
                       targets,
                       instrumentRes = df,
                       RelInt_threshold = input$RelInt_Thresh_input,
                       stick_method = "intensoid"
                     )
                     ###################################################
                     incProgress(1/15, detail = "detecting ROIs...")
                     print('Start ROI detection')

#mt <<- MassTraces

                     rois <- getROIsForEICs(
                       files = files,
                       Target.table = MassTraces,
                       PrecisionMZtol = input$percision_mz_tol_input,
                       plan = input$plan_input,
                       minCentroids = input$min_centroids_input,
                       AccurateMZtol = input$accurate_MZ_tol_input
                     )

                     incProgress(3/15, detail = "detecting peaks...")
                     ################################################
                     print('Start peak detection and evaluation')
#ttrtt <<- rois
                     PCbp <- findBenchPeaks(
                       files = files,
                       Grps = grps,
                       plan = input$plan_input,
                       CompCol = rois,
                       Min.PointsperPeak = input$min_PpP_input
                     )
#tt11tt <<- PCbp
                     incProgress(10/15, detail = "aligning peaks over samples...")
                     #####################################################

                     print('Start peak alignment')
                     PCal <- align_PC(PCbp[Iso_count > 1],
                                      add = "main_adduct",
                                      pick_best = "highest_mean_area")


                     fwrite(PCal, file = "Peak_list.csv", row.names = FALSE)

                     print(paste0("Benchmark dataset has been exported to ", getwd(), "/Peak_list.csv"))



                     incProgress(15/15, detail = "Finished")


                     shinyjs::enable('generate_benchmark')
                     shinyjs::js$enableTab('benchmark_results')

                     Sys.sleep(0.25)
                   })

      SkyTranList <- SkylineTransitionList(PCal)

      SkyPeakBo <- SkylinePeakBoundaries(PCal)

      print(Sys.time() - starttime)

      updateTabsetPanel(session = session, 'main_panel', selected = 'benchmark_results')

      return(list(files = files, targets = targets, PCal = PCal))
    })



    observeEvent({input$mol; input$add; input$ia},{
      benchmark_data <- benchmark_data()
      benchmark_data <- benchmark_data$PCal
      if(nrow(benchmark_data[molecule == input$mol & adduct == input$add & round(isoabb, 2) == input$ia]) > 0){

        p <- suppressWarnings(
          plot_Peak_per_mol(
            benchmark_data,
            mol = input$mol,
            add = input$add,
            ia = input$ia
          )
        )

        output$graph_area_bench_4 <- renderPlotly(p)
      }
    }, ignoreInit = FALSE)


    ##################
    ####COMPARISON####
    ##################

    comparison <- eventReactive(input$start_compare, {
      tryCatch({





      ######
      #Import csv files
      #####################
      options_table <- import_options(input$options_upload$datapath)
      b_table <- import_benchmark(input$benchmark_upload$datapath, options_table)
      import_results <- pick_algorithm(input$ug_upload, input$g_upload, options_table, input$algorithm_input)
      req(import_results)

      #Disable Elements
      shinyjs::disable('start_compare')
      shinyjs::js$disableTab('results_tab_features')
      shinyjs::js$disableTab('results_tab_peaks')

      #To-Do: Clear graphing Areas!!!!!


      updateTabsetPanel(session = session, 'main_panel', selected = 'results_tab_peaks')


      ug_table <- import_results$ug_table
      g_table <- import_results$g_table

      ######
      #perform comparison
      #####################
      comparison_ug_g <- compare_peaks_ug_g(b_table, ug_table, g_table, input$algorithm_input, input$main_feature_method_input)

      shinyjs::js$enableTab('results_tab_features')
      shinyjs::js$enableTab('results_tab_peaks')
      shinyjs::enable('start_compare')


      ###For Debug
      comparison_ev <<- comparison_ug_g
      ###

      return(comparison_ug_g)
      },
      error=function(error_message){
        print('Lol')
        print(error_message)
        return(NULL)
      })


    })


    ######
    #Calculate different graphs
    #####################

    #EIC_plot
    observeEvent({input$mol_c; input$add_c; input$ia_c},{
      comp <- comparison()
      print(comp)
      req(comp)
      comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)

      if(nrow(comp.dt[molecule_b == input$mol_c & adduct_b == input$add_c & round(isoabb_b, 2) == input$ia_c]) > 0){

        p <- suppressWarnings(
          plot_Peak_per_mol(
            comp.dt,
            mol = input$mol_c,
            add = input$add_c,
            ia = input$ia_c
          )
        )

        output$graph_area_4 <- renderPlotly(p)
      }
    }, ignoreInit = FALSE)


    #Scatter_plot
    observe({
    #observeEvent(c(input$overview_plot_input_x, input$overview_plot_input_y),{
      comp <- comparison()
      req(comp)
      f_nf_plot <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)

      f_nf_plot <- f_nf_plot[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

      f_nf_plot <- suppressWarnings(f_nf_plot[order(as.numeric(f_nf_plot$f_nf_col), decreasing = TRUE),])

      x = input$overview_plot_input_x
      y = input$overview_plot_input_y

      suppressWarnings(
        p <- ggplot() +
          geom_point(data = f_nf_plot[f_nf_col == TRUE], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                             y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                             col = "F",
                                                             molecule = molecule_b,
                                                             adduct = adduct_b,
                                                             isoabb = round(isoabb_b, 2),
                                                             sample_name = sample_name_b),
                     color = "blue") +

          geom_point(data = f_nf_plot[f_nf_col == FALSE], aes(x = if(x != "peak_height_b" & x != "peak_area_b") {get(x)} else {log10(get(x))},
                                                              y = if(y != "peak_height_b" & y != "peak_area_b") {get(y)} else {log10(get(y))},
                                                              col = "NF",
                                                              molecule = molecule_b,
                                                              adduct = adduct_b,
                                                              isoabb = isoabb_b,
                                                              sample_name = sample_name_b),
                     color = "red") +

          labs(x = if(x != "peak_height_b" & x != "peak_area_b") {names(choice_vector_comp)[choice_vector_comp == x]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == x], ")")},
               y = if(y != "peak_height_b" & y != "peak_area_b") {names(choice_vector_comp)[choice_vector_comp == y]} else {paste0("log10(", names(choice_vector_comp)[choice_vector_comp == y], ")")}) +
          ggtitle("Overview of found/not found peaks and their variables")
      )
      output$overview_plot <- renderPlotly(plotly::ggplotly(p, tooltip = c("molecule", "adduct", "isoabb", "sample_name"), dynamicTicks = TRUE))
    })

    #Missing value heatmap
    observe({
      comparison <- comparison()

      req(comparison)

      hm_dt <- rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)

      fwrite(hm_dt, 'debug_r_s.csv')

      hm_dt[, missing_peaks := find_r_s_error(
        comp_id_b,
        molecule_b,
        adduct_b,
        sample_id_b,
        isoabb_b,
        peak_area_b,
        peak_area_ug,
        peak_height_b
      ), by = .(molecule_b, adduct_b, isoabb_b)]



      hm_dt <- (hm_dt[, main_feature_check := ifelse((length(unique(na.omit(feature_id_g))) == 1) &
                                                           (isoabb_b == 100), 'TRUE', 'TRUE'),
                          by = .(molecule_b, adduct_b, isoabb_b)])
      hm_dt <- hm_dt[, overgroup := paste0(molecule_b, adduct_b)]
      hm_dt <- hm_dt[, if (any(missing_peaks != 'F')) .SD, by = .(molecule_b, adduct_b, isoabb_b)]
      hm_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]


      plot_r_s <- ggplot(
        hm_dt,
        aes(
          x = as.character(plot_group),
          y = sample_id_b,
          fill = missing_peaks,
          molecule = molecule_b,
          mz = mz_acc_b,
          isoabb = isoabb_b,
          adduct = adduct_b,
          FileName = sample_name_b
        )
      ) +
        geom_tile() +
        scale_fill_manual(values=c("forestgreen", "firebrick", "royalblue4", "mediumpurple1")) +
        ggtitle("Missing peaks") +
        labs(x = "benchmark features", y = "sample IDs", fill = "b_peaks") +
        theme(legend.title = element_blank())

      output$graph_area_1 <-
        renderPlotly(plotly::ggplotly(
          plot_r_s,
          tooltip = c("molecule", "adduct", "isoabb", "FileName", "mz")
        ))



      ####################################
      ########Alignment Errors table######
      ####################################

      testDT <-
        rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)

      #if(length(unique(testDT$peak_area_g)) != 1){
      #testDT <-
      #  testDT[, main_feature := as.numeric(names(which.max(table(feature_id_g)))), by =
      #           .(molecule_b, adduct_b, isoabb_b)]
      test <- testDT[, count_errors_max(.SD), .SDcols=c('molecule_b',
                                                        'adduct_b',
                                                        'main_peak',
                                                        'sample_id_b',
                                                        'isoabb_b',
                                                        'feature_id_g',
                                                        'peak_group_b',
                                                        'peak_area_g',
                                                        'peak_area_ug'),
                     by=.(molecule_b, adduct_b)]
      test <- setnames(test, c('V1', 'molecule_b', 'adduct_b'), c('errors', 'Molecule', 'Adduct'))
      output$error_count <- renderTable(test[errors > 0])


#}










      ###############end alignment error table


      cov_dt <-
        rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)


      #nf_dt <- rbindlist(list(comparison$nf_g))

      #View(nf_dt)
      cov_dt <- na.omit(cov_dt, cols = c('peak_area_ug'))
      cov_dt <-
        cov_dt[, complete_group_set := ifelse(.N == max(samples_per_group_b, na.rm = TRUE), 'TRUE', 'FALSE'), by =
                 .(molecule_b, adduct_b, isoabb_b, grp_b)]
      cov_dt <- cov_dt[complete_group_set == 'TRUE']
      #View(cov_dt)
      #print('here')

      #grped_consideration for CV plot
      #nf_dt <- rbindlist(list(comparison$nf_g))


      #tmp <- cov_dt[isoabb_b == 100 & !is.na(feature_id_g), .(ut_feature_nr = length(unique(feature_id_g))), by = .(molecule_b, adduct_b)]

      #tmp <- tmp[ut_feature_nr == 1]

      #cov_dt2 <- cov_dt[tmp, on = .(molecule_b, adduct_b)]

      #cov_dt2[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]

      #cov_dt2 <- cov_dt2[, reindexedFeatures_g := reIndexFeatures(feature_id_g), by = .(plot_group)]

      #cov_dt2 <- cov_dt2[cov_dt2[, .(mainFeature_g = names(sort(table(feature_id_g), decreasing = TRUE))[1]), by = .(plot_group)], on = .(plot_group)]

      #cov2 <- dt_test[cov_dt[, !c("peak_area_g", "feature_id_g")], on = .(molecule_b, adduct_b, isoabb_b, sample_name_b), nomatch = NA]

      #FOI <- unique(cov_dt$mainFeature_g)

      #unexpMF <- nf_dt[feature_id_g %in% FOI]



      #tmp <- rbindlist(list(tmp[mainFeature_g == feature_id_g], unexpMF), fill = TRUE)

      #cov_dt <- tmp
      #cov_dt_plot_dt2 <-
      #  tmp[, .(benchmark_peaks = sd(peak_area_g, na.rm = TRUE) / mean(peak_area_g, na.rm = TRUE) * 100,
      #             UT_unaligned_peaks = sd(peak_area_ug, na.rm = TRUE) / mean(peak_area_ug, na.rm = TRUE) * 100
      #  ), by = .(molecule_b, adduct_b, isoabb_b, grp_b)]
      ##grpd end


      ####test
      cov_dt_plot_dt <-
        cov_dt[, .(benchmark_peaks = sd(peak_area_b, na.rm = TRUE) / mean(peak_area_b, na.rm = TRUE) * 100,
                   UT_unaligned_peaks = sd(peak_area_ug, na.rm = TRUE) / mean(peak_area_ug, na.rm = TRUE) * 100
        ), by = .(molecule_b, adduct_b, isoabb_b, grp_b)]


      cov_dt_plot_dt[, diffH10PP := as.character(UT_unaligned_peaks - benchmark_peaks > 10)]

      cov_dt_plot_dt[diffH10PP == "TRUE"]$diffH10PP <- "Inc. > 10%p"
      cov_dt_plot_dt[diffH10PP == "FALSE"]$diffH10PP <- "Inc. < 10%p"

      cov_dt_plot_dt <-
        melt(
          cov_dt_plot_dt,
          id.vars = c('molecule_b', 'adduct_b', 'grp_b', 'isoabb_b', 'diffH10PP'),
          measure.vars = c('benchmark_peaks', 'UT_unaligned_peaks'),
          variable.name = 'data_type',
          value.name = 'CV'
        )

      cov_dt_plot_dt[, grp_col := paste0(molecule_b, adduct_b, grp_b, isoabb_b)]

      plot_cov <- ggplot(cov_dt_plot_dt) +
        suppressWarnings( geom_line(suppressWarnings( aes(x = data_type,
                                                          y = CV,
                                                          group = paste(grp_col, diffH10PP),
                                                          color = diffH10PP,
                                                          molecule = molecule_b,
                                                          adduct = adduct_b,
                                                          isoabb = isoabb_b,
                                                          grp = grp_b,
                                                          diffH10PP = diffH10PP
        )), alpha = 0.3)) +
        scale_color_manual(name = "+ > 10%p", values=c("blue", "red")) +
        ggtitle("Quality of peak borders") +
        labs(x = "", y = "CV [%]") +
        theme(legend.title = element_blank())

      output$graph_area_2 <- renderPlotly(ggplotly(plot_cov, tooltip = c("molecule", "adduct", "isoabb", "grp")))


      peak_i <- generate_results_text(comparison = comparison)

      output$results_text <- renderText(paste0(peak_i,
                                               "     Missing values (S|R): ", nrow(hm_dt[missing_peaks == "S"]), "|", nrow(hm_dt[missing_peaks == "R"]),
                                               "     CV increase (>10%p): ", nrow(cov_dt_plot_dt[diffH10PP == "Inc. > 10%p"]), "/", nrow(cov_dt_plot_dt), " (",
                                               round(nrow(cov_dt_plot_dt[diffH10PP == "Inc. > 10%p"])/nrow(cov_dt_plot_dt) * 100, 1), "%)",
                                               "     Min. # of alignment errors: ", sum(test$errors)))







      observeEvent(input$graph_select_input, {

        req(comparison)

        f_nf_plot <-
          rbindlist(list(comparison$c_table, comparison$nf_b_table), fill = TRUE)
        f_nf_plot <-
          f_nf_plot[, f_nf_col := ifelse(!is.na(peak_area_ug), 'TRUE', 'FALSE')]

        #print(sapply(f_nf_plot[, input$graph_select_input], class))
        if(input$graph_select_input %in% c("molecule_b", "adduct_b", "Grp_b", "sample_name_b")){

          the_plot1 <-
            ggplot(f_nf_plot,
                   aes_string(x = input$graph_select_input, fill = 'f_nf_col')) + stat_count(position =
                                                                                               'dodge') +
            scale_color_manual(name = "peaks found") +
            ggtitle("Distribution of found/not found peaks") +
            theme(axis.text.x = element_blank())


        } else{

          #calc binwidth
          #binwidth = 0.5

          var <- input$graph_select_input



          if(var == "peak_height_b" | var == "peak_area_b"){
            f_nf_plot[, var] <- log10(setDT(f_nf_plot)[, ..var])
          }


          binwidth <- (max(as.numeric(unlist(f_nf_plot[, ..var])), na.rm = TRUE) - min(as.numeric(unlist(f_nf_plot[, ..var])), na.rm = TRUE)) / 20


          # Count how many of each lab1 within each bin of var1
          df_bin <- f_nf_plot %>%
            count(var = floor(!! sym(var)/binwidth)*binwidth, f_nf_col)

          df_bin <- as.data.table(df_bin)
          df_bin <- df_bin[df_bin[, .(MAXn = max(n)), by = var], on = .(var)]
          df_bin <- df_bin[MAXn>=10]


          # Get "no" share within each bin
          df_sum <- df_bin %>%
            group_by(var) %>%
            summarize(no_pct = 100 * sum(n * (f_nf_col == "TRUE")) / sum(n))

          t <- plotly::ggplotly(
            ggplot2::ggplot() +
              geom_col(data = df_bin, aes(var, n, fill = f_nf_col),
                       position = position_dodge(preserve = "single")) +
              theme(legend.position = "none") +
              scale_fill_manual(values  = c("red", "blue")) +
              ggtitle("Distribution of found/not found peaks")
          )


          the_plot1 <- t %>% add_trace(x=~var,
                                       y =~no_pct,
                                       line = list(color = 'rgb(0, 0, 0)'),
                                       marker = list(color = 'rgb(0, 0, 0)'),
                                       yaxis = "y2",
                                       data = df_sum,
                                       showlegend = FALSE,
                                       inherit = FALSE,
                                       mode = 'lines+markers',
                                       type = "scatter")%>%
            layout(yaxis2 = list(
              #tickfont = list(size=11.7),
              titlefont=list(size=14.6),
              overlaying = "y",
              #nticks = 5,
              side = "right",
              title = "found peaks [%]"
            ),
            yaxis = list(title = "peak count",
                         titlefont=list(size=14.6)
            ),
            xaxis = list(title = names(choice_vector_comp)[choice_vector_comp == var],
                         titlefont=list(size=14.6)
            ),
            margin = list(r = 100),
            showlegend = FALSE
            )

        }
        output$graph_area_3 <- renderPlotly(plotly::ggplotly(the_plot1, dynamicTicks = TRUE))
      })
    })


    ####Test Move
    #observeEvent(input$start_compare, {
    #  updateTabsetPanel(session = session, 'main_panel', selected = 'results_tab_peaks')
    #})
    #######



    #observeEvent(input$generate_benchmark, {
    #  updateTabsetPanel(session = session, 'main_panel', selected = 'benchmark_results')
    #})

    ######MS DAIL ERROR
    # observe({
    #   comparison <- comparison()
    #   print(comparison)
    #   matched_dt <-
    #     rbindlist(list(comparison$c_table), fill = TRUE)
    #
    #   if(length(unique(matched_dt$peak_area_g)) != 1){
    #
    #   matched_dt <- matched_dt[, c("molecule_b", "adduct_b", "isoabb_b", "peak_area_g", "feature_id_g", "sample_name_b", "sample_id_b")]
    #
    #   print(matched_dt)
    #   fwrite(matched_dt, 'matched.csv')
    #
    #
    #   if(!all(is.na(matched_dt$feature_id_g))){
    #
    #     print('111')
    #     hm_split_plot_dt <- matched_dt[isoabb_b == 100 & !is.na(feature_id_g), .(ut_feature_nr = length(unique(feature_id_g))), by = .(molecule_b, adduct_b)]
    #     print('222')
    #     hm_split_plot_dt <- hm_split_plot_dt[ut_feature_nr == 1]
    #     print('333')
    #     hm_split_plot_dt <- matched_dt[hm_split_plot_dt, on = .(molecule_b, adduct_b)]
    #     print('444')
    #     hm_split_plot_dt[, plot_group := .GRP, by = .(molecule_b, adduct_b, isoabb_b)]
    #     print('555')
    #     hm_split_plot_dt <- hm_split_plot_dt[, reindexedFeatures_g := reIndexFeatures(feature_id_g), by = .(plot_group)]
    #     print('666')
    #     fwrite(hm_split_plot_dt, 'hm.csv')
    #     hm_split_plot_dt <- hm_split_plot_dt[hm_split_plot_dt[, .(mainFeature_g = names(sort(table(feature_id_g), decreasing = TRUE))[1]), by = .(plot_group)], on = .(plot_group)]
    #     print('777')
    #     hm_split_plot_dt <- hm_split_plot_dt[, split_flag := (length(unique(reindexedFeatures_g)) > 1), by = .(plot_group)]
    #     print('888')
    #     hm_split_plot_dt <- hm_split_plot_dt[split_flag == TRUE]
    #     print('999')
    #
    #     hm_split = ggplot(
    #       hm_split_plot_dt,
    #       aes(
    #         x = as.character(plot_group),
    #         y = as.character(sample_id_b),
    #         fill = reindexedFeatures_g,
    #         molecule = molecule_b,
    #         isoabb = isoabb_b,
    #         adduct = adduct_b,
    #         FileName = sample_name_b
    #       )
    #     ) +
    #       geom_tile() +
    #       #scale_fill_manual(values=c("forestgreen", "firebrick", "royalblue4", "mediumpurple1")) +
    #       ggtitle("Split features") +
    #       labs(x = "benchmark features", y = "sample IDs", fill = "b_peaks") +
    #       theme(legend.title = element_blank())
    #
    #
    #  output$graph_hm_split <- renderPlotly(plotly::ggplotly(
    #    hm_split,
    #    tooltip = c("molecule", "adduct", "isoabb", "FileName"),
    #    dynamicTicks = TRUE
    #   ))
    #
    #  }
    #   }
    # })
    ######


    #input buttons to be updated live
    ####
    observe({
      comp <- comparison()
      req(comp)
      comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)
      updateSelectInput(session, 'mol_c', choices = as.character(unique(comp.dt$molecule_b)), selected = as.character(unique(comp.dt$molecule_b)[1]))
    })

    observeEvent(input$mol_c,{
      comp <- comparison()
      req(comp)
      comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)
      updateSelectInput(session, 'add_c', choices = unique(comp.dt[molecule_b == input$mol_c]$adduct_b))
    })

    observeEvent(c(input$mol_c, input$add_c),{
      comp <- comparison()
      req(comp)
      comp.dt <-  rbindlist(list(comp$c_table, comp$nf_b_table), fill = TRUE)
      updateSelectInput(session, 'ia_c', choices = sort(round(unique(comp.dt[molecule_b == input$mol_c & adduct_b == input$add_c]$isoabb_b), 2), decreasing = TRUE))
    })

    observe({
      benchmark <- benchmark_data()
      benchmark <- benchmark$PCal
      updateSelectInput(session, 'mol', choices = as.character(unique(benchmark$molecule)), selected = as.character(unique(benchmark$molecule)[1]))
    })

    observeEvent(input$mol,{
      benchmark <- benchmark_data()
      benchmark <- benchmark$PCal
      updateSelectInput(session, 'add', choices = unique(benchmark[molecule == input$mol]$adduct))
    })

    observeEvent(c(input$mol, input$add),{
      benchmark <- benchmark_data()
      benchmark <- benchmark$PCal
      updateSelectInput(session, 'ia', choices = sort(round(unique(benchmark[molecule == input$mol & adduct == input$add]$isoabb), 2), decreasing = TRUE))
    })

    observe({
      benchmark <- benchmark_data()
      benchmark <- benchmark$PCal
      #output$graph_area_bench_1 <-
      #  renderPlotly(plot_vs_prediction(PC, y = peaks.area))
      #output$graph_area_bench_2 <-
      #  renderPlotly(plot_vs_prediction(PC, y = ErrorRel_A))


      x = input$bench_plotxy_input_x
      y = input$bench_plotxy_input_y
      colb = input$bench_plotxy_input_color

      suppressWarnings(
              p <- ggplot() +
                geom_point(data = benchmark[!is.na(get(x)) & !is.na(get(y))], aes(x = get(x),
                                                                                  y = get(y),
                                                                                  color = get(colb),
                                                                                  molecule = molecule,
                                                                                  adduct = adduct,
                                                                                  isoabb = isoabb,
                                                                                  sample_name = FileName)) +
                labs(x = names(choice_vector_bench)[choice_vector_bench == x],
                     y = names(choice_vector_bench)[choice_vector_bench == y]) +
                labs(color=names(choice_vector_bench)[choice_vector_bench == colb]) +
                ggtitle("Overview - Peaks")
      )
        output$bench_plotxy <- renderPlotly(plotly::ggplotly(p,
                                                             tooltip = c("molecule",
                                                                         "adduct",
                                                                         "isoabb",
                                                                         "sample_name"),
                                                             dynamicTicks = TRUE,
                                                             width = 1000))
    })


    observe({
      benchmark <- benchmark_data()
      benchmark <- benchmark$PCal


      var = input$bench_plotHisto


suppressWarnings(
        if(!(var %in% c("molecule", "FileName", "Grp", "adduct"))){
          p <- ggplot() +
          geom_histogram(data = benchmark[!is.na(get(var))], aes(get(var)), bins = 30) +
          ggtitle("Overview - Histogram") +
          xlab(names(choice_vector_bench)[choice_vector_bench == var])

        } else{
          p <- ggplot() +
          geom_bar(data = benchmark[!is.na(get(var))], aes(as.character(get(var)))) +
          ggtitle("Overview - Histogram") +
          xlab(names(choice_vector_bench)[choice_vector_bench == var])

        }
)


      output$bench_plotHisto <- renderPlotly(plotly::ggplotly(p, dynamicTicks = TRUE))


      output$graph_area_bench_3 <-
        renderPlotly(plot_Peak_with_predicted_peak(benchmark,
                                                   IndexNumber = input$index_number))

    })

observe({
  benchmark_set <- benchmark_data()
  benchmark_all <- benchmark_set$PCal
  benchmark <- unique(benchmark_all[!is.na(peaks.PpP) & isoabb == 100, c("molecule", "FileName", "isoabb")], by = c("molecule", "FileName"))
  benchmark_files <- benchmark_set$files
  benchmark_targets <- benchmark_set$targets
  benchmark_targets <- unique(benchmark_targets[, "molecule"])



  files.dt <- data.table(FileName = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(benchmark_files)))
  files.dt[,fileIdx:= seq(nrow(files.dt))]
  benchmark_targets$fileIdx <- rep(1, nrow(benchmark_targets))
  benchmark_targets <- benchmark_targets[files.dt, on=.(fileIdx<=fileIdx), allow.cartesian = TRUE]

  plot.dt <- benchmark[benchmark_targets, on = .(molecule, FileName), nomatch = NA]

  plot.dt$Found <- !is.na(plot.dt$isoabb)


  plot.dt <- plot.dt[Found == TRUE, .(nr = .N), by = .(molecule, Found)][, !"Found"][plot.dt, on =.(molecule), nomatch = NA]
  plot.dt[is.na(nr)]$nr <- 0

  plot_hm <- ggplot(
    plot.dt,
    aes(
      x = reorder(as.character(molecule), nr),
      y = as.character(FileName),
      fill = as.factor(Found),
      molecule = molecule,
      FileName = FileName
    )
  ) +
    geom_tile() +
    scale_fill_manual(values=c("firebrick", "forestgreen")) +
    ggtitle("Found/not found compounds per sample") +
    labs(x = "molecule", y = "file name", fill = "Found")# +
    #theme(axis.text.x = element_text(angle = 45))
#    theme(legend.title = element_blank())
#
  output$graph_area_bench_hm <-
    renderPlotly(plotly::ggplotly(
      plot_hm,
      width = 1000,
      tooltip = c("molecule", "FileName")
 ))

  output$results_text_b <- renderText(paste0("# of benchmark peaks: ", nrow(benchmark_all), "       # of compounds: " , length(unique(benchmark_all$molecule)),
                                           "       median FWHM [s]: ", round(median(benchmark_all$peaks.FW50M, na.rm = TRUE), 1), "       median # points per peak: ",
                                           median(benchmark_all$peaks.PpP), "       median mz accuracy [ppm]: ",
                                           round(median(benchmark_all$peaks.mz_accuracy_ppm), 1)))

})




  }

  shinyApp(ui, server)

}


callmzRAPP()
