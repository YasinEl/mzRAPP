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

  options(shiny.reactlog = TRUE)
  options(useFancyQuotes = FALSE)
  options(shiny.trace = F)
  options(shiny.maxRequestSize = 100000 * 1024 ^ 2)
  options(spinner.type = 4)

  data('resolution_list')

  ui <- tagList(shinyjs::useShinyjs(),
                shinyjs::extendShinyjs(text = jscode),
                shinyjs::inlineCSS(css),
                navbarPage(

    #App Title
    'mzRAPP',
    #ID for entire Page
    id = 'main_panel',

    #First Tab: Gnerate Benchmark
    tabPanel(
      title = 'Generate benchmark',
      value = 'generate_benchmark_tab',

        #1st Row
        fluidRow(
          column(4,
            strong("1. Select necessary files:", style = "font-size:30px"),
            p("(and choose resolution used)")
          )
        ),

        #2nd Row
        fluidRow(
          column(6,
            style = "display: inline-flex;",

            actionButton(inputId = 'mzML_upload',
                         label = 'Select mzML files',
                         width = '190px'),

            div(style = "width: 20px;"),

            actionButton(inputId = 'grps_upload',
                         label = 'Select sample-group file',
                         width = '190px'),

            div(style = "width: 20px;"),

            actionButton(inputId = 'coi_upload',
                         label = 'Select target file',
                         width = '190px'),
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
              ),            plotlyOutput('graph_area_bench_overview', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
                 plotlyOutput('graph_area_bench_histo', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")

                 )
        ),
        fluidRow(
          column(2, selectInput('bench_overview_input_x', 'x-axis',
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
            column(2, selectInput('bench_overview_input_y', 'y-axis',
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
          column(2, selectInput('bench_overview_input_color', 'Color-by',
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
          column(2, selectInput('select_bench_histo', 'Peak variable',
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
            plotlyOutput('graph_area_bench_peak_overview', width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
            12, strong('2. Select ungrouped, groupedand option files'), br(), br()
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            actionButton(inputId = 'ug_upload',
                         label = 'Select ungrouped file(s)',
                         width = '190px'),
            div(style = "width: 20px;"),
            actionButton(inputId = 'g_upload',
                         label = 'Select grouped file',
                         width = '190px'),
            div(style = "width: 20px;"),
            actionButton(inputId = 'options_upload',
                         label = 'Select options files',
                         width = '190px')
          )
        ),
        fluidRow(
          column(
            12, strong('3. Use previously generated benchmark or upload benchmark file'), br(), br()
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            checkboxInput(inputId = 'use_generated_benchmark',
                          label = 'Use generated benchmark',
                          value = FALSE,
                          width = '190px'),
            div(style = "width: 20px;"),
            actionButton(inputId = 'benchmark_upload',
                         label = 'Select benchmark file',
                         width = '190px')

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

  server <- function (input, output, session) {

    shinyjs::js$disableTab('benchmark_results')
    shinyjs::js$disableTab('results_tab_features')
    shinyjs::js$disableTab('results_tab_peaks')


    ##Reactive Values
    data_dir <- reactiveVal(getwd())
    benchmark_data <- reactiveVal(NULL)
    comparison_data <- reactiveVal(NULL)

    ##File Filters for choice cialogues
    mzML_filter <- matrix(c('mzML Files (*.mzML)', '*.mzML'), nrow = 1, ncol = 2)
    csv_filter <- matrix(c('Text files (*.csv, *.txt)', '*.csv;*.txt'), nrow = 1, ncol = 2)

    #File input reactives
    #Benchmark
    mzML_files <- reactive({
      if (input$mzML_upload == 0){return(NULL)}
      else {
        files <- rchoose.files(default = isolate(data_dir()), caption = 'Select .mzML files', multi = TRUE, filters = mzML_filter)
        if(!is.na(dirname(files[1]))){data_dir(dirname(files[1]))}
        return(files)
      }
    })

    grps_file <- reactive({
      if(input$grps_upload[1] == 0){return(NULL)}
      else {
        file <- rchoose.files(default = isolate(data_dir()), caption = 'Select sample-group file', multi = FALSE, filters = csv_filter)
        if(!is.na(dirname(file[1]))){data_dir(dirname(file[1]))}
        return(file)
      }
    })

    coi_file <- reactive({
      if(input$coi_upload == 0){return(NULL)}
      else {
        file <- rchoose.files(default = isolate(data_dir()), caption = 'Select target file', multi = FALSE, filters = csv_filter)
        if(!is.na(dirname(file[1]))){data_dir(dirname(file[1]))}
        return(file)
      }
    })


    #Comparison
    ug_files <- reactive({
      if (input$ug_upload == 0){return(NULL)}
      else {
        files <- rchoose.files(default = isolate(data_dir()), caption = 'Select ungrouped file(s)', multi = TRUE, filters = csv_filter)
        if(!is.na(dirname(files[1]))){data_dir(dirname(files[1]))}
        return(files)
      }
    })
    g_file <- reactive({
      if (input$g_upload == 0){return(NULL)}
      else {
        file <- rchoose.files(default = isolate(data_dir()), caption = 'Select grouped file', multi = FALSE, filters = csv_filter)
        if(!is.na(dirname(file[1]))){data_dir(dirname(file[1]))}
        return(file)
      }
    })
    benchmark_file <- reactive({
      if (input$benchmark_upload == 0){return(NULL)}
      else {
        file <- rchoose.files(default = isolate(data_dir()), caption = 'Select benchmark file', multi = FALSE, filters = csv_filter)
        if(!is.na(dirname(file[1]))){data_dir(dirname(file[1]))}
        return(file)
      }
    })
    options_file <- reactive({
      if (input$options_upload == 0){return(NULL)}
      else {
        file <- rchoose.files(default = isolate(data_dir()), caption = 'Select options file', multi = FALSE, filters = csv_filter)
        if(!is.na(dirname(file[1]))){data_dir(dirname(file[1]))}
        return(file)
      }
    })

    #General Observers
    observe({mzML_files()})
    observe({grps_file()})
    observe({coi_file()})
    observe({ug_files()})
    observe({g_file()})
    observe({benchmark_file()})
    observe({options_file()})
    observe({
              if(!is.null(benchmark_data())){
                updateCheckboxInput(session, 'use_generated_benchmark', value = TRUE)
                shinyjs::enable('use_generated_benchmark')
              } else {
                updateCheckboxInput(session, 'use_generated_benchmark', value = FALSE)
                shinyjs::disable('use_generated_benchmark')
              }
           })



    observeEvent(input$generate_benchmark, {

      #Disable the generate benchmark button to prevent multiple clicks
      shinyjs::disable('generate_benchmark')
      shinyjs::js$disableTab('benchmark_results')


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
      }

      #coi
      if(is.null(coi_file()) || length(coi_file()) == 0){
        stop('No coi file selected')
      } else {
        targets <- fread(coi_file())
        if(!is.character(targets$molecule)) {targets$molecule <- as.character(targets$molecule)}
      }

      #Resolution
      res_input <- input$resolution_drop
      resolution_df <- resolution_list[[res_input]]

      withProgress(message = 'Calculation in progress',
                   detail = "calculating isotopologue MZs...", value = 0, {
                   starttime <- Sys.time()

                   ###################################################
                   print('Start calculating isotopologue mz')

                   MassTraces <- getMZtable(targets,
                                            instrumentRes = resolution_df,
                                            RelInt_threshold = input$RelInt_Thresh_input,
                                            stick_method = "intensoid"
                                           )

                   ###################################################
                   incProgress(1/15, detail = "detecting ROIs...")
                   print('Start ROI detection')


                   rois <- getROIsForEICs(files = files,
                                          Target.table = MassTraces,
                                          PrecisionMZtol = input$percision_mz_tol_input,
                                          plan = input$plan_input,
                                          minCentroids = input$min_centroids_input,
                                          AccurateMZtol = input$accurate_MZ_tol_input
                                         )

                   ################################################
                   incProgress(3/15, detail = "detecting peaks...")
                   print('Start peak detection and evaluation')

                   PCbp <- findBenchPeaks(files = files,
                                          Grps = grps,
                                          plan = input$plan_input,
                                          CompCol_all = rois,
                                          Min.PointsperPeak = input$min_PpP_input
                                         )

                   #####################################################
                   incProgress(10/15, detail = "aligning peaks over samples...")
                   print('Start peak alignment')

                   PCal <- align_PC(PCbp[Iso_count > 1],
                                    add = "main_adduct",
                                    pick_best = "highest_mean_area"
                                   )

                   #####################################################
                   fwrite(PCal, file = "Peak_list.csv", row.names = FALSE)
                   print(paste0("Benchmark dataset has been exported to ", getwd(), "/Peak_list.csv"))
                   incProgress(15/15, detail = "Finished")


                   shinyjs::enable('generate_benchmark')
                   shinyjs::js$enableTab('benchmark_results')
                }) #End of With Progress

      SkyTranList <- SkylineTransitionList(PCal)

      SkyPeakBo <- SkylinePeakBoundaries(PCal)

      print(Sys.time() - starttime)

      updateTabsetPanel(session = session, 'main_panel', selected = 'benchmark_results')

      benchmark_data(list(files = files, targets = targets, PCal = PCal))
    })

    #Benchmark Plot Functions
    #Benchmark overview plot
    observeEvent({benchmark_data(); input$bench_overview_input_x; input$bench_overview_input_y; input$bench_overview_input_color}, {
      benchmark_data <- isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        output$graph_area_bench_overview <- renderPlotly(plot_bench_overview(benchmark_data, input$bench_overview_input_x, input$bench_overview_input_y, input$bench_overview_input_color, choice_vector_bench))
      }
    })
    #Benchmark historgramm plot
    observeEvent({benchmark_data(); input$select_bench_histo}, {
      benchmark_data <- isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        output$graph_area_bench_histo <- renderPlotly(plot_bench_histo(benchmark_data, input$select_bench_histo, choice_vector_bench))
      }
    })

    #Benchmark heatmap plot
    observeEvent({benchmark_data()}, {
      benchmark_data <- isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        plot_and_text <- plot_bench_heatmap(benchmark_data)
        output$graph_area_bench_hm <- renderPlotly(plot_and_text$p)
        output$results_text_b <- renderText(plot_and_text$t)
      }
    })


    #Benchmark peak overview plot
    observeEvent(benchmark_data(),{
      benchmark_data<-isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        benchmark_data <- benchmark_data$PCal
        updateSelectInput(session, 'mol', choices = as.character(unique(benchmark_data$molecule)), selected = as.character(unique(benchmark_data$molecule)[1]))
      }
    })
    observeEvent(input$mol, {
      benchmark_data<-isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        benchmark_data <- benchmark_data$PCal
        updateSelectInput(session, 'add', choices = unique(benchmark_data[molecule == input$mol]$adduct))
      }
    })

    observeEvent({input$mol; input$add}, {
      benchmark_data<-isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        benchmark_data <- benchmark_data$PCal
        updateSelectInput(session, 'ia', choices = sort(round(unique(benchmark_data[molecule == input$mol & adduct == input$add]$isoabb), 2), decreasing = TRUE))
      }
    })

    observeEvent({benchmark_data(); input$mol; input$add; input$ia}, {
      benchmark_data<-isolate(benchmark_data())
      if(!is.null(benchmark_data)){
        output$graph_area_bench_peak_overview <- renderPlotly(plot_bench_peak_overview(benchmark_data, input$mol, input$add, input$ia))
      }
    })


    ##################
    ####COMPARISON####
    ##################

    comparison <- observeEvent(input$start_compare, {
      tryCatch({
      #####################
      #Import csv files
      #####################
      options_table <- import_options(options_file())
      if (input$use_generated_benchmark == TRUE) {
        b_table <- import_benchmark(isolate(benchmark_data())$PCal, options_table, from_csv = FALSE)
      } else {
        b_table <- import_benchmark(benchmark_file(), options_table)
      }
      print(ug_files())
      import_results <- pick_algorithm(ug_files(), g_file(), options_table, input$algorithm_input)
      ug_table <- import_results$ug_table
      g_table <- import_results$g_table
      req(import_results)

      #Disable Elements
      shinyjs::disable('start_compare')
      shinyjs::js$disableTab('results_tab_features')
      shinyjs::js$disableTab('results_tab_peaks')

      #To-Do: Clear graphing Areas!!!!!


      #updateTabsetPanel(session = session, 'main_panel', selected = 'results_tab_peaks')




      ######
      #perform comparison
      #####################
      comparison_ug_g <- compare_peaks_ug_g(b_table, ug_table, g_table, input$algorithm_input, input$main_feature_method_input)

      shinyjs::js$enableTab('results_tab_features')
      shinyjs::js$enableTab('results_tab_peaks')
      shinyjs::enable('start_compare')

      comparison_data(comparison_ug_g)
      },
      error=function(error_message){
        shinyjs::enable('start_compare')
        print('Lol')
        print(error_message)
        return(NULL)
      })


    })
    ######
    #Comparison Plot functions
    #####################

    #Comparison peak overview plot
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
        print('add_c')
        comp.dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)
        updateSelectInput(session, 'add_c', choices = unique(comp.dt[molecule_b == input$mol_c]$adduct_b))
      }
    })

    observeEvent({input$mol_c; input$add_c}, {
      comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data)){
        print('ia_c')
        comp.dt <-  rbindlist(list(comparison_data$c_table, comparison_data$nf_b_table), fill = TRUE)
        updateSelectInput(session, 'ia_c', choices = sort(round(unique(comp.dt[molecule_b == input$mol_c & adduct_b == input$add_c]$isoabb_b), 2), decreasing = TRUE))
      }
    })

    #EIC_plot
    observeEvent({comparison_data(); input$mol_c; input$add_c; input$ia_c},{
      comparison_data <- isolate(comparison_data())
      if (!is.null(comparison_data)){
        output$graph_area_4 <- renderPlotly(plot_comp_peak_overview(comparison_data, input$mol_c, input$add_c, input$ia_c))
      }
    })


    #Scatter_plot
    observeEvent({comparison_data(); input$overview_plot_input_x; input$overview_plot_input_y}, {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){
        output$overview_plot <- renderPlotly(plot_comp_scatter_plot(comparison_data, input$overview_plot_input_x, input$overview_plot_input_y, choice_vector_comp))
      }
    })

    #R/S Heatmap Plot
    observeEvent(comparison_data(), {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){
        output$graph_area_1 <- renderPlotly(plot_comp_missing_value_hm(comparison_data))
      }
    })

    #Ditsribution of peaks plot
    observeEvent({comparison_data(); input$graph_select_input}, {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){
        output$graph_area_3 <- renderPlotly(plot_comp_dist_of_found_peaks(comparison_data, input$graph_select_input, choice_vector_comp))
      }
    })
    #Isotopologe prediction error
    observeEvent(comparison_data(), {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){
        output$graph_area_2 <- renderPlotly(plot_comp_iso_pred_error(comparison_data))
      }
    })
    #Results Text
    observeEvent(comparison_data(), {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){
        output$results_text <- renderText(generate_results_text(comparison_data))

      }
    })
  }

  shinyApp(ui, server)

}
