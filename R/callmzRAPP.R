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

  options(shiny.reactlog = F)
  options(useFancyQuotes = FALSE)
  options(shiny.trace = T)
  #options(shiny.maxRequestSize = 100000 * 1024 ^ 2)
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
                        fluidRow(
                                 column(4,
                                        actionButton(inputId = 'mzML_upload',
                                                     label = 'Select mzML files',
                                                     width = '100%')
                                        ),
                                 column(4,
                                        actionButton(inputId = 'grps_upload',
                                                     label = 'Select sample-group file',
                                                     width = '100%')
                                         ),
                                 column(4,
                                         actionButton(inputId = 'coi_upload',
                                                      label = 'Select target file',
                                                      width = '100%')
                                         )
                                 ),
                        fluidRow(
                                 column(4,
                                        verbatimTextOutput(outputId = 'mzML_upload_files',placeholder = TRUE)
                                        ),
                                 column(4,
                                        verbatimTextOutput(outputId = 'grps_upload_file',placeholder = TRUE)
                                        ),
                                 column(4,
                                        verbatimTextOutput(outputId = 'coi_upload_file',placeholder = TRUE)
                                        )
                                )
                        )
                ),
        #Line Break for layout
        fluidRow(column(6,br())),
        fluidRow(
                 column(4,
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
                        strong("2. Set parameters:", style = "font-size:30px"),
                        br(),
                        p("(Processing plan: multiprocess for parallel processing on
                           Windows computers; sequential for serial processing; for other
                           options see help of future::plan)")
                       )
                ),
        fluidRow(
                 column(2,numericInput('RelInt_Thresh_input', 'Lowest iso. to be considered [%]', 0.05, step = 0.01, max = 100)),
                 column(2,numericInput('min_PpP_input', 'Min. # of scans per peak', 10, step = 1, min = 5))
                ),
        fluidRow(
                 column(2,numericInput('percision_mz_tol_input', 'mz precision [ppm]', 5, step = 0.1)),
                 column(2,numericInput('accurate_MZ_tol_input', 'mz accuracy [ppm]', 5, step = 0.1))
                ),
        fluidRow(
                 column(2,selectInput('plan_input', 'Processing plan', c('multiprocess', 'sequential', 'multicore', 'cluster', 'remote')))
                ),
        fluidRow(
                 column(6,
                        strong("3. Start benchmark generation:", style = "font-size:30px"),
                        br(),
                        p("(depending on the number of files and
                          compounds this can take some time (minutes to hours))")
                       )
                ),
        fluidRow(
                 column(2,actionButton('generate_benchmark', 'Generate benchmark', style = "background-color: #d2f8fa"))
                ),

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
            12, strong('2. Select ungrouped and grouped files'), br(), br()
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            actionButton(inputId = 'ug_upload',
                         label = 'Select unaligned file(s)',
                         width = '190px'),
            div(style = "width: 20px;"),
            actionButton(inputId = 'g_upload',
                         label = 'Select aligned file',
                         width = '190px')
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            div(style='width:190px', verbatimTextOutput(outputId = 'ug_upload_files',placeholder = TRUE)),
            div(style = "width: 20px;"),
            div(style='width:190px', verbatimTextOutput(outputId = 'g_upload_file',placeholder = TRUE)),
            div(style = "width: 20px;")
          )
        ),
        fluidRow(
          column(
            12, strong('3. Use previously generated benchmark and/or options file?'), br(), br()
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            prettySwitch(inputId = 'use_generated_benchmark',
                          label = 'Use generated benchmark',
                          value = FALSE,
                          width = '190px'),
            div(style = "width: 20px;"),
            prettySwitch(inputId = 'use_generated_options',
                         label = 'Use generated options',
                         value = TRUE,
                         width = '190px')
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            div(style = "width:190px",
              conditionalPanel(condition = "!input.use_generated_benchmark",
                               actionButton(inputId = 'benchmark_upload',
                                            label = 'Select benchmark file',
                                            width = '190px')
              )
            ),
            div(style = "width: 20px;"),
            div(style = "width:190px",
              conditionalPanel(condition = "!input.use_generated_options",
                               actionButton(inputId = 'options_upload',
                                            label = 'Select options files',
                                            width = '190px')
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            style = "display: inline-flex;",
            div(style="width:190px",
              conditionalPanel(condition = "!input.use_generated_benchmark",
                               div(style='width:190px', verbatimTextOutput(outputId = 'benchmark_upload_file',placeholder = TRUE)),
              )
            ),
            div(style = "width: 20px;"),
            div(style = "width:190px",
              conditionalPanel(condition = "!input.use_generated_options",
                               div(style='width:190px', verbatimTextOutput(outputId = 'options_upload_file',placeholder = TRUE))
              )
            )
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
          column(3, tableOutput('error_count'), style="overflow-y:scroll; height:464px"),

          column(9,
                 fluidRow(
                   column(12,
                          plotlyOutput('graph_hm_split') %>% shinycssloaders::withSpinner(color="#0dc5c1")
                         )),
                   fluidRow(
                     column(2, pickerInput('mol_a', 'Molecule', c())),
                     column(1, pickerInput('add_a', 'Adduct', c()))
                   )
        )

      )

    )

  )))

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

    #shinyjs::js$disableTab('benchmark_results')
    #shinyjs::js$disableTab('results_tab_features')
    #shinyjs::js$disableTab('results_tab_peaks')
    #shinyjs::disable('ug_upload_files')


    ##Reactive Values
    data_dir <- reactiveVal(getwd())
    benchmark_data <- reactiveVal(NULL)
    comparison_data <- reactiveVal(NULL)

    observe({
      if((input$main_panel == 'benchmark_results') & is.null(benchmark_data())) {
        sendSweetAlert(session, title = 'No benchmark file generated', text = 'Benchmark must be generated before viewing the results', type = 'warning')
      }
      if(((input$main_panel == 'results_tab_peaks') |(input$main_panel == 'results_tab_features')) & is.null(comparison_data())) {
        sendSweetAlert(session, title = 'No peak comparison file generated', text = 'Peak comparison must be generated before viewing the results', type = 'warning')
      }
    })

    ##File Filters for choice cialogues
    mzML_filter <- matrix(c('mzML Files (*.mzML)', '*.mzML'), nrow = 1, ncol = 2)
    csv_filter <- matrix(c('Text files (*.csv, *.txt)', '*.csv;*.txt'), nrow = 1, ncol = 2)

    #File input reactives
    #Benchmark
    mzML_files <- reactive({
      if (input$mzML_upload == 0){return(NULL)}
      else {
        files <- tk_choose.files(caption = 'Select .mzML files', multi = TRUE, filters = mzML_filter)
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
        file <- tk_choose.files(caption = 'Select sample-group file', multi = FALSE, filters = csv_filter)
        output$grps_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })

    coi_file <- reactive({
      if(input$coi_upload == 0){return(NULL)}
      else {
        file <- tk_choose.files(caption = 'Select target file', multi = FALSE, filters = csv_filter)
        output$coi_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })


    #Comparison
    ug_files <- reactive({
      if (input$ug_upload == 0){return(NULL)}
      else {
        files <- tk_choose.files(caption = 'Select ungrouped file(s)', multi = TRUE, filters = csv_filter)
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
        file <- tk_choose.files(caption = 'Select grouped file', multi = FALSE, filters = csv_filter)
        output$g_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })
    benchmark_file <- reactive({
      if (input$benchmark_upload == 0){return(NULL)}
      else {
        file <- tk_choose.files(caption = 'Select benchmark file', multi = FALSE, filters = csv_filter)
        output$benchmark_upload_file <- renderText(paste0(basename(file)))
        return(file)
      }
    })
    options_file <- reactive({
      if (input$options_upload == 0){return(NULL)}
      else {
        file <- tk_choose.files(caption = 'Select options file', multi = FALSE, filters = csv_filter)
        output$options_upload_file <- renderText(paste0(basename(file)))
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



    observeEvent(input$generate_benchmark, {
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
                }) #End of With Progress

      SkyTranList <- SkylineTransitionList(PCal)

      SkyPeakBo <- SkylinePeakBoundaries(PCal)

      print(Sys.time() - starttime)

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
        #To-Do: Clear graphing Areas!!!!!
        shinybusy::show_modal_spinner(spin='scaling-squares', text='Please wait while we make things traceable for you.')
        #####################
        #Import csv files
        #####################
        if(input$use_generated_options == TRUE){
          options_path <- 'generate'
        } else {
          options_path <- options_file()
        }
        #options_table <- import_options(options_file())
        if (input$use_generated_benchmark == TRUE) {
          b_o_tables <- import_benchmark(isolate(benchmark_data())$PCal, options_path, from_csv = FALSE, input$algorithm_input)
          b_table = b_o_tables$b_table
          options_table <- b_o_tables$options_table
        } else {
          b_o_tables <- import_benchmark(benchmark_file(), options_path, from_csv = TRUE, input$algorithm_input)
          b_table = b_o_tables$b_table
          options_table <- b_o_tables$options_table
        }
        if(input$algorithm_input == '---'){
          stop('Please select an algorithm')
        }
        import_results <- pick_algorithm(ug_files(), g_file(), options_table, input$algorithm_input)
        ug_table <- import_results$ug_table
        g_table <- import_results$g_table
        req(import_results)


        #####################
        #perform comparison
        #####################
        comparison_ug_g <- compare_peaks_ug_g(b_table, ug_table, g_table, input$algorithm_input, input$main_feature_method_input)
        comparison_data(comparison_ug_g)
        shinybusy::remove_modal_spinner()
      },
      error=function(error_message){
        shinybusy::remove_modal_spinner()
        Sys.sleep(0.2) # Otherwise remove modal overwirites error modal
        sendSweetAlert(session, title = 'Error', text = geterrmessage(), type = 'error', closeOnClickOutside = FALSE)
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
    observeEvent({comparison_data(); input$mol_c; input$add_c; input$ia_c},{
      comparison_data <- isolate(comparison_data())
      if (!is.null(comparison_data)){
        output$graph_area_4 <- renderPlotly(plot_comp_peak_overview(comparison_data, input$mol_c, input$add_c, input$ia_c))
      }
    })


    #Scatter_plot
    observeEvent({comparison_data(); input$overview_plot_input_x; input$overview_plot_input_y}, {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$overview_plot <- renderPlotly(plot_comp_scatter_plot(comparison_data(), input$overview_plot_input_x, input$overview_plot_input_y, choice_vector_comp))
      }
    })

    #R/S Heatmap Plot
    observeEvent(comparison_data(), {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$graph_area_1 <- renderPlotly(plot_comp_missing_value_hm(comparison_data()))
      }
    })

    #Ditsribution of peaks plot
    observeEvent({comparison_data(); input$graph_select_input}, {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$graph_area_3 <- renderPlotly(plot_comp_dist_of_found_peaks(comparison_data(), input$graph_select_input, choice_vector_comp))
      }
    })
    #Isotopologe prediction error
    observeEvent(comparison_data(), {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$graph_area_2 <- renderPlotly(plot_comp_iso_pred_error(comparison_data()))
      }
    })
    #Results Text
    observeEvent(comparison_data(), {
      comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data)){
        output$results_text <- renderText(generate_results_text(comparison_data))

      }
    })
    #Alignment table
    observeEvent(comparison_data(), {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$error_count <- renderTable(comparison_data()$ali_error_table[errors > 0])
      }
    })

    #Alignment error plot
    observeEvent(comparison_data(),{
      #comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data())){
        error_molecules <- as.character(comparison_data()$ali_error_table[errors > 0, Molecule])
        no_error_molecules <- as.character(comparison_data()$ali_error_table[errors == 0, Molecule])
        choices <- list('Errors:' = as.list(error_molecules), 'No errors:' = as.list(no_error_molecules))
        print(choices)
        updateSelectInput(session, 'mol_a', choices = choices)
      }
    })
    observeEvent(input$mol_a, {
      #comparison_data<-isolate(comparison_data())
      if(!is.null(comparison_data())){
        error_adducts <- as.character(comparison_data()$ali_error_table[(Molecule == input$mol_a) & (errors > 0), Adduct])
        no_error_adducts <- as.character(comparison_data()$ali_error_table[(Molecule == input$mol_a) & (errors == 0), Adduct])
        choices <- list('Errors:' = as.list(error_adducts), 'No errors:' = as.list(no_error_adducts))
        print(choices)
        updateSelectInput(session, 'add_a', choices = choices)
      }
    })
    observeEvent(comparison_data(), {
      #comparison_data <- isolate(comparison_data())
      if(!is.null(comparison_data())){
        output$graph_hm_split <- renderPlotly(Alignment_error_plot(comparison_data(), mol = input$mol_a, add = input$add_a))
      }
    })



  }

  shinyApp(ui, server)

}
