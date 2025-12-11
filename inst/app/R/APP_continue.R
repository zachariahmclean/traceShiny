continue_server <- function(input, output, session) {

  reactive_continue <- reactiveValues(laddertable = NULL,
                                      fsa_list = NULL,
                                      fastq = NULL,
                                      config = NULL,
                                      metadata_table = NULL,
                                      metadata_table_fastq = NULL,
                                      ladders = NULL,
                                      instability_metrics = NULL,
                                      index_list = NULL)

  observeEvent(input$continue, {
    showModal(modalDialog(
      title = strong("CONTINUE"),

      fileInput("fileinputLOAD", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Load previously saved enviroment (.RData file)')),
                accept = c(".Rdata"),
                width = "100%"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })

  # Load function
  observeEvent(input$fileinputLOAD, {
    tryCatch({
      withProgress(message = 'Loading previously saved enviroment...', style = "old",
                   value = 0, {
                     incProgress(1/20, detail = "Loading Packages")

                     file = input$fileinputLOAD
                     if (is.null(file)) {
                       return(NULL)
                     }

                     library(shinyalert)
                     library(dashboardthemes)
                     library(trace)
                     library(dplyr)
                     library(assertr)
                     library(ggpubr)
                     library(tibble)
                     library(tidyr)
                     library(stringr)
                     library(readxl)
                     library(microseq)
                     library(purrr)

                     removeModal()

                     incProgress(3/10, detail = "Setting up previously saved variables")

                     load(file$datapath)

                     incProgress(5/10, detail = "Setting up upload settings")

                     if (exists("metadata_table")) {

                     shinyjs::hide("NextButtonLoad")
                     shinyjs::hide("NextButtonLadder")
                     shinyjs::hide("NextButtonPeaks")

                     #Load
                     if (input$LoadBoxIntro$collapsed == FALSE) {
                       js$collapse("LoadBoxIntro")
                     }
                     shinyjs::show("LoadBox2")
                     shinyjs::show("LoadBox3")
                     shinyjs::show("LoadBox4")

                     #Ladder
                     if(input$LadderBoxIntro$collapsed == FALSE) {
                       js$collapse("LadderBoxIntro")
                     }
                     shinyjs::show("LadderBox1")
                     shinyjs::show("LadderBox2")

                     #Peaks
                     if(input$PeaksBoxIntro$collapsed == FALSE) {
                       js$collapse("PeaksBoxIntro")
                     }
                     shinyjs::show("PeaksBox1")
                     shinyjs::show("PeaksBox2")

                     #Metrics
                     if(input$MetricsBoxIntro$collapsed == FALSE) {
                       js$collapse("MetricsBoxIntro")
                     }
                     shinyjs::show("MetricsBox1")
                     shinyjs::show("MetricsBox2")
                     shinyjs::show("MetricsBox3")

                     #Upload
                     updateRadioGroupButtons(session, "DataUpload", selected = DataUpload)
                     updateMaterialSwitch(session, "DataUploadMeta", value = DataUploadMeta)
                     updateMaterialSwitch(session, "Ladder_switch", value = Ladder_switch)

                     incProgress(6/10, detail = "Setting up ladder settings")

                     #Ladders
                     updatePickerInput(session,"LadderChannel", selected = config$ladder_channel)
                     updatePickerInput(session,"SignalChannel", selected = config$signal_channel)
                     updatePickerInput(session,"LadderSizes", selected = config$ladder_sizes)
                     updateMaterialSwitch(session, "ladder_assign_left_to_right", value = config$ladder_assign_left_to_right)
                     updateMaterialSwitch(session, "spikeswitch", value = spikeswitch)
                     updateNumericInput(session, "spikelocation", value = config$ladder_start_scan)
                     updateNumericInput(session, "ladderselectionwindow", value = config$ladder_selection_window)
                     updateNumericInput(session, "maxcombinations", value = config$max_combinations)
                     updateMaterialSwitch(session, "minimum_peak_signal_ladder", value = minimum_peak_signal_ladder)
                     updateNumericInput(session, "minimum_peak_signal_number", value = config$minimum_ladder_signal)
                     updateMaterialSwitch(session, "scan_subset", value = scan_subset)
                     updateNumericInput(session, "scan_subset1", value = config$min_scan)
                     updateNumericInput(session, "scan_subset2", value = config$max_scan)
                     updateNumericInput(session, "ladder_top_n_branching", value = config$ladder_top_n_branching)
                     updateNumericInput(session, "ladderselectionwindow", value = config$ladderselectionwindow)

                     incProgress(7/10, detail = "Setting up peaks settings")

                     #Peaks
                     updateNumericInput(session, "min_bp_size", value = (config$min_bp_size - config$assay_size_without_repeat)/config$repeat_size)
                     updateNumericInput(session, "max_bp_size", value = (config$max_bp_size - config$assay_size_without_repeat)/config$repeat_size)
                     updateNumericInput(session, "smoothing_window", value = config$smoothing_window)
                     updateNumericInput(session, "minimum_peak_signal", value = config$minimum_peak_signal)
                     updateNumericInput(session, "peak_scan_ramp ", value = config$peak_scan_ramp )
                     updatePickerInput(session, "number_of_alleles", selected = config$number_of_alleles)
                     updatePickerInput(session, "batchcorrectionswitch", selected = config$correction)
                     updateNumericInput(session, "peak_region_size_gap_threshold", value = config$peak_region_size_gap_threshold)
                     updateNumericInput(session, "peak_region_signal_threshold_multiplier", value = config$peak_region_signal_threshold_multiplier)
                     updateNumericInput(session, "assay_size_without_repeat", value = config$assay_size_without_repeat)
                     updateRadioGroupButtons(session, "force_whole_repeat_units", selected = force_whole_repeat_units)
                     updatePickerInput(session,"force_repeat_pattern", selected = config$force_repeat_pattern)
                     updateNumericInput(session, "force_repeat_pattern_size_period", value = config$force_repeat_pattern_size_period)
                     updateNumericInput(session, "force_repeat_pattern_size_window", value = config$force_repeat_pattern_size_window)

                     incProgress(9/10, detail = "Setting up instability metrics settings")

                     #Investigate
                     updateNumericInput(session, "peak_threshold", value = peak_threshold)
                     updateNumericInput(session, "window_around_index_peak_min", value = window_around_index_peak_min)
                     updateNumericInput(session, "window_around_index_peak_max", value = window_around_index_peak_max)
                     updateNumericInput(session, "percentile_range1", value = percentile_range1)
                     updateNumericInput(session, "percentile_range2", value = percentile_range2)
                     updateNumericInput(session, "percentile_range3", value = percentile_range3)
                     updateNumericInput(session, "repeat_range1", value = repeat_range1)
                     updateNumericInput(session, "repeat_range2", value = repeat_range2)
                     updateNumericInput(session, "repeat_range3", value = repeat_range3)
                     updatePickerInput(session, "sample_subset2", choices = sample_subset2)
                     updatePickerInput(session,"sample_subset_metrics", choices = sample_subset_metrics)
                     updateMaterialSwitch(session, "group_controls", value = group_controls)

                     reactive_continue$laddertable <- laddertable
                     reactive_continue$fsa_list <- fsa_list
                     reactive_continue$metadata_table <- metadata_table
                     reactive_continue$instability_metrics <- instability_metrics
                     reactive_continue$ladders <- ladders
                     reactive_continue$index_list <- index_list
                     reactive_continue$scan <- scan
                     reactive_continue$size <- size
                     reactive_continue$sample_traces_size <- sample_traces_size
                     reactive_continue$sample_traces_repeats <- sample_traces_repeats
                     reactive_continue$number_of_alleles <- config$number_of_alleles
                     reactive_continue$batchcorrectionswitch <- config$correction
                     reactive_continue$sample_subset_metrics <- sample_subset_metrics
                     reactive_continue$sample_subset2 <- sample_subset2
                     reactive_continue$Index_Table <- Index_Table
                     reactive_continue$Index_Table_original <- Index_Table_original
                     reactive_continue$config <- config

                     if (!is.null(index_list) && !is.null(sample_subset_metrics)) {
                       updateNumericInput(session, "IndexRepeat1", value = Index_Table[which(Index_Table$`Unique IDs` == sample_subset_metrics[1]),]$`Index Repeat`)

                       if (!is.null(sample_subset2)) {
                         updateNumericInput(session, "IndexRepeat2", value = Index_Table[which(Index_Table$`Unique IDs` == sample_subset2[1]),]$`Index Repeat`)
                       }
                     }

                     shinyjs::show("LoadBox2")
                     shinyjs::show("LoadBox5")
                     shinyjs::show("LoadBox3")
                     shinyjs::show("LoadBox4")
                     shinyjs::hide("NextButtonLoad")
                     shinyjs::hide("NextButtonLoad2")
                     shinyjs::hide("LoadBox_FASTQ1")
                     shinyjs::hide("LoadBox_FASTQ2")

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = F),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                                      menuItem("Instability Metrics", icon = icon("water-ladder"), tabName = "InstabilityMetrics", selected = T),
                                                                      menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis", selected = F),
                                                                      menuItem("Documentation", icon = icon("file"), startExpanded = T,
                                                                               menuSubItem("Step 1: Upload", tabName = "Documentation1"),
                                                                               menuSubItem("Step 2: Find Ladders", tabName = "Documentation2"),
                                                                               menuSubItem("Step 3: Find Peaks", tabName = "Documentation3"),
                                                                               menuSubItem("Step 4: Instability Metrics", tabName = "Documentation4"),
                                                                               menuSubItem("Step 5: Analysis", tabName = "Documentation5"))))
                     }
                     if (exists("metadata_table_fastq")) {

                       #Load
                       if (input$LoadBoxIntro$collapsed == FALSE) {
                         js$collapse("LoadBoxIntro")
                       }

                       #Metrics
                       if(input$Metrics2BoxIntro$collapsed == FALSE) {
                         js$collapse("MetricsBoxIntro")
                       }
                       shinyjs::show("Metrics2Box1")
                       shinyjs::show("Metrics2Box2")
                       shinyjs::show("Metrics2Box3")

                       #Upload
                       #updateRadioGroupButtons(session, "DataUpload", selected = DataUpload)
                       updateMaterialSwitch(session, "DataUploadMeta", value = DataUploadMeta)

                       incProgress(9/10, detail = "Setting up instability metrics settings")

                       #Investigate
                       updateNumericInput(session, "peak_threshold2", value = peak_threshold2)
                       updateNumericInput(session, "window_around_index_peak_min", value = window_around_index_peak_min)
                       updateNumericInput(session, "window_around_index_peak_max", value = window_around_index_peak_max)
                       updateNumericInput(session, "percentile_range1", value = percentile_range1)
                       updateNumericInput(session, "percentile_range2", value = percentile_range2)
                       updateNumericInput(session, "percentile_range3", value = percentile_range3)
                       updateNumericInput(session, "repeat_range1", value = repeat_range1)
                       updateNumericInput(session, "repeat_range2", value = repeat_range2)
                       updateNumericInput(session, "repeat_range3", value = repeat_range3)
                       updatePickerInput(session, "sample_subset2_2", choices = sample_subset2_2)
                       updatePickerInput(session,"sample_subset_metrics2", choices = sample_subset_metrics2)
                       updateMaterialSwitch(session, "group_controls2", value = group_controls2)

                       reactive_continue$fastq <- fastq
                       reactive_continue$df <- All
                       reactive_continue$metadata_table_fastq <- metadata_table_fastq
                       reactive_continue$peak_list <- peak_list
                       reactive_continue$instability_metrics_fastq <- instability_metrics_fastq
                       reactive_continue$sample_subset_metrics2 <- sample_subset_metrics2
                       reactive_continue$sample_subset2_2 <- sample_subset2_2
                       reactive_continue$Index_Table <- Index_Table
                       reactive_continue$Index_Table_original <- Index_Table_original

                       if (!is.null(fastq) && !is.null(sample_subset_metrics2)) {
                         updateNumericInput(session, "IndexRepeat1_2", value = Index_Table[which(Index_Table$`Unique IDs` == sample_subset_metrics2[1]),]$`Index Repeat`)

                         if (!is.null(sample_subset2_2)) {
                           updateNumericInput(session, "IndexRepeat2_2", value = Index_Table[which(Index_Table$`Unique IDs` == sample_subset2_2[1]),]$`Index Repeat`)
                         }
                       }

                       shinyjs::show("LoadBox2")
                       shinyjs::hide("LoadBox5")
                       shinyjs::show("LoadBox3")
                       shinyjs::hide("LoadBox4")
                       shinyjs::hide("NextButtonLoad")
                       shinyjs::hide("NextButtonLoad2")
                       shinyjs::show("LoadBox_FASTQ1")
                       shinyjs::show("LoadBox_FASTQ2")

                       output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                        menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = F),
                                                                        menuItem("Instability Metrics", icon = icon("water-ladder"), tabName = "InstabilityMetrics2", selected = T),
                                                                        menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis2", selected = F),
                                                                        menuItem("Documentation", icon = icon("file"), startExpanded = T,
                                                                                 menuSubItem("Step 1: Upload", tabName = "Documentation1"),
                                                                                 menuSubItem("Step 2: Find Ladders", tabName = "Documentation2"),
                                                                                 menuSubItem("Step 3: Find Peaks", tabName = "Documentation3"),
                                                                                 menuSubItem("Step 4: Instability Metrics", tabName = "Documentation4"),
                                                                                 menuSubItem("Step 5: Analysis", tabName = "Documentation5"))))
                     }

                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
    })
  })

  return(list(
    config = reactive(reactive_continue$config),
    laddertable = reactive(reactive_continue$laddertable),
    fsa_list = reactive(reactive_continue$fsa_list),
    metadata_table = reactive(reactive_continue$metadata_table),
    fastq = reactive(reactive_continue$fastq),
    All = reactive(reactive_continue$df),
    peak_list = reactive(reactive_continue$peak_list),
    metadata_table_fastq = reactive(reactive_continue$metadata_table_fastq),
    ladders = reactive(reactive_continue$ladders),
    scan = reactive(reactive_continue$scan),
    size = reactive(reactive_continue$size),
    index_list = reactive(reactive_continue$index_list),
    instability_metrics = reactive(reactive_continue$instability_metrics),
    sample_subset_metrics = reactive(reactive_continue$sample_subset_metrics),
    sample_subset2 = reactive(reactive_continue$sample_subset2),
    Index_Table = reactive(reactive_continue$Index_Table),
    Index_Table_original = reactive(reactive_continue$Index_Table_original),
    sample_traces_size = reactive(reactive_continue$sample_traces_size),
    sample_traces_repeats = reactive(reactive_continue$sample_traces_repeats),
    number_of_alleles = reactive(reactive_continue$number_of_alleles),
    batchcorrectionswitch = reactive(reactive_continue$batchcorrectionswitch),
    instability_metrics_fastq = reactive(reactive_continue$instability_metrics_fastq),
    sample_subset_metrics2 = reactive(reactive_continue$sample_subset_metrics2),
    sample_subset2_2 = reactive(reactive_continue$sample_subset2_2)
  ))

}
