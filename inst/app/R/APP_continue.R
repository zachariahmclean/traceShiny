continue_server <- function(input, output, session) {

  reactive_continue <- reactiveValues(laddertable = NULL,
                                      fsa_list = NULL,
                                      metadata_table = NULL,
                                      ladders = NULL,
                                      instability_metrics = NULL,
                                      index_list = NULL)

  observeEvent(input$continue, {
    showModal(modalDialog(
      title = strong("CONTINUE"),

      fileInput("fileinputLOAD", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Load previously saved enviroment (.RData file)')),
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

                     if (!is.null(reactive_continue$fsa_list)) {
                       shinyalert("ERROR!", "You already have a file loaded, please click the refresh button on the top to delete all data and start from fresh.", type = "error", confirmButtonCol = "#337ab7")
                     }
                     else {

                     library(trace)

                     removeModal()

                     incProgress(3/10, detail = "Setting up previously saved variables")

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

                     load(file$datapath)

                     incProgress(5/10, detail = "Setting up upload settings")

                     #Upload
                     updateRadioGroupButtons(session, "DataUpload", selected = DataUpload)
                     updateMaterialSwitch(session, "DataUploadMeta", value = DataUploadMeta)
                     updateMaterialSwitch(session, "Ladder_switch", value = Ladder_switch)

                     incProgress(6/10, detail = "Setting up ladder settings")

                     #Ladders
                     updatePickerInput(session,"LadderChannel", selected = LadderChannel)
                     updatePickerInput(session,"SignalChannel", selected = SignalChannel)
                     updatePickerInput(session,"LadderSizes", selected = LadderSizes)
                     updateMaterialSwitch(session, "spikeswitch", value = spikeswitch)
                     updateNumericInput(session, "spikelocation", value = spikelocation)
                     updateMaterialSwitch(session, "zerofloor", value = zerofloor)
                     updateNumericInput(session, "ladderselectionwindow", value = ladderselectionwindow)
                     updateNumericInput(session, "smoothingwindow", value = smoothingwindow)
                     updateNumericInput(session, "maxcombinations", value = maxcombinations)
                     updateMaterialSwitch(session, "minimum_peak_signal_ladder", value = minimum_peak_signal_ladder)
                     updateNumericInput(session, "minimum_peak_signal_number", value = minimum_peak_signal_number)
                     updateMaterialSwitch(session, "scan_subset", value = scan_subset)
                     updateNumericInput(session, "scan_subset1", value = scan_subset1)
                     updateNumericInput(session, "scan_subset2", value = scan_subset2)

                     incProgress(7/10, detail = "Setting up peaks settings")

                     #Peaks
                     updateNumericInput(session, "min_bp_size", value = min_bp_size)
                     updateNumericInput(session, "max_bp_size", value = max_bp_size)
                     updateNumericInput(session, "smoothing_window", value = smoothing_window_peaks)
                     updateNumericInput(session, "minimum_peak_signal", value = minimum_peak_signal)
                     updatePickerInput(session, "batchcorrectionswitch", selected = batchcorrectionswitch)
                     updateNumericInput(session, "peak_region_size_gap_threshold", value = peak_region_size_gap_threshold)
                     updateNumericInput(session, "peak_region_height_threshold_multiplier", value = peak_region_height_threshold_multiplier)
                     updateNumericInput(session, "assay_size_without_repeat", value = assay_size_without_repeat)
                     updateRadioGroupButtons(session, "force_whole_repeat_units", selected = force_whole_repeat_units)
                     updatePickerInput(session,"repeat_calling_algorithm", selected = repeat_calling_algorithm)
                     updateNumericInput(session, "repeat_calling_algorithm_size_window_around_allele", value = repeat_calling_algorithm_size_window_around_allele)
                     updateNumericInput(session, "repeat_calling_algorithm_size_period", value = repeat_calling_algorithm_size_period)
                     updateNumericInput(session, "repeat_calling_algorithm_peak_assignment_scan_window", value = repeat_calling_algorithm_peak_assignment_scan_window)

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
                     reactive_continue$batchcorrectionswitch <- batchcorrectionswitch
                     reactive_continue$sample_subset_metrics <- sample_subset_metrics
                     reactive_continue$sample_subset2 <- sample_subset2
                     reactive_continue$Index_Table <- Index_Table
                     reactive_continue$Index_Table_original <- Index_Table_original

                     if (!is.null(index_list) && !is.null(sample_subset_metrics)) {
                       updateNumericInput(session, "IndexRepeat1", value = Index_Table[1,]$`Index Repeat`)

                       if (!is.null(sample_subset2)) {
                         updateNumericInput(session, "IndexRepeat2", value = Index_Table[1,]$`Index Repeat`)
                       }
                     }

                     shinyjs::show("LoadBox2")
                     shinyjs::show("LoadBox5")
                     shinyjs::show("LoadBox3")
                     shinyjs::show("LoadBox4")

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = F),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                                      menuItem("Instability Metrics", icon = icon("water-ladder"), tabName = "InstabilityMetrics", selected = T),
                                                                      menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis", selected = F)))
                     }

                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "File is not in correct format.", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  return(list(
    laddertable = reactive(reactive_continue$laddertable),
    fsa_list = reactive(reactive_continue$fsa_list),
    metadata_table = reactive(reactive_continue$metadata_table),
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
    batchcorrectionswitch = reactive(reactive_continue$batchcorrectionswitch)
  ))

}
