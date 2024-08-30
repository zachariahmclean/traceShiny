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
    #tryCatch({
      withProgress(message = 'Loading previously saved enviroment...', style = "old",
                   value = 0, {
                     incProgress(1/20, detail = "Loading Packages")

                     file = input$fileinputLOAD
                     if (is.null(file)) {
                       return(NULL)
                     }

                     library(instability)

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
                     reactive_continue$laddertable <- laddertable
                     reactive_continue$fsa_list <- fsa_list
                     reactive_continue$metadata_table <- metadata_table
                     reactive_continue$instability_metrics <- instability_metrics
                     reactive_continue$ladders <- ladders
                     reactive_continue$index_list <- index_list
                     reactive_continue$scan <- scan
                     reactive_continue$size <- size

                     updateRadioGroupButtons(session, "DataUpload", selected = DataUpload)
                     updateMaterialSwitch(session, "DataUploadMeta", value = DataUploadMeta)
                     updateMaterialSwitch(session, "Ladder_switch", value = Ladder_switch)

                     incProgress(6/10, detail = "Setting up ladder settings")

                     #Ladders
                     updatePickerInput(session, "LadderChannel", selected = LadderChannel)
                     updatePickerInput(session, "SignalChannel", selected = SignalChannel)
                     updatePickerInput(session, "LadderSizes", selected = LadderSizes)
                     updateMaterialSwitch(session, "spikeswitch", value = spikeswitch)
                     updateNumericInput(session, "spikelocation", value = spikelocation)
                     updateMaterialSwitch(session, "zerofloor", value = zerofloor)
                     updateNumericInput(session, "ladderselectionwindow", value = ladderselectionwindow)
                     updateNumericInput(session, "smoothingwindow", value = smoothingwindow)
                     updateNumericInput(session, "maxcombinations", value = maxcombinations)

                     incProgress(7/10, detail = "Setting up peaks settings")

                     #Peaks
                     updateNumericInput(session, "min_bp_size", value = min_bp_size)
                     updateNumericInput(session, "max_bp_size", value = max_bp_size)
                     updateNumericInput(session, "smoothing_window", value = smoothing_window_peaks)
                     updateNumericInput(session, "minimum_peak_signal", value = minimum_peak_signal)
                     updateRadioGroupButtons(session, "number_of_peaks_to_return", selected = number_of_peaks_to_return)
                     updateNumericInput(session, "peak_region_size_gap_threshold", value = peak_region_size_gap_threshold)
                     updateNumericInput(session, "peak_region_height_threshold_multiplier", value = peak_region_height_threshold_multiplier)
                     updateNumericInput(session, "assay_size_without_repeat", value = assay_size_without_repeat)
                     updateRadioGroupButtons(session, "force_whole_repeat_units", selected = force_whole_repeat_units)
                     updatePickerInput(session, "repeat_calling_algorithm", selected = repeat_calling_algorithm)
                     updateNumericInput(session, "repeat_calling_algorithm_size_window_around_allele", value = repeat_calling_algorithm_size_window_around_allele)
                     updateNumericInput(session, "repeat_calling_algorithm_size_period", value = repeat_calling_algorithm_size_period)
                     updateNumericInput(session, "repeat_calling_algorithm_peak_assignment_scan_window", value = repeat_calling_algorithm_peak_assignment_scan_window)

                     incProgress(9/10, detail = "Setting up instability metrics settings")

                     #Investigate
                     updateNumericInput(session, "peak_threshold", value = peak_threshold)
                     updateNumericInput(session, "window_around_index_peak_min", value = window_around_index_peak_min)
                     updateNumericInput(session, "window_around_index_peak_max", value = window_around_index_peak_max)

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = F),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                                      menuItem("Instability Metrics", icon = icon("water-ladder"), tabName = "InstabilityMetrics", selected = T)))

                   })
    # },
    # error = function(e) {
    #   shinyalert("ERROR!", "File is not in correct format.", type = "error", confirmButtonCol = "#337ab7")
    # })
  })

  return(list(
    laddertable = reactive(reactive_continue$laddertable),
    fsa_list = reactive(reactive_continue$fsa_list),
    metadata_table = reactive(reactive_continue$metadata_table),
    ladders = reactive(reactive_continue$ladders),
    scan = reactive(reactive_continue$scan),
    size = reactive(reactive_continue$size),
    index_list = reactive(reactive_continue$index_list),
    instability_metrics = reactive(reactive_continue$instability_metrics)
  ))

}
