metrics_box_ui1 <- function(id) {
  box(id = "MetricsBoxIntro", title = strong("Calculate Instability Metrics"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,
      h4(includeHTML("data/metrics/metrics_landing_page.html")),
      br(), br(),

      fluidRow(column(3,
                      valueBox("NEW", actionBttn("MetricsBoxSTART", "START",
                                                 style = "jelly",
                                                 color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

metrics_box_ui2 <- function(id) {
  box(id = "MetricsBox1", title = p("Settings", help_button("metrics_params")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      materialSwitch("advancesettings_Metrics", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Advanced Settings')), value = FALSE, status = "primary"),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Calculate Repeat Instability Metrics</b>')),
               fluidRow(
                 column(6,
                        numericInput("peak_threshold", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Threshold')),
                                     min = 0.01,
                                     value = 0.05, step = 0.01))
               ),
               fluidRow(
                 column(6,
                        numericInput("window_around_index_peak_min", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Window')),
                                     value = -5, step = 1)),
                 column(6,
                        numericInput("window_around_index_peak_max", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Maximum Window')),
                                     value = 40, step = 1))
               )
        )
      ),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Index Repeat Table</b>')),
               withSpinner(DT::dataTableOutput("Index_Table"))
               )
      ),
      conditionalPanel(
        condition = 'input.advancesettings_Metrics == true',
      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Instability Metrics Table Output</b><br>')),
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><br>Percentile Range')),
               fluidRow(
                 column(4,
                        numericInput("percentile_range1", label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">From')),
                                     value = 0.5,
                                     min = 0,
                                     step = 0.1)),
                 column(4,
                        numericInput("percentile_range2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">To')),
                                     min = 0,
                                     value = 0.95, step = 0.1)),
                 column(4,
                        numericInput("percentile_range3", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Interval')),
                                     min = 0,
                                     value = 0.05, step = 0.1))
               ),
               h4(HTML('<h4 style = "text-align:justify;color:#000000">Repeat Range')),
               fluidRow(
                 column(4,
                        numericInput("repeat_range1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">From')),
                                     min = 0,
                                     value = 0, step = 1)),
                 column(4,
                        numericInput("repeat_range2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">To')),
                                     min = 0,
                                     value = 20, step = 1)),
                 column(4,
                        numericInput("repeat_range3", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Interval')),
                                     min = 0,
                                     value = 5, step = 1))
               ),
        )
      )
      ),
      p(style="text-align: center;", actionBttn("startbuttonMetrics", "APPLY", size = "lg"))
  )
}

metrics_box_ui3 <- function(id) {
  box(id = "MetricsBox2", title = p("Instability Metrics Table"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      withSpinner(DT::dataTableOutput("metrics_table", width = "100%", height = "400"))
  )
}

metrics_box_ui4 <- function(id) {
  box(id = "MetricsBox3", title = p("Traces"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(3,
               pickerInput("sample_subset_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = NULL)),
        column(3,
               radioGroupButtons(
                 inputId = "show_peaks_metrics",
                 label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Show Peaks')),
                 choices = c("YES",
                             "NO"),
                 justified = TRUE,
                 selected = "YES"
               )
        ),
        column(6,
               sliderInput("HeightPeaks_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )),
      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Plot Controls</b>')),
               fluidRow(
                 column(1,
                        numericInput("xlim1_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                                     value = 0)),

                 column(1,
                        numericInput("xlim2_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                                     value = 250)),
                 column(1,
                        numericInput("ylim1_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                                     value = 0)),

                 column(1,
                        numericInput("ylim2_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                                     value = 2000))))),

      fluidRow(
        column(6,
               numericInput("IndexRepeat1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
                            value = NULL)
        )
      ),

      htmlOutput("text_no_data2"),
      htmlOutput("plot_traces_final_UI"),

      fluidRow(
        column(3,
               pickerInput("sample_subset2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Index Samples')),
                           choices = NULL))
      ),

      fluidRow(
        column(6,
               numericInput("IndexRepeat2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
                            value = NULL)
        )
      ),
      htmlOutput("plot_traces_INDEX_UI")
  )
}

metrics_box_ui5 <- function(id) {
  box(id = "MetricsBox4", title = p("Export Data"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      downloadBttn("downloadRDS", "Download R Object For Further Analysis"), br(), br(),
      downloadBttn("downloadmetrics", "Download Instability Metrics Table"), br(), br(),
      actionBttn("downloadPlotButton", "Download All Plots", icon = icon("download")), br(), br(),
      downloadBttn("downloadlogs", "Download Code For Current Analysis"), br(), br(),
      downloadBttn("downloadDataSave", "SAVE OBJECT TO LOAD INTO APP")
  )
}


metrics_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module) {

  # help files
  help_click("metrics_params", helpfile = "data/metrics/metrics_params.html")

  reactive_metrics <- reactiveValues()

  observeEvent(input$fileinputLOAD, {
    reactive_metrics$df <- continue_module$instability_metrics()
    reactive_metrics$sample_subset_metrics <- continue_module$sample_subset_metrics()
    reactive_metrics$sample_subset2 <- continue_module$sample_subset2()
    reactive_metrics$Index_Table <- continue_module$Index_Table()
    reactive_metrics$Index_Table2 <- continue_module$Index_Table2()
  })

  output$downloadlogs <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_logs", ".html")
    },
    content = function(file) {
      strLoad <- paste("<h4>Data Upload </h4>",
                       "fsa_raw <- read_fsa('/path/to/fsa_files.fsa') <br>
                 metadata_table <- read.csv('/path/to/metadata_table.txt')")

      strLadders <- ifelse (input$spikeswitch == T,
                            paste("<h4>Find Ladders </h4>",
                                  "ladder_list <- find_ladders(fsa_raw, ladder_channel = ", paste(input$LadderChannel), ", ",
                                  "signal_channel = ", paste(input$SignalChannel), ", ",
                                  "ladder_sizes = ", paste(as.numeric(strsplit(input$LadderSizes, split = ",")[[1]])), ", ",
                                  "spike location = NULL, ",
                                  "zero_floor = ", paste(input$zerofloor), ", ",
                                  "ladder_selection_window = ", paste(input$ladderselectionwindow), ", ",
                                  "smoothing_window = ", paste(input$smoothingwindow), ", ",
                                  "max_combinations = ", paste(input$maxcombinations), ")"),
                            paste("<h4>Find Ladders </h4>",
                                  "ladder_list <- find_ladders(fsa_raw, ladder_channel = ", paste(input$LadderChannel), ", ",
                                  "signal_channel = ", paste(input$SignalChannel), ", ",
                                  "ladder_sizes = ", paste(as.numeric(strsplit(input$LadderSizes, split = ",")[[1]])), ", ",
                                  "spike location = ", paste(input$spikelocation), ", ",
                                  "zero_floor = ", paste(input$zerofloor), ", ",
                                  "ladder_selection_window = ", paste(input$ladderselectionwindow), ", ",
                                  "smoothing_window = ", paste(input$smoothingwindow), ", ",
                                  "max_combinations = ", paste(input$maxcombinations), ")")
      )

      strPeaks <- paste("<h4>Find Peaks </h4>",
                        "peak_list <- find_fragments(ladder_list, ",
                        "smoothing_window = ", paste(input$smoothing_window), ", ",
                        "minimum_peak_signal = ", paste(input$minimum_peak_signal), ", ",
                        "min_bp_size = ", paste(input$min_bp_size), ", ",
                        "max_bp_size = ", paste(input$max_bp_size), ")")

      strAddMeta <- paste("<h4>Add Metadata </h4>",
                          "metadata_added_list <- add_metadata(peak_list, ",
                          "metadata_data.frame = metadata_table, ",
                          "unique_id = 'unique_id'",
                          "metrics_baseline_control = 'metrics_baseline_control')")

      strAlleles <- paste("<h4>Find Alleles </h4>",
                          "allele_list <- find_alleles(metadata_added_list, ",
                          "peak_region_size_gap_threshold = ", paste(input$peak_region_size_gap_threshold), ", ",
                          "peak_region_height_threshold_multiplier = ", paste(input$peak_region_height_threshold_multiplier), ")")

      strRepeats <- ifelse (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control)),
                            paste("<h4>Find Repeats </h4>",
                                  "repeats_list <- call_repeats(allele_list, assay_size_without_repeat = ", paste(input$assay_size_without_repeat), ", ",
                                  "repeat_size = ", paste(input$repeat_size), ", ",
                                  "repeat_calling_algorithm = ", paste(input$repeat_calling_algorithm), ", ",
                                  "repeat_calling_algorithm_peak_assignment_scan_window = ", paste(input$repeat_calling_algorithm_peak_assignment_scan_window), ", ",
                                  "repeat_calling_algorithm_size_window_around_allele = ", paste(input$repeat_calling_algorithm_size_window_around_allele), ", ",
                                  "repeat_calling_algorithm_size_period = ", paste(input$repeat_calling_algorithm_size_period), ", ",
                                  "force_whole_repeat_units = ", paste(ifelse(input$force_whole_repeat_units == "YES", "TRUE", "FALSE")), ", ",
                                  "repeat_length_correction = 'from_metadata')"),
                            paste("<h4>Find Repeats </h4>",
                                  "repeats_list <- call_repeats(allele_list, assay_size_without_repeat = ", paste(input$assay_size_without_repeat), ", ",
                                  "repeat_size = ", paste(input$repeat_size), ", ",
                                  "repeat_calling_algorithm = ", paste(input$repeat_calling_algorithm), ", ",
                                  "repeat_calling_algorithm_peak_assignment_scan_window = ", paste(input$repeat_calling_algorithm_peak_assignment_scan_window), ", ",
                                  "repeat_calling_algorithm_size_window_around_allele = ", paste(input$repeat_calling_algorithm_size_window_around_allele), ", ",
                                  "repeat_calling_algorithm_size_period = ", paste(input$repeat_calling_algorithm_size_period), ", ",
                                  "force_whole_repeat_units = ", paste(ifelse(input$force_whole_repeat_units == "YES", "TRUE", "FALSE")), ", ",
                                  "repeat_length_correction = 'none')")
      )

      strIndex <- ifelse (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control)),
                            paste("<h4>Find Index </h4>",
                                  "index_list <- assign_index_peaks(repeats_list, grouped = TRUE)"),
                          paste("<h4>Find Index </h4>",
                                "index_list <- assign_index_peaks(repeats_list, grouped = FALSE)")
      )

      strMetrics <- paste("<h4>Calculate Instability Metrics </h4>",
                        "metrics_dataframe <- calculate_instability_metrics(index_list, ",
                        "peak_threshold = ", paste(input$peak_threshold), ", ",
                        "window_around_index_peak = c(", paste(input$window_around_index_peak_min), ", ", paste(input$window_around_index_peak_max), "))")

      code <- c(paste(strLoad, strLadders, strPeaks, strAddMeta, strAlleles, strRepeats, strIndex, strMetrics, sep = '<br>'))

      writeLines(text = code, file)
    }
  )

  #Downloads
  output$downloadmetrics <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_InstabilityMetricsTable", ".csv")
    },
    content = function(file) {
      write.csv(reactive_metrics$df, file, row.names = F, col.names = T)
    }
  )

  output$downloadRDS <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_RObjects", ".rds")
    },
    content = function(file) {
      saveRDS(peaks_module$index_list(), file)
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      "Instability_Plots.pdf"
    },

    content = function(file) {

      pdf(file,
          width = input$downloadPlotWidth,
          height = input$downloadPlotHeight)

      if (input$show_peaks_metrics == "YES") {
      for (i in 1:length(names(peaks_module$index_list()))) {
        trace::plot_traces(peaks_module$index_list()[i],
                    show_peaks = T,
                    xlim = c(input$xlim1_metrics, input$xlim2_metrics),
                    ylim = c(input$ylim1_metrics, input$ylim2_metrics))
      }
      }
      else {
        for (i in 1:length(names(peaks_module$index_list()))) {
          trace::plot_traces(peaks_module$index_list()[i],
                      show_peaks = F,
                      xlim = c(input$xlim1_metrics, input$xlim2_metrics),
                      ylim = c(input$ylim1_metrics, input$ylim2_metrics))
        }
      }

      dev.off()
    }
  )

  observeEvent(input$downloadPlotButton, {
    showModal(modalDialog(
      title = strong("Download Plots"),
      numericInput("downloadPlotHeight", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Height of plot')),
                   value = 7,
                   min = 0,
                   max = 20),
      numericInput("downloadPlotWidth", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Width of plot')),
                   value = 7,
                   min = 0,
                   max = 20),
      downloadBttn("downloadPlot", "Download"),
      size = "s",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$MetricsBoxSTART, {
    shinyjs::hide("NextButtonPeaks")
    reactive_metrics$df <- NULL

    if(input$MetricsBoxIntro$collapsed == FALSE) {
      js$collapse("MetricsBoxIntro")
    }
    shinyjs::show("MetricsBox1")
    shinyjs::show("MetricsBox2")
    shinyjs::show("MetricsBox3")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T)))

    if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
      }
    }

    if (is.null(upload_data$metadata_table())) {
      reactive_metrics$sample_subset_metrics <- names(upload_data$fsa_list())
    }
    else if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        reactive_metrics$sample_subset_metrics <- upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id
        reactive_metrics$sample_subset2 <- upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id
      }
      else {
        reactive_metrics$sample_subset_metrics <- upload_data$metadata_table()$unique_id
      }
    }

    index <- list()

    for (i in 1:length(peaks_module$index_list())) {
      index[[i]] <- as.data.frame(cbind(peaks_module$index_list()[[i]]$unique_id, peaks_module$index_list()[[i]]$get_index_peak()$index_repeat))
    }

    reactive_metrics$Index_Table <- do.call(rbind, index)
    colnames(reactive_metrics$Index_Table) <- c("Unique IDs", "Index Repeat")

    reactive_metrics$Index_Table2 <- "no"

  })

  observe({
    if (!is.null(continue_module$Index_Table()) && is.null(reactive_metrics$Index_Table)) {

      reactive_metrics$Index_Table <- continue_module$Index_Table()
    }
  })

  observeEvent(input$NextButtonPeaks, {

    reactive_metrics$df <- NULL

    shinyjs::hide("NextButtonLadder")

    if(input$MetricsBoxIntro$collapsed == TRUE) {
      js$collapse("MetricsBoxIntro")
    }
    shinyjs::hide("MetricsBox1")
    shinyjs::hide("MetricsBox2")
    shinyjs::hide("MetricsBox3")
    shinyjs::hide("MetricsBox4")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T,
                                                              badgeColor = "green", badgeLabel = "new")))
  })

  observe({
    if (!is.null(peaks_module$index_list()) && !is.null(input$sample_subset_metrics)) {
      updateNumericInput(session, "IndexRepeat1", value = reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat`)

      if (!is.null(input$sample_subset2)) {
        updateNumericInput(session, "IndexRepeat2", value = reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset2),]$`Index Repeat`)
      }
    }
  })

  observeEvent(input$sample_subset_metrics, {
    if (!is.null(input$sample_subset2)) {
      if (!is.null(upload_data$metadata_table())) {
        if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
          updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
        }
      }
    }
  })

  observe({
    if (is.null(upload_data$metadata_table())) {
      shinyjs::hide("sample_subset2")
      shinyjs::show("IndexRepeat1")
      shinyjs::hide("IndexRepeat2")
      shinyjs::hide("plot_traces_INDEX_UI")
    }
    else if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        shinyjs::hide("IndexRepeat1")
        shinyjs::show("IndexRepeat2")
        shinyjs::show("sample_subset2")
        shinyjs::show("plot_traces_INDEX_UI")
      }
      else {
        shinyjs::show("IndexRepeat1")
        shinyjs::hide("IndexRepeat2")
        shinyjs::hide("sample_subset2")
        shinyjs::hide("plot_traces_INDEX_UI")
      }
    }
  })

  observe({
    if (!is.null(reactive_metrics$df)) {
      shinyjs::show("MetricsBox4")
    }
    else {
      shinyjs::hide("MetricsBox4")
    }
  })

  observeEvent(input$startbuttonMetrics, {
    tryCatch({
      withProgress(message = 'Calculating Instability Metrics ...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                       assign_index_peaks(
                         peaks_module$index_list(),
                         grouped = if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) TRUE else FALSE,
                         index_override_dataframe = reactive_metrics$Index_Table
                       )

                     reactive_metrics$df <- calculate_instability_metrics(
                       fragments_list = peaks_module$index_list(),
                       peak_threshold = input$peak_threshold,
                       window_around_index_peak = c(input$window_around_index_peak_min, input$window_around_index_peak_max),
                       percentile_range = seq(input$percentile_range1, input$percentile_range2, input$percentile_range3),
                       repeat_range = seq(input$repeat_range1 , input$repeat_range2, input$repeat_range3)
                     )

                     reactive_metrics$Index_Table2 <- "no"
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "Analysis Failed, please check if the previous steps were completed successfully.", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$IndexRepeat1, {
    if (!is.null(reactive_metrics$Index_Table)) {
    reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat` <- input$IndexRepeat1
    reactive_metrics$Index_Table2 <- "yes"
    }
  })

  observeEvent(list(input$IndexRepeat2, input$sample_subset_metrics),  {
    if (!is.null(ladder_module$ladders())) {
    if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
      if (!is.null(reactive_metrics$Index_Table)) {
        reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat` <- input$IndexRepeat2
        reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset2),]$`Index Repeat` <- input$IndexRepeat2
        reactive_metrics$Index_Table2 <- "yes"
      }
    }
    }
  })

  output$Index_Table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_metrics$Index_Table), 'You must perform the analysis first...'))

    as.data.frame(reactive_metrics$Index_Table)

  },  options = list(scrollX = TRUE))

  output$metrics_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_metrics$df), 'You must perform the analysis first...'))
    validate(
      need(reactive_metrics$Index_Table2 == "no", 'Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.')
    )
    reactive_metrics$df
  },  options = list(scrollX = TRUE))

  output$plot_traces_final_UI <- renderUI({
    plotlyOutput("plot_traces_final", height = (300 + input$HeightPeaks_metrics*20))
  })

  output$plot_traces_final <- renderPlotly({

    if (is.null(peaks_module$index_list())) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$show_peaks_metrics == "YES") {
      show_peaks_metrics = TRUE
    }
    else {
      show_peaks_metrics = FALSE
    }

    xlim = c(input$xlim1_metrics, input$xlim2_metrics)
    ylim = c(input$ylim1_metrics, input$ylim2_metrics)
    height_color_threshold = 0.05
    plot_title = NULL

    #there must be a simpler way of the following if else below
      data <- peaks_module$index_list()[[input$sample_subset_metrics]]$trace_bp_df
      data$x <- data$calculated_repeats

    if (!is.null(xlim)) {
      data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
    }

    if (show_peaks_metrics == TRUE) {
      # add points onto plot showing peaks
          peak_table <- peaks_module$index_list()[[input$sample_subset_metrics]]$repeat_table_df
          peak_table$x <- peak_table$repeats

        if (!is.null(xlim)) {
          peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
        }

        tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
        tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
        if (!is.null(peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height) && !is.na(peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height)) {
          tallest_peak_height <- peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height
          tallest_peak_x <- peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_repeat
        }

        peaks_above <- peak_table[which(peak_table$height > tallest_peak_height * height_color_threshold), ]
        peaks_below <- peak_table[which(peak_table$height < tallest_peak_height * height_color_threshold), ]

    }

    if (is.null(upload_data$metadata_table())) {
      if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
        if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
          plot_ly(data = data,
                  x = ~x, y = ~signal,
                  type = "scatter",
                  mode = "lines",
                  height = (300 + input$HeightPeaks_metrics*20)) %>%
            add_markers(x = peaks_above$x,
                        y = peaks_above$height,
                        colors = "blue") %>%
            # add_markers(x = peaks_below$x,
            #             y = peaks_below$height,
            #             colors = "purple") %>%
            add_markers(x = tallest_peak_x,
                        y = tallest_peak_height,
                        colors = "green") %>%
            add_segments(x = peak_table$repeats,
                         y = peak_table$height,
                         xend = peak_table$calculated_repeats,
                         yend = peak_table$height,
                         line = list(dash = "dash")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list("Repeats",
                                range = xlim),
                   yaxis = list("Signal",
                                range = ylim),
                   shapes = list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
                     #vertical line
                     list(type = "line", x0 = input$IndexRepeat1,
                          x1 = input$IndexRepeat1,
                          y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
                     list(type = "rect",
                          fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                          y0 = 0, y1 = input$ylim2_metrics,
                          x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
                          x1 = input$window_around_index_peak_max + input$IndexRepeat1))
            )
        }
      }
      else {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                height = (300 + input$HeightPeaks_metrics*20)) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
                               #vertical line
                               list(type = "line", x0 = input$IndexRepeat1,
                                    x1 = input$IndexRepeat1,
                                    y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
                               list(type = "rect",
                                    fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                                    y0 = 0, y1 = input$ylim2_metrics,
                                    x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
                                    x1 = input$window_around_index_peak_max + input$IndexRepeat1))
          )
      }
    }

    else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {

      if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
        if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
          plot_ly(data = data,
                  x = ~x, y = ~signal,
                  type = "scatter",
                  mode = "lines",
                  height = (300 + input$HeightPeaks_metrics*20)) %>%
            add_markers(x = peaks_above$x,
                        y = peaks_above$height,
                        colors = "blue") %>%
            # add_markers(x = peaks_below$x,
            #             y = peaks_below$height,
            #             colors = "purple") %>%
            add_markers(x = tallest_peak_x,
                        y = tallest_peak_height,
                        colors = "green") %>%
            add_segments(x = peak_table$repeats,
                         y = peak_table$height,
                         xend = peak_table$calculated_repeats,
                         yend = peak_table$height,
                         line = list(dash = "dash")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list("Repeats",
                                range = xlim),
                   yaxis = list("Signal",
                                range = ylim),
                   shapes = list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
                     #vertical line
                     list(type = "line", x0 = input$IndexRepeat2,
                          x1 = input$IndexRepeat2,
                          y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
                     list(type = "rect",
                          fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                          y0 = 0, y1 = input$ylim2_metrics,
                          x0 = input$IndexRepeat2 + input$window_around_index_peak_min,
                          x1 = input$window_around_index_peak_max + input$IndexRepeat2))
            )
        }
      }
      else {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                height = (300 + input$HeightPeaks_metrics*20)) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
                   #vertical line
                   list(type = "line", x0 = input$IndexRepeat2,
                        x1 = input$IndexRepeat2,
                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
                   list(type = "rect",
                        fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                        y0 = 0, y1 = input$ylim2_metrics,
                        x0 = input$IndexRepeat2 + input$window_around_index_peak_min,
                        x1 = input$window_around_index_peak_max + input$IndexRepeat2))
          )
      }
    }
    else {
      if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
        if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
          plot_ly(data = data,
                  x = ~x, y = ~signal,
                  type = "scatter",
                  mode = "lines",
                  height = (300 + input$HeightPeaks_metrics*20)) %>%
            add_markers(x = peaks_above$x,
                        y = peaks_above$height,
                        colors = "blue") %>%
            # add_markers(x = peaks_below$x,
            #             y = peaks_below$height,
            #             colors = "purple") %>%
            add_markers(x = tallest_peak_x,
                        y = tallest_peak_height,
                        colors = "green") %>%
            add_segments(x = peak_table$repeats,
                         y = peak_table$height,
                         xend = peak_table$calculated_repeats,
                         yend = peak_table$height,
                         line = list(dash = "dash")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list("Repeats",
                                range = xlim),
                   yaxis = list("Signal",
                                range = ylim),
                   shapes = list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
                     #vertical line
                     list(type = "line", x0 = input$IndexRepeat1,
                          x1 = input$IndexRepeat1,
                          y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
                     list(type = "rect",
                          fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                          y0 = 0, y1 = input$ylim2_metrics,
                          x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
                          x1 = input$window_around_index_peak_max + input$IndexRepeat1))
            )
        }
      }
      else {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                height = (300 + input$HeightPeaks_metrics*20)) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
                   #vertical line
                   list(type = "line", x0 = input$IndexRepeat1,
                        x1 = input$IndexRepeat1,
                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
                   list(type = "rect",
                        fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                        y0 = 0, y1 = input$ylim2_metrics,
                        x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
                        x1 = input$window_around_index_peak_max + input$IndexRepeat1))
          )
      }
    }
  })

  output$plot_traces_INDEX_UI <- renderUI({
    plotlyOutput("plot_traces_INDEX", height = (300 + input$HeightPeaks_metrics*20))
  })

  output$plot_traces_INDEX <- renderPlotly({
    validate(
      need(!is.null(input$sample_subset2), 'You do not have any baseline control samples in your metadata, please check your metadata file.'))

    if (is.null(peaks_module$index_list())) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$show_peaks_metrics == "YES") {
      show_peaks_metrics = TRUE
    }
    else {
      show_peaks_metrics = FALSE
    }

    xlim = c(input$xlim1_metrics, input$xlim2_metrics)
    ylim = c(input$ylim1_metrics, input$ylim2_metrics)
    height_color_threshold = 0.05
    plot_title = NULL

    #there must be a simpler way of the following if else below
      data <- peaks_module$index_list()[[input$sample_subset2]]$trace_bp_df
      data$x <- data$calculated_repeats

    if (!is.null(xlim)) {
      data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
    }

    if (show_peaks_metrics == TRUE) {
      # add points onto plot showing peaks
      peak_table <- peaks_module$index_list()[[input$sample_subset2]]$repeat_table_df
      peak_table$x <- peak_table$repeats

        if (!is.null(xlim)) {
          peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
        }

        tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
        tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
        if (!is.null(peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_height) && !is.na(peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_height)) {
          tallest_peak_height <- peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_height
          tallest_peak_x <- peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_repeat
        }

        peaks_above <- peak_table[which(peak_table$height > tallest_peak_height * height_color_threshold), ]
        peaks_below <- peak_table[which(peak_table$height < tallest_peak_height * height_color_threshold), ]

      }

    if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
      if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                source = "plot_peak2",
                height = (300 + input$HeightPeaks_metrics*20)) %>%
          add_markers(x = peaks_above$x,
                      y = peaks_above$height,
                      colors = "blue") %>%
          # add_markers(x = peaks_below$x,
          #             y = peaks_below$height,
          #             colors = "purple") %>%
          add_markers(x = tallest_peak_x,
                      y = tallest_peak_height,
                      colors = "green") %>%
          add_segments(x = peak_table$repeats,
                       y = peak_table$height,
                       xend = peak_table$calculated_repeats,
                       yend = peak_table$height,
                       line = list(dash = "dash")) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset2]]$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(
                   #vertical line
                   list(type = "line", x0 = input$IndexRepeat2,
                        x1 = input$IndexRepeat2,
                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
          )
      }
    }
    else {
      plot_ly(data = data,
              x = ~x, y = ~signal,
              type = "scatter",
              mode = "lines",
              source = "plot_peak2",
              height = (300 + input$HeightPeaks_metrics*20)) %>%
        layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset2]]$unique_id, plot_title),
               xaxis = list("Repeats",
                            range = xlim),
               yaxis = list("Signal",
                            range = ylim),
               shapes = list(
                 #vertical line
                 list(type = "line", x0 = input$IndexRepeat2,
                      x1 = input$IndexRepeat2,
                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
        )
    }
  })

  observe({
    if (!is.null(reactive_metrics$Index_Table2) && !is.null(reactive_metrics$Index_Table)) {
      if (reactive_metrics$Index_Table2 == "no") {
        shinyjs::hide("text_no_data2")
      }
      else {
        shinyjs::show("text_no_data2")
      }
    }
  })

  output$text_no_data2 <- renderUI({
    if (is.null(reactive_metrics$df)) {
    h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Instability Metrics not computed! Press Apply on the left to compute metrics</b>'))
    }
    else {
      h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.</b>'))
    }
  })

  ###SAVE FUNCTION
  output$downloadDataSave <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_SAVED_OBJECT_", sessionInfo()$otherPkgs$traceShiny$Version, ".Rdata")
    },
    content = function(file) {

      #Upload
      laddertable = upload_data$laddertable()
      fsa_list = upload_data$fsa_list()
      metadata_table = upload_data$metadata_table()
      DataUpload = upload_data$DataUpload()
      DataUploadMeta = upload_data$DataUploadMeta()
      Ladder_switch = upload_data$Ladder_switch()

      #Ladders
      ladders <- ladder_module$ladders()
      scan <- ladder_module$scan()
      size <- ladder_module$size()
      LadderChannel <- ladder_module$LadderChannel()
      SignalChannel <- ladder_module$SignalChannel()
      LadderSizes <- ladder_module$LadderSizes()
      spikeswitch <- ladder_module$spikeswitch()
      spikelocation <- ladder_module$spikelocation()
      zerofloor <- ladder_module$zerofloor()
      ladderselectionwindow <- ladder_module$ladderselectionwindow()
      smoothingwindow <- ladder_module$smoothingwindow()
      maxcombinations <- ladder_module$maxcombinations()

      #Peaks
      index_list <- peaks_module$index_list()
      min_bp_size <- peaks_module$min_bp_size()
      max_bp_size <- peaks_module$max_bp_size()
      smoothing_window_peaks <- peaks_module$smoothing_window()
      minimum_peak_signal <- peaks_module$minimum_peak_signal()
      batchcorrectionswitch <- peaks_module$batchcorrectionswitch()
      peak_region_size_gap_threshold <- peaks_module$peak_region_size_gap_threshold()
      peak_region_height_threshold_multiplier <- peaks_module$peak_region_height_threshold_multiplier()
      assay_size_without_repeat <- peaks_module$assay_size_without_repeat()
      repeat_size <- peaks_module$repeat_size()
      force_whole_repeat_units <- peaks_module$force_whole_repeat_units()
      repeat_calling_algorithm <- peaks_module$repeat_calling_algorithm()
      repeat_calling_algorithm_size_window_around_allele <- peaks_module$repeat_calling_algorithm_size_window_around_allele()
      repeat_calling_algorithm_size_period <- peaks_module$repeat_calling_algorithm_size_period()
      repeat_calling_algorithm_peak_assignment_scan_window <- peaks_module$repeat_calling_algorithm_peak_assignment_scan_window()
      sample_traces_size <- peaks_module$sample_traces_size()
      sample_traces_repeats <- peaks_module$sample_traces_repeats()

      #Investigate
      instability_metrics <- reactive_metrics$df
      Index_Table <- reactive_metrics$Index_Table
      Index_Table2 <- reactive_metrics$Index_Table2
      sample_subset_metrics <- reactive_metrics$sample_subset_metrics
      sample_subset2 <- reactive_metrics$sample_subset2
      peak_threshold <- input$peak_threshold
      repeat_range1 <- input$repeat_range1
      repeat_range2 <- input$repeat_range2
      repeat_range3 <- input$repeat_range3
      percentile_range1 <- input$percentile_range1
      percentile_range2 <- input$percentile_range2
      percentile_range3 <- input$percentile_range3
      window_around_index_peak_min <- input$window_around_index_peak_min
      window_around_index_peak_max <- input$window_around_index_peak_max

      #Package Version
      Package_version <- sessionInfo()$otherPkgs$traceShiny$Version


      save("laddertable", "fsa_list", "metadata_table", "DataUpload", "DataUploadMeta", "Ladder_switch",
           "ladders", "scan", "size", "LadderChannel", "SignalChannel", "LadderSizes", "spikeswitch", "spikelocation", "zerofloor", "ladderselectionwindow", "smoothingwindow", "maxcombinations",
           "index_list", "min_bp_size", "max_bp_size", "smoothing_window_peaks", "minimum_peak_signal", "batchcorrectionswitch", "peak_region_size_gap_threshold",
           "peak_region_height_threshold_multiplier", "assay_size_without_repeat", "repeat_size", "force_whole_repeat_units", "repeat_calling_algorithm", "repeat_calling_algorithm_size_window_around_allele",
           "repeat_calling_algorithm_size_period", "repeat_calling_algorithm_peak_assignment_scan_window", "sample_traces_size", "sample_traces_repeats",
           "instability_metrics", "peak_threshold", "window_around_index_peak_min", "window_around_index_peak_max", "repeat_range1", "repeat_range2", "repeat_range3", "percentile_range1", "percentile_range2", "percentile_range3",
           "sample_subset2", "sample_subset_metrics", "Package_version", "Index_Table", "Index_Table2",
           file = file)
    }
  )

}
