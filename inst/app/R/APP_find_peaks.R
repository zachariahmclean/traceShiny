peaks_box_ui1 <- function(id) {
  box(id = "PeaksBoxIntro", title = strong("Find Peaks"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,
      h5(includeHTML("data/peaks/peaks_landing_page.html")),
      img(id="pipe3",src="pipeline3.jpg", width="20%"),
      br(),
      br(),

      fluidRow(column(3,
                      valueBox("PROCEED", actionBttn("PeaksBoxSTART", "START",
                                                     style = "jelly",
                                                     color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

peaks_box_ui2 <- function(id) {
  box(id = "PeaksBox1", title = p("Settings", help_button("peaks_param")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      materialSwitch("advancesettings_Peaks", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show Advanced Settings')), value = FALSE, status = "primary"),

      fluidRow(
        column(12,
               h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Find Fragments</b>')),
               fluidRow(
                 column(6,
                        numericInput("min_bp_size", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Repeat Size')),
                                     min = 1,
                                     value = 35, step = 1)
                 ),
                 conditionalPanel(
                   condition = 'input.advancesettings_Peaks == true',
                   column(6,
                          numericInput("max_bp_size", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Maximum Repeat Size')),
                                       min = 1,
                                       value = 500, step = 1)
                   )
                 )
               ),
               conditionalPanel(
                 condition = 'input.advancesettings_Peaks == true',
                 fluidRow(
                   column(6,
                          numericInput("smoothing_window", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Smoothing Window')),
                                       min = 1,
                                       value = 21, step = 1)),
                   column(6,
                          numericInput("minimum_peak_signal", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Peak Signal')),
                                       min = 1,
                                       value = 20, step = 1))
                 ),
                 fluidRow(
                   column(6,
                          numericInput("peak_scan_ramp", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Scan Ramp')),
                                       min = 1,
                                       value = 5, step = 1))
                 )
               )
        )
      ),

      fluidRow(
        column(12,
               h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Find Alleles</b>')),

               fluidRow(
                 column(6,
                        pickerInput("number_of_alleles", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Number of Alleles')),
                                    choices = c("1", "2"), selected = "1"))
               ),
               conditionalPanel(
                 condition = 'input.advancesettings_Peaks == true',
                 fluidRow(
                   column(6,
                          numericInput("peak_region_size_gap_threshold", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Region Size Gap Threshold')),
                                       min = 1,
                                       value = 6, step = 1)
                   ),
                   column(6,
                          numericInput("peak_region_signal_threshold_multiplier", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Region Signal Threshold Multiplier')),
                                       min = 1,
                                       value = 1, step = 1)
                   )
                 )
               )
        )
      ),

      fluidRow(
        column(12,
               h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Call Repeats</b>')),
               fluidRow(
                 column(6,
                        numericInput("assay_size_without_repeat", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Assay Size Without Repeat')),
                                     min = 1,
                                     value = 87, step = 1)
                 ),
                 column(6,
                        numericInput("repeat_size", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Repeat Size')),
                                     min = 1,
                                     value = 3, step = 1)
                 )
               ),

               fluidRow(
                 column(12,
                        pickerInput("batchcorrectionswitch", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Correction')),
                                    choices = c("none", "batch", "repeat"), selected = "none")
                 )
               ),

               fluidRow(
                 column(6,
                        radioGroupButtons(
                          inputId = "force_whole_repeat_units",
                          label = h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Force Whole Repeat Units')),
                          choices = c("YES",
                                      "NO"),
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle",
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o",
                                        style = "color: steelblue")),
                          selected = "YES"
                        )
                 ),
                 column(6,
                        radioGroupButtons(
                          inputId = "force_repeat_pattern",
                          label = h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Force repeat pattern')),
                          choices = c("YES",
                                      "NO"),
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle",
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o",
                                        style = "color: steelblue")),
                          selected = "NO"
                        )
                 )
               ),
               conditionalPanel(
                 condition = 'input.advancesettings_Peaks == true',
                 fluidRow(
                   column(6,
                          numericInput("force_repeat_pattern_size_period", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Force repeat pattern size period')),
                                       min = 1,
                                       value = NA_real_, # is set below based on repeat_size
                                       step = 1)
                   ),
                   column(6,
                          numericInput("force_repeat_pattern_size_window", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Force repeat pattern size window')),
                                       min = 0,
                                       value = 0.5, step = 0.1)
                   )
                 )
               )
        )
      ),
      p(style="text-align: center;", actionBttn("startbuttonFindPeaks", "APPLY", size = "lg"))
  )
}

peaks_box_ui3 <- function(id) {
  box(id = "PeaksBox2", title = p("Find Peaks", help_button("peaks_plot")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(2,
               pickerInput("sample_subset", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = NULL)),

        column(1,
               actionButton("up_peak", NULL, icon("arrow-up"), style='text-align: left; margin-top:50px; font-size:200%'),
               br(),
               actionButton("down_peak", NULL, icon("arrow-down"), style='text-align: left; margin-top:-20px; font-size:200%')),

        column(3,
               radioGroupButtons(
                 inputId = "show_peaks",
                 label = h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Show Peaks')),
                 choices = c("YES",
                             "NO"),
                 justified = TRUE,
                 selected = "YES"
               )
        ),
        column(6,
               sliderInput("HeightPeaks", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )),
      fluidRow(
        column(12,
               h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Plot Controls</b>')),
               fluidRow(
                 column(1,
                        numericInput("xlim1", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                                     value = 0)),

                 column(1,
                        numericInput("xlim2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                                     value = 250)),
                 column(1,
                        numericInput("ylim1", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                                     value = 0)),

                 column(1,
                        numericInput("ylim2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                                     value = 2000))))),
      htmlOutput("text_no_data"),
      withSpinner(htmlOutput("plot_tracesUI")),
      htmlOutput("peaks_text"),
      p(style="text-align: right;", downloadButton("peaks_text_download")),
      dataTableOutput("peaks_summary")
  )
}

peaks_box_ui4 <- function(id) {
  box(id = "PeaksBox3", title = strong("Correction Trace", help_button("peaks_correction")), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,

      fluidRow(
        column(3,
               pickerInput("sample_subset_Batch", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Batch')),
                           choices = NULL)),
        column(3,
               sliderInput("HeightBatch", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1))
      ),

      htmlOutput("BatchWarning"),

      fluidRow(
        column(6,
               h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Before Correction</b>'))),
        column(6,
               h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>After Correction</b>')))
      ),
      withSpinner(htmlOutput("plot_traces_BatchUI"))
  )
}

peaks_box_ui5 <- function(id) {
  box(id = "PeaksBox4", title = strong("Repeat Correction Correlation", help_button("peaks_repeat_correction")), status = "warning", solidHeader = F,
      collapsible = T, collapsed = F, width = 12,

      fluidRow(
        column(3,
               pickerInput("sample_subset_Repeat", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select run_id')),
                           choices = NULL)),
        column(3,
               p(style="text-align: left;margin-top:50px;", actionBttn("Manual_peak", "Manually select Correction Peak", size = "lg")))
      ),

      fluidRow(
        column(6,
               plotlyOutput("correlation_plot", height = 800)
        ),
        column(6,
               htmlOutput("correlation_text"),
               p(style="text-align: right;", downloadButton("correlation_text_download")),
               dataTableOutput("correlation_summary"))
      )
  )
}

peaks_server <- function(input, output, session, continue_module, upload_data, ladder_module) {

  # help files
  help_click("peaks_param", helpfile = "data/peaks/peak_params.html")
  help_click("peaks_plot", helpfile = "data/peaks/peaks_plot.html")
  help_click("peaks_repeat_correction", helpfile = "data/peaks/peaks_repeat_correlation.html")
  help_click("peaks_correction", helpfile = "data/peaks/peaks_correction.html")

  reactive_peaks <- reactiveValues()

  #Load saved objects if applicable
  observe({
    if(!is.null(continue_module$index_list())) {
      reactive_peaks$peaks <- continue_module$index_list()
      reactive_peaks$config <- continue_module$config()
    }
  })

  observeEvent(ignoreInit = F, list(input$MetadataUpload, input$SelectionButton, input$DataFSA, input$fastq, input$fileinputLOAD), {
    reactive_peaks$peaks <- NULL
    reactive_peaks$config <- NULL
  })

  #Download
  output$peaks_text_download <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Peaks_Table.csv")
    },
    content = function(file) {
      if (!is.null(upload_data$metadata_table())) {
        df <- dplyr::left_join(upload_data$metadata_table(), extract_fragment_summary(reactive_peaks$peaks))
      }
      else {
        df <- arrange(extract_fragment_summary(reactive_peaks$peaks), unique_id)
      }

      rownames(df) <- NULL
      write.csv(df, file, row.names = F, col.names = T)
    }
  )

  output$correlation_text_download <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Repeat_correction_summary.csv")
    },
    content = function(file) {
      if (!is.null(upload_data$metadata_table())) {
        df <- trace::extract_repeat_correction_summary(reactive_peaks$peaks)
        rownames(df) <- NULL
        write.csv(df, file, row.names = F, col.names = T)
      }
    }
  )

  observeEvent(input$PeaksBoxSTART, {

    shinyjs::hide("NextButtonLadder")

    reactive_peaks$peaks <- NULL

    if(input$PeaksBoxIntro$collapsed == FALSE) {
      js$collapse("PeaksBoxIntro")
    }
    shinyjs::show("PeaksBox1")
    shinyjs::show("PeaksBox2")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = T),
                                                     menuItem("Documentation", icon = icon("file"), startExpanded = T,
                                                              menuSubItem("Step 1: Upload", tabName = "Documentation1"),
                                                              menuSubItem("Step 2: Find Ladders", tabName = "Documentation2"),
                                                              menuSubItem("Step 3: Find Peaks", tabName = "Documentation3"),
                                                              menuSubItem("Step 4: Instability Metrics", tabName = "Documentation4"),
                                                              menuSubItem("Step 5: Analysis", tabName = "Documentation5"))))

    if (is.null(upload_data$metadata_table())) {
      shinyjs::hide("PeaksBox3")
      shinyjs::hide("PeaksBox4")
      shinyjs::hide("batchcorrectionswitch")
    }
    else if (!is.null(upload_data$metadata_table())) {
      if (any(!is.na(upload_data$metadata_table()$batch_sample_id))) {
        if(input$PeaksBox3$collapsed == TRUE) {
          js$collapse("PeaksBox3")
        }
        shinyjs::show("PeaksBox3")
        shinyjs::show("batchcorrectionswitch")
        updatePickerInput(session, "sample_subset_Batch", choices = na.omit(unique(upload_data$metadata_table()$batch_sample_id)))
        updatePickerInput(session, "sample_subset_Repeat", choices = na.omit(unique(upload_data$metadata_table()$batch_run_id)))
      }
      else {
        shinyjs::hide("PeaksBox3")
        shinyjs::hide("PeaksBox4")
        shinyjs::hide("batchcorrectionswitch")
      }
    }
  })

  observe({
    if (is.null(upload_data$metadata_table())) {
      shinyjs::hide("PeaksBox3")
      shinyjs::hide("PeaksBox4")
      shinyjs::hide("batchcorrectionswitch")
    }
    else if (!is.null(upload_data$metadata_table())) {
      if (any(!is.na(upload_data$metadata_table()$batch_sample_id))) {
        if(input$PeaksBox3$collapsed == TRUE) {
          js$collapse("PeaksBox3")
        }
        shinyjs::show("PeaksBox3")
        shinyjs::show("batchcorrectionswitch")
        updatePickerInput(session, "sample_subset_Batch", choices = na.omit(unique(upload_data$metadata_table()$batch_sample_id)))
        updatePickerInput(session, "sample_subset_Repeat", choices = na.omit(unique(upload_data$metadata_table()$batch_run_id)))
      }
      else {
        shinyjs::hide("PeaksBox3")
        shinyjs::hide("PeaksBox4")
        shinyjs::hide("batchcorrectionswitch")
      }
    }

    if (input$batchcorrectionswitch == "none" | input$batchcorrectionswitch == "batch") {
      shinyjs::hide("PeaksBox4")
    }
    else {
      shinyjs::show("PeaksBox4")
    }

    if (input$batchcorrectionswitch == "none") {
      shinyjs::hide("PeaksBox3")
    }
    else {
      shinyjs::show("PeaksBox3")
    }

  })

  observeEvent(input$NextButtonLadder, {

    reactive_peaks$peaks <- NULL

    shinyjs::hide("NextButtonLadder")

    if(input$PeaksBoxIntro$collapsed == TRUE) {
      js$collapse("PeaksBoxIntro")
    }
    shinyjs::hide("PeaksBox1")
    shinyjs::hide("PeaksBox2")
    shinyjs::hide("PeaksBox3")
    shinyjs::hide("PeaksBox4")
    shinyjs::hide("NextButtonPeaks")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = T,
                                                              badgeColor = "green", badgeLabel = "new"),
                                                     menuItem("Documentation", icon = icon("file"), startExpanded = T,
                                                              menuSubItem("Step 1: Upload", tabName = "Documentation1"),
                                                              menuSubItem("Step 2: Find Ladders", tabName = "Documentation2"),
                                                              menuSubItem("Step 3: Find Peaks", tabName = "Documentation3"),
                                                              menuSubItem("Step 4: Instability Metrics", tabName = "Documentation4"),
                                                              menuSubItem("Step 5: Analysis", tabName = "Documentation5"))
    ))
  })


  observeEvent(input$startbuttonFindPeaks, {
    tryCatch({
      withProgress(message = 'Finding Peaks ...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     reactive_peaks$peaks <- NULL

                     reactive_peaks$peaks <- lapply(ladder_module$ladders(), function(x) x$clone())

                     reactive_peaks$config <- ladder_module$config()

                     reactive_peaks$config$smoothing_window <- input$smoothing_window
                     reactive_peaks$config$minimum_peak_signal <- input$minimum_peak_signal
                     reactive_peaks$config$min_bp_size <- input$min_bp_size*input$repeat_size + input$assay_size_without_repeat
                     reactive_peaks$config$max_bp_size <- input$max_bp_size*input$repeat_size + input$assay_size_without_repeat
                     reactive_peaks$config$peak_scan_ramp <- input$peak_scan_ramp

                     trace:::find_fragments(reactive_peaks$peaks, reactive_peaks$config)

                     if (!is.null(upload_data$metadata_table())) {

                       trace:::add_metadata(
                         fragments_list = reactive_peaks$peaks,
                         metadata_data.frame = upload_data$metadata_table()
                       )
                     }

                     reactive_peaks$config$number_of_alleles <- as.numeric(input$number_of_alleles)
                     reactive_peaks$config$peak_region_size_gap_threshold <- input$peak_region_size_gap_threshold
                     reactive_peaks$config$peak_region_signal_threshold_multiplier <- input$peak_region_signal_threshold_multiplier

                     trace:::find_alleles(reactive_peaks$peaks, reactive_peaks$config)

                     reactive_peaks$config$assay_size_without_repeat <- input$assay_size_without_repeat
                     reactive_peaks$config$repeat_size <- input$repeat_size
                     reactive_peaks$config$force_whole_repeat_units <- if(input$force_whole_repeat_units == "YES") TRUE else FALSE
                     reactive_peaks$config$correction <- input$batchcorrectionswitch
                     reactive_peaks$config$force_repeat_pattern <- if(input$force_repeat_pattern == "YES") TRUE else FALSE
                     reactive_peaks$config$force_repeat_pattern_size_period <- input$force_repeat_pattern_size_period
                     reactive_peaks$config$force_repeat_pattern_size_window <- input$force_repeat_pattern_size_window

                     reactive_peaks$batchcorrectionswitch <- input$batchcorrectionswitch

                     trace:::call_repeats(fragments_list = reactive_peaks$peaks, reactive_peaks$config)

                     shinyjs::show("NextButtonPeaks")

                     if(input$MetricsBoxIntro$collapsed == TRUE) {
                       js$collapse("MetricsBoxIntro")
                     }
                     shinyjs::hide("MetricsBox1")
                     shinyjs::hide("MetricsBox2")
                     shinyjs::hide("MetricsBox3")
                     shinyjs::hide("MetricsBox4")
                     shinyjs::hide("NextButtonMetrics")

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = T),
                                                                      menuItem("Instability Metrics", icon = icon("water-ladder"), tabName = "InstabilityMetrics", selected = F,
                                                                               badgeColor = "green", badgeLabel = "new"),
                                                                      menuItem("Documentation", icon = icon("file"), startExpanded = T,
                                                                               menuSubItem("Step 1: Upload", tabName = "Documentation1"),
                                                                               menuSubItem("Step 2: Find Ladders", tabName = "Documentation2"),
                                                                               menuSubItem("Step 3: Find Peaks", tabName = "Documentation3"),
                                                                               menuSubItem("Step 4: Instability Metrics", tabName = "Documentation4"),
                                                                               menuSubItem("Step 5: Analysis", tabName = "Documentation5"))))
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observe({
    if (!is.null(reactive_peaks$peaks) && !is.null(input$sample_subset)) {
      updateNumericInput(session, "xlim1", value = reactive_peaks$peaks[[input$sample_subset]]$get_allele_peak()$allele_repeat - 50)
      updateNumericInput(session, "xlim2", value = reactive_peaks$peaks[[input$sample_subset]]$get_allele_peak()$allele_repeat + 50)
      updateNumericInput(session, "ylim1", value = -200)
      updateNumericInput(session, "ylim2", value = reactive_peaks$peaks[[input$sample_subset]]$get_allele_peak()$allele_signal + 100)
    }
  })

  observe({
    if (input$force_repeat_pattern == "NO") {
      updateNumericInput(session, "force_repeat_pattern_size_period", value = input$repeat_size * 0.93)
      shinyjs::hide("force_repeat_pattern_size_period")
      shinyjs::hide("force_repeat_pattern_size_window")
    } else {
      updateNumericInput(session, "force_repeat_pattern_size_period", value = input$repeat_size * 0.93)
      shinyjs::show("force_repeat_pattern_size_period")
      shinyjs::show("force_repeat_pattern_size_window")
    }
  })

  observe({
    updatePickerInput(session, "sample_subset", choices = names(upload_data$fsa_list()))
  })

  output$plot_tracesUI <- renderUI({
    plotlyOutput("plot_traces", height = (300 + input$HeightPeaks*20))
  })

  output$plot_traces <- renderPlotly({

    if (is.null(reactive_peaks$peaks)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$show_peaks == "YES") {
      show_peaks = TRUE
    }
    else {
      show_peaks = FALSE
    }

    fragments <- reactive_peaks$peaks[[input$sample_subset]]
    xlim = c(input$xlim1, input$xlim2)
    ylim = c(input$ylim1, input$ylim2)

    signal_color_threshold = input$minimum_peak_signal
    plot_title = NULL

    data <- fragments$trace_bp_df
    data$x <- data$calculated_repeats

    if (!is.null(xlim)) {
      data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
    }

    if (show_peaks == TRUE) {
      # add points onto plot showing peaks

      peak_table <- fragments$repeat_table_df
      peak_table$x <- peak_table$repeats

      if (!is.null(xlim)) {
        peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
      }

      tallest_peak_signal <- peak_table[which(peak_table$signal == max(peak_table$signal)), "signal"]
      tallest_peak_x <- peak_table[which(peak_table$signal == tallest_peak_signal), "x"]
      if (!is.null(fragments$get_allele_peak()$allele_signal) && !is.na(fragments$get_allele_peak()$allele_signal)) {
        tallest_peak_signal <- fragments$get_allele_peak()$allele_signal
        tallest_peak_x <- fragments$get_allele_peak()$allele_repeat
      }


      peaks_above <- peak_table[which(peak_table$signal > input$minimum_peak_signal), ]
      peaks_below <- peak_table[which(peak_table$signal < input$minimum_peak_signal), ]

    }

    if (show_peaks == TRUE && nrow(peak_table) > 0) {
      if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                height = (300 + input$HeightPeaks*20),
                name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), "")) %>%
          add_trace(x = peaks_above$x,
                    y = peaks_above$signal,
                    mode = "markers",
                    name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), " Peaks Above Threshold")) %>%
          add_trace(x = peaks_below$x,
                    y = peaks_below$signal,
                    mode = "markers",
                    name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), " Peaks Below Threshold")) %>%
          add_trace(x = tallest_peak_x,
                    y = tallest_peak_signal,
                    mode = "markers",
                    name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), " Modal Peak")) %>%
          add_segments(x = peak_table$repeats,
                       y = peak_table$signal,
                       xend = peak_table$calculated_repeats,
                       yend = peak_table$signal,
                       line = list(dash = "dash"),
                       name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), " Force Whole Repeats")) %>%
          layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
                 xaxis = list(title = "Repeats",
                              range = xlim),
                 yaxis = list(title = "Signal",
                              range = ylim)
          )
      }
    }
    else {
      plot_ly(data = data,
              x = ~x, y = ~signal,
              type = "scatter",
              mode = "lines",
              height = (300 + input$HeightPeaks*20),
              name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), "")) %>%
        layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
               xaxis = list(title = "Repeats",
                            range = xlim),
               yaxis = list(title = "Signal",
                            range = ylim)
        )
    }
  })

  observe({
    if (is.null(reactive_peaks$peaks)) {
      shinyjs::show("text_no_data")
      shinyjs::hide("peaks_text")
      shinyjs::hide("peaks_text_download")
      shinyjs::hide("peaks_summary")
    }
    else {
      shinyjs::hide("text_no_data")
      shinyjs::show("peaks_text")
      shinyjs::show("peaks_text_download")
      shinyjs::show("peaks_summary")
    }
  })

  output$text_no_data <- renderUI({
    h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Please select your inputs and press apply to start analysis.</b>'))
  })

  output$plot_traces_BatchUI <- renderUI({
    plotlyOutput("plot_traces_Batch", height = (300 + input$HeightBatch*20))
  })

  output$plot_traces_Batch <- renderPlotly({

    validate(
      need(!is.null(reactive_peaks$peaks), 'Please Run The Analysis First'))

    fragments_list <- reactive_peaks$peaks

    xlim = c(input$xlim1, input$xlim2)
    ylim = c(input$ylim1, input$ylim2)

    size_standard_fragments <- sapply(fragments_list, function(x) x$batch_sample_id)
    controls_fragments_list <- fragments_list[which(!is.na(size_standard_fragments))]

    size_standard_fragments_sample_groups <- sapply(controls_fragments_list, function(x) x$batch_sample_id)

    split_by_sample <- split(controls_fragments_list, size_standard_fragments_sample_groups)

    sample_fragments <- split_by_sample[[input$sample_subset_Batch]]

    sample_traces_size <- lapply(sample_fragments, function(y) {
      df <- y$trace_bp_df
      df$x <- df$size
      return(df)
    })

    sample_traces_repeats <- lapply(sample_fragments, function(y) {
      df <- y$trace_bp_df
      df$x <- df$calculated_repeats
      return(df)
    })

    # Generate colors dynamically
    n_dfs <- length(sample_traces_size)

    # normalize signal to samples have the same maximum
    sample_traces_size <- lapply(sample_traces_size, function(x){
      x$signal <- x$signal - min(x$signal)
      x$rel_signal <- x$signal / max(x[which(x$calculated_repeats > input$min_bp_size),]$signal)
      return(x)
    })

    sample_traces_repeats <- lapply(sample_traces_repeats, function(x){
      x$signal <- x$signal - min(x$signal)
      x$rel_signal <- x$signal / max(x[which(x$calculated_repeats > input$min_bp_size),]$signal)
      return(x)
    })

    reactive_peaks$sample_traces_size <- sample_traces_size
    reactive_peaks$sample_traces_repeats <- sample_traces_repeats

    # add points onto plot showing peaks
    peak_table_peaks <- lapply(sample_fragments, function(y) {
      df <- y$trace_bp_df
      df$x <- (df$size - input$assay_size_without_repeat)/input$repeat_size
      df <- df[which(df$x < xlim[2] & df$x > xlim[1]), ]
      return(df)
    })

    peak_table_repeats <- lapply(sample_fragments, function(y) {
      df <- y$repeat_table_df
      df$x <- df$repeats
      df <- df[which(df$x < xlim[2] & df$x > xlim[1]), ]
      return(df)
    })

    peak_table_peaks_tallest <- lapply(peak_table_peaks, function(y) {
      tallest_peak_signal <- y[which(y$signal == max(y[which(y$calculated_repeats > input$min_bp_size),]$signal)), "x"]
      return(tallest_peak_signal)
    })

    peak_table_repeats_tallest <- lapply(peak_table_repeats, function(y) {
      tallest_peak_signal <- y[which(y$signal == max(y$signal)), "x"]
      return(tallest_peak_signal)
    })

    xlim_corrected = c(peak_table_repeats_tallest[[1]]-50, peak_table_repeats_tallest[[1]]+50)
    ylim_corrected = c(-0.2, 1.2)

    #Size
    p1 <- plot_ly(height = (300 + input$HeightBatch*20)) %>%
      layout(title = sample_traces_size[[1]]$batch_sample_id,
             xaxis = list(title = "Repeat",
                          range = xlim_corrected),
             yaxis = list(title = "Signal",
                          range = ylim_corrected)
      )

    ## Add the traces one at a time
    for (i in 1:n_dfs) {
      p1 <- p1 %>% add_trace(y = sample_traces_size[[i]]$rel_signal, x = ((sample_traces_size[[i]]$x - input$assay_size_without_repeat)/input$repeat_size), name = gsub(".fsa", "", unique(sample_traces_size[[i]]$unique_id)),
                             mode="lines") %>%
        add_trace(x = peak_table_peaks_tallest[[i]],
                  y = 1,
                  mode = "markers",
                  name = paste0(gsub(".fsa", "", unique(sample_traces_size[[i]]$unique_id)), " Modal Peak"))
    }

    #Repeats
    p2 <- plot_ly(height = (300 + input$HeightBatch*20)) %>%
      layout(title = sample_traces_repeats[[1]]$batch_sample_id,
             xaxis = list(title = "Repeat",
                          range = xlim_corrected),
             yaxis = list(title = "Signal",
                          range = ylim_corrected)
      )

    ## Add the traces one at a time
    for (i in 1:n_dfs) {
      p2 <- p2 %>% add_trace(y = sample_traces_repeats[[i]]$rel_signal, x = sample_traces_repeats[[i]]$x, name = gsub(".fsa", "", unique(sample_traces_repeats[[i]]$unique_id)),
                             mode="lines") %>%
        add_trace(x = peak_table_repeats_tallest[[i]],
                  y = 1,
                  mode = "markers",
                  name = paste0(gsub(".fsa", "", unique(sample_traces_repeats[[i]]$unique_id)), " Modal Peak"))
    }

    subplot(p1, p2)

  })

  output$correlation_plot <- renderPlotly({
    validate(
      need(!is.null(reactive_peaks$peaks), 'Please Run the Analysis First'))
    validate(
      need(reactive_peaks$batchcorrectionswitch == "repeat", 'Please Re-Run the Analysis First'))

    plot_repeat_correction_model(reactive_peaks$peaks, input$sample_subset_Repeat)
  })

  output$correlation_summary <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_peaks$peaks), 'Please Run the Analysis First'))
    validate(
      need(reactive_peaks$batchcorrectionswitch == "repeat", 'Please Re-Run the Analysis First'))

    df <- trace:::extract_repeat_correction_summary(reactive_peaks$peaks)
    rownames(df) <- NULL

    ## Colour and values for table colour formatting
    brks <- seq(0, 0.3, .0001)
    clrs <- colorRampPalette(c("green", "white", "red"))(length(brks) + 1)

    datatable(df,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              selection = 'single',
              rownames = FALSE) %>%
      formatStyle(c("abs_avg_residual"), backgroundColor = styleInterval(brks, clrs))
  })

  output$correlation_summary2 <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_peaks$peaks), 'Please Run The Analysis First'))
    validate(
      need(reactive_peaks$batchcorrectionswitch %in% "repeat", 'Please Re-Run The Analysis First'))

    df <-trace:::extract_repeat_correction_summary(reactive_peaks$peaks)
    rownames(df) <- NULL

    ## Colour and values for table colour formatting
    brks <- seq(0, 0.3, .0001)
    clrs <- colorRampPalette(c("green", "white", "red"))(length(brks) + 1)

    datatable(df,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              selection = 'single',
              rownames = FALSE) %>%
      formatStyle(c("abs_avg_residual"), backgroundColor = styleInterval(brks, clrs))
  })

  observeEvent(input$correlation_summary2_rows_selected, {
    df <-extract_repeat_correction_summary(reactive_peaks$peaks)
    updatePickerInput(session, "sample_subset_Manual", selected = df[input$correlation_summary2_rows_selected, ]$unique_id)
  })

  observeEvent(input$up_peak, {
    tryCatch({
      updatePickerInput(session, "sample_subset", selected = if (which(names(upload_data$fsa_list()) == input$sample_subset) - 1 == 0)
        upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset)]]$unique_id
        else upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset) - 1]]$unique_id)
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$down_peak, {
    tryCatch({
      updatePickerInput(session, "sample_subset", selected = if (which(names(upload_data$fsa_list()) == input$sample_subset) + 1 > length(names(upload_data$fsa_list())))
        upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset)]]$unique_id
        else upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset) + 1]]$unique_id)
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$up_peak_Manual, {
    tryCatch({

      df <- upload_data$metadata_table()[which(!is.na(upload_data$metadata_table()$batch_sample_modal_repeat)),]$unique_id

      updatePickerInput(session, "sample_subset_Manual", selected = if (which(df == input$sample_subset_Manual) - 1 == 0)
        df[which(df == input$sample_subset_Manual)]
        else df[which(df == input$sample_subset_Manual) - 1])
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$down_peak_Manual, {
    tryCatch({
      df <- upload_data$metadata_table()[which(!is.na(upload_data$metadata_table()$batch_sample_modal_repeat)),]$unique_id

      updatePickerInput(session, "sample_subset_Manual", selected = if (which(df == input$sample_subset_Manual) + 1 > length(df))
        df[which(df == input$sample_subset_Manual)]
        else df[which(df == input$sample_subset_Manual) +  1])
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  output$peaks_summary <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_peaks$peaks), 'Please Run The Analysis First'))

    if (!is.null(upload_data$metadata_table())) {
      df <- dplyr::left_join(upload_data$metadata_table(), extract_fragment_summary(reactive_peaks$peaks))
    }
    else {
      df <- arrange(extract_fragment_summary(reactive_peaks$peaks), unique_id)
    }

    rownames(df) <- NULL

    datatable(df,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              selection = 'single',
              rownames = FALSE)
  })

  output$peaks_text <- renderUI({
    h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Called Peaks Table (check this to see how well the peaks are called)</b>'))
  })

  output$correlation_text <- renderUI({
    h5(HTML('<h5 style = "text-align:justify;color:#000000"><b>Repeat Correction Summary (for plot on the left)</b>'))
  })

  observeEvent(input$peaks_summary_rows_selected, {
    updatePickerInput(session, "sample_subset", selected = upload_data$fsa_list()[[input$peaks_summary_rows_selected]]$unique_id)
  })

  output$BatchWarning <- renderUI({
    if (all(((reactive_peaks$sample_traces_size[[1]]$x - input$assay_size_without_repeat)/input$repeat_size) == reactive_peaks$sample_traces_repeats[[1]]$x)) {
      h5(HTML('<h5 style = "text-align:justify;color:#FF0000"><b>Warning: Correction not performed the before and after plots will look the same. </b>'))
    }
    else{
      return()
    }
  })

  observeEvent(input$Manual_peak, {
    showModal(modalDialog(
      title = strong("Manual Peak Correction"),
      fluidRow(
        column(4,
               pickerInput("sample_subset_Manual", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = upload_data$metadata_table()[which(!is.na(upload_data$metadata_table()$batch_sample_modal_repeat)),]$unique_id, selected = upload_data$metadata_table()[which(!is.na(upload_data$metadata_table()$batch_sample_modal_repeat)),]$unique_id[1])),

        column(2,
               actionButton("up_peak_Manual", NULL, icon("arrow-up"), style='text-align: left; margin-top:50px; font-size:200%'),
               br(),
               actionButton("down_peak_Manual", NULL, icon("arrow-down"), style='text-align: left; margin-top:-20px; font-size:200%')),
        column(6,
               sliderInput("HeightPeaks_Manual", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )),
      fluidRow(
        column(6,
               numericInput("Modal_Peak", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Modal Repeat for correction (can be manually changed)')),
                            value = NULL)
        ),
        column(6,
               p(style="text-align: left;margin-top:30px; margin-left:-100px;", actionBttn("Manual_peak_start", "SET", size = "lg")))
      ),
      withSpinner(htmlOutput("plot_tracesUI_Manual")),
      dataTableOutput("correlation_summary2"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })

  observe({
    if (!is.null(reactive_peaks$peaks) && !is.null(input$sample_subset_Manual)) {
      updateNumericInput(session, "Modal_Peak", value = reactive_peaks$peaks[[input$sample_subset_Manual]]$get_allele_peak()$allele_repeat)
    }
  })

  observeEvent(input$Manual_peak_start, {
    tryCatch({

      reactive_peaks$peaks <- lapply(reactive_peaks$peaks, function(x) x$clone())

      if (!is.null(upload_data$metadata_table())) {
        trace:::add_metadata(
          fragments_list = reactive_peaks$peaks,
          metadata_data.frame = upload_data$metadata_table()
        )
      }

      trace:::find_alleles(reactive_peaks$peaks, reactive_peaks$config)

      trace:::call_repeats(reactive_peaks$peaks, reactive_peaks$config)

      reactive_peaks$peaks[[input$sample_subset_Manual]]$set_allele_peak(allele = input$number_of_alleles, unit = "repeats", value = input$Modal_Peak)

      trace:::call_repeats(reactive_peaks$peaks, reactive_peaks$config)
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
    })
  })

  output$plot_tracesUI_Manual <- renderUI({
    plotlyOutput("plot_traces_Manual", height = (300 + input$HeightPeaks_Manual*20))
  })

  output$plot_traces_Manual <- renderPlotly({

    if (is.null(reactive_peaks$peaks)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    fragments <- reactive_peaks$peaks[[input$sample_subset_Manual]]
    xlim = c(input$xlim1, input$xlim2)
    ylim = c(input$ylim1, input$ylim2)
    signal_color_threshold = input$minimum_peak_signal
    plot_title = NULL

    data <- fragments$trace_bp_df
    data$x <- data$calculated_repeats

    if (!is.null(xlim)) {
      data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
    }

    # add points onto plot showing peaks
    peak_table <- fragments$repeat_table_df
    peak_table$x <- peak_table$repeats

    if (!is.null(xlim)) {
      peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
    }

    tallest_peak_signal <- peak_table[which(peak_table$signal == max(peak_table$signal)), "signal"]
    tallest_peak_x <- peak_table[which(peak_table$signal == tallest_peak_signal), "x"]
    if (!is.null(fragments$get_allele_peak()$allele_signal) && !is.na(fragments$get_allele_peak()$allele_signal)) {
      tallest_peak_signal <- fragments$get_allele_peak()$allele_signal
      tallest_peak_x <- fragments$get_allele_peak()$allele_repeat
    }

    peaks_above <- peak_table[which(peak_table$signal > signal_color_threshold), ]
    peaks_below <- peak_table[which(peak_table$signal < signal_color_threshold), ]

    xlim_corrected = c(tallest_peak_x[[1]]-50, tallest_peak_x[[1]]+50)
    ylim_corrected = c(-200, tallest_peak_signal + 100)

    if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
      plot_ly(data = data,
              x = ~x, y = ~signal,
              type = "scatter",
              mode = "lines",
              height = (300 + input$HeightPeaks_Manual*20),
              name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), "")) %>%
        add_trace(x = tallest_peak_x,
                  y = tallest_peak_signal,
                  mode = "markers",
                  name = paste0(gsub(".fsa", "", unique(fragments$unique_id)), " Modal Peak")) %>%
        layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
               xaxis = list(title = "Repeats",
                            range = xlim_corrected),
               yaxis = list(title = "Signal",
                            range = ylim_corrected)
        )
    }
  })

  return(list(
    index_list = reactive(reactive_peaks$peaks),
    sample_traces_size = reactive(reactive_peaks$sample_traces_size),
    sample_traces_repeats = reactive(reactive_peaks$sample_traces_repeats),
    force_whole_repeat_units = reactive(reactive_peaks$force_whole_repeat_units),
    config = reactive(reactive_peaks$config)
  ))

}
