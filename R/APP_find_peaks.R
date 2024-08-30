peaks_box_ui1 <- function(id) {
  box(id = "PeaksBoxIntro", title = strong("Find Peaks"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,

      h4(HTML('<h4 style="text-align:justify">This function is basically a wrapper around pracma::findpeaks. As mentioned above, the default arguments arguments of pracma::findpeaks can be changed
      by passing them to find_fragments with.
      If too many and inappropriate peaks are being called, this may also be solved with the different repeat calling algorithms in call_repeats().')),
      br(), br(),

      fluidRow(column(3,
                      valueBox("NEW", actionBttn("PeaksBoxSTART", "START",
                                                 style = "jelly",
                                                 color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

peaks_box_ui2 <- function(id) {
  box(id = "PeaksBox1", title = p("Settings", help_button("Peaks_Upload")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      materialSwitch("advancesettings_Peaks", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Advanced Settings')), value = FALSE, status = "primary"),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Find Fragments</b>')),
               fluidRow(
                 column(6,
                        numericInput("min_bp_size", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Base Pair Size')),
                                     min = 1,
                                     value = 200, step = 1)
                 ),
                 conditionalPanel(
                   condition = 'input.advancesettings_Peaks == true',
                   column(6,
                          numericInput("max_bp_size", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Maximum Base Pair Size')),
                                       min = 1,
                                       value = 1000, step = 1)
                   )
                 )
               ),
               conditionalPanel(
                 condition = 'input.advancesettings_Peaks == true',
                 fluidRow(
                   column(6,
                          numericInput("smoothing_window", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Smoothing Window')),
                                       min = 1,
                                       value = 21, step = 1)),
                   column(6,
                          numericInput("minimum_peak_signal", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Peak Signal')),
                                       min = 1,
                                       value = 20, step = 1))
                 )
               )
        )
      ),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Find Alleles</b>')),
               fluidRow(
                 column(6,
                        radioGroupButtons(
                          inputId = "number_of_peaks_to_return",
                          label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Number of Peaks to Return')),
                          choices = c("1",
                                      "2"),
                          individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle",
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o",
                                        style = "color: steelblue")),
                          selected = "1"
                        ))
               ),
               conditionalPanel(
                 condition = 'input.advancesettings_Peaks == true',
                 fluidRow(
                   column(6,
                          numericInput("peak_region_size_gap_threshold", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Region Size Gap Threshold')),
                                       min = 1,
                                       value = 6, step = 1)
                   ),
                   column(6,
                          numericInput("peak_region_height_threshold_multiplier", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Region Height Threshold Multiplier')),
                                       min = 1,
                                       value = 1, step = 1)
                   )
                 )
               )
        )
      ),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Call Repeats</b>')),
               fluidRow(
                 column(6,
                        numericInput("assay_size_without_repeat", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Assay Size Without Repeat')),
                                     min = 1,
                                     value = 87, step = 1)
                 ),
                 column(6,
                        numericInput("repeat_size", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Repeat Size')),
                                     min = 1,
                                     value = 3, step = 1)
                 )
               ),

               fluidRow(
                 column(6,
                        radioGroupButtons(
                          inputId = "force_whole_repeat_units",
                          label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Force Whole Repeat Units')),
                          choices = c("YES",
                                      "NO"),
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle",
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o",
                                        style = "color: steelblue")),
                          selected = "YES"
                        )
                 )
               ),

               conditionalPanel(
                 condition = 'input.advancesettings_Peaks == true',
                 fluidRow(
                   column(6,
                          pickerInput("repeat_calling_algorithm", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Repeat Calling Algorithm')),
                                      choices = c("simple", "fft", "size_period"), selected = "simple")
                   ),
                   column(6,
                          numericInput("repeat_calling_algorithm_size_window_around_allele", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Size Window Around Allele')),
                                       min = 1,
                                       value = 15, step = 1)
                   )
                 ),
                 fluidRow(
                   column(6,
                          numericInput("repeat_calling_algorithm_size_period", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Size Period')),
                                       min = 1,
                                       value = 3, step = 1)
                   ),
                   column(6,
                          numericInput("repeat_calling_algorithm_peak_assignment_scan_window", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Assignment Scan Window')),
                                       min = 1,
                                       value = 3, step = 1)
                   )
                 )
               )
        )
      ),
      p(style="text-align: center;", actionBttn("startbuttonFindPeaks", "APPLY", size = "lg"))
  )
}

peaks_box_ui3 <- function(id) {
  box(id = "PeaksBox2", title = p("Find Peaks"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(3,
               pickerInput("sample_subset", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = NULL)),
        column(3,
               radioGroupButtons(
                 inputId = "show_peaks",
                 label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Show Peaks')),
                 choices = c("YES",
                             "NO"),
                 justified = TRUE,
                 selected = "YES"
               )
        ),
        column(6,
               sliderInput("HeightPeaks", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )),
      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Plot Controls</b>')),
               fluidRow(
                 column(1,
                        numericInput("xlim1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                                     value = 0)),

                 column(1,
                        numericInput("xlim2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                                     value = 250)),
                 column(1,
                        numericInput("ylim1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                                     value = 0)),

                 column(1,
                        numericInput("ylim2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                                     value = 2000))))),
      htmlOutput("text_no_data2"),
      htmlOutput("text_coordinates1"),
      htmlOutput("plot_tracesUI"),

      fluidRow(
        column(3,
               pickerInput("sample_subset2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Index Samples')),
                           choices = NULL))
      ),

      htmlOutput("text_coordinates2"),
      htmlOutput("plot_traces_INDEX_UI")
  )
}

peaks_server <- function(input, output, session, continue_module, upload_data, ladder_module) {

  reactive_peaks <- reactiveValues()

  #Load saved objects if applicable
  observe({
    if(!is.null(continue_module$index_list())) {
      reactive_peaks$index_list <- continue_module$index_list()
    }
  })

  observeEvent(input$PeaksBoxSTART, {

    shinyjs::hide("NextButtonLadder")

    if(input$PeaksBoxIntro$collapsed == FALSE) {
      js$collapse("PeaksBoxIntro")
    }
    shinyjs::show("PeaksBox1")
    shinyjs::show("PeaksBox2")
  })

  observeEvent(input$NextButtonPeaks, {

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


  observeEvent(input$startbuttonFindPeaks, {
    tryCatch({
      withProgress(message = 'Finding Peaks ...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     reactive_peaks$peaks <- find_fragments(ladder_module$ladders(),
                                                            smoothing_window = input$smoothing_window,
                                                            minimum_peak_signal = input$minimum_peak_signal,
                                                            min_bp_size = input$min_bp_size,
                                                            max_bp_size = input$max_bp_size
                     )

                     reactive_peaks$metadata_added_list <- add_metadata(
                       fragments_list = reactive_peaks$peaks,
                       metadata_data.frame = upload_data$metadata_table(),
                       unique_id = "unique_id",
                       metrics_baseline_control = "metrics_baseline_control"
                     )

                     if (input$number_of_peaks_to_return == "1") {

                       reactive_peaks$allele_list <- find_alleles(reactive_peaks$metadata_added_list,
                                                                  number_of_peaks_to_return = 1,
                                                                  peak_region_size_gap_threshold = input$peak_region_size_gap_threshold,
                                                                  peak_region_height_threshold_multiplier = input$peak_region_height_threshold_multiplier)
                     }
                     else {
                       reactive_peaks$allele_list <- find_alleles(reactive_peaks$metadata_added_list,
                                                                  number_of_peaks_to_return = 2,
                                                                  peak_region_size_gap_threshold = input$peak_region_size_gap_threshold,
                                                                  peak_region_height_threshold_multiplier = input$peak_region_height_threshold_multiplier)
                     }

                     if (input$force_whole_repeat_units == "YES") {
                       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
                       reactive_peaks$repeats_list <- call_repeats(fragments_list = reactive_peaks$allele_list,
                                                                   assay_size_without_repeat = input$assay_size_without_repeat,
                                                                   repeat_size = input$repeat_size,
                                                                   repeat_calling_algorithm = input$repeat_calling_algorithm,
                                                                   repeat_calling_algorithm_size_window_around_allele = input$repeat_calling_algorithm_size_window_around_allele,
                                                                   repeat_calling_algorithm_peak_assignment_scan_window = input$repeat_calling_algorithm_peak_assignment_scan_window,
                                                                   repeat_calling_algorithm_size_period = input$repeat_calling_algorithm_size_period,
                                                                   force_whole_repeat_units = TRUE,
                                                                   repeat_length_correction = "from_metadata")
                       }
                       else {
                         reactive_peaks$repeats_list <- call_repeats(fragments_list = reactive_peaks$allele_list,
                                                                     assay_size_without_repeat = input$assay_size_without_repeat,
                                                                     repeat_size = input$repeat_size,
                                                                     repeat_calling_algorithm = input$repeat_calling_algorithm,
                                                                     repeat_calling_algorithm_size_window_around_allele = input$repeat_calling_algorithm_size_window_around_allele,
                                                                     repeat_calling_algorithm_peak_assignment_scan_window = input$repeat_calling_algorithm_peak_assignment_scan_window,
                                                                     repeat_calling_algorithm_size_period = input$repeat_calling_algorithm_size_period,
                                                                     force_whole_repeat_units = TRUE,
                                                                     repeat_length_correction = "none")
                       }
                     }
                     else if (input$force_whole_repeat_units == "NO") {
                       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
                       reactive_peaks$repeats_list <- call_repeats(fragments_list = reactive_peaks$allele_list,
                                                                   assay_size_without_repeat = input$assay_size_without_repeat,
                                                                   repeat_size = input$repeat_size,
                                                                   repeat_calling_algorithm = input$repeat_calling_algorithm,
                                                                   repeat_calling_algorithm_size_window_around_allele = input$repeat_calling_algorithm_size_window_around_allele,
                                                                   repeat_calling_algorithm_peak_assignment_scan_window = input$repeat_calling_algorithm_peak_assignment_scan_window,
                                                                   repeat_calling_algorithm_size_period = input$repeat_calling_algorithm_size_period,
                                                                   force_whole_repeat_units = FALSE,
                                                                   repeat_length_correction = "from_metadata"
                       )
                       }
                       else {
                         reactive_peaks$repeats_list <- call_repeats(fragments_list = reactive_peaks$allele_list,
                                                                     assay_size_without_repeat = input$assay_size_without_repeat,
                                                                     repeat_size = input$repeat_size,
                                                                     repeat_calling_algorithm = input$repeat_calling_algorithm,
                                                                     repeat_calling_algorithm_size_window_around_allele = input$repeat_calling_algorithm_size_window_around_allele,
                                                                     repeat_calling_algorithm_peak_assignment_scan_window = input$repeat_calling_algorithm_peak_assignment_scan_window,
                                                                     repeat_calling_algorithm_size_period = input$repeat_calling_algorithm_size_period,
                                                                     force_whole_repeat_units = FALSE,
                                                                     repeat_length_correction = "none"
                         )
                       }
                     }

                     if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
                       reactive_peaks$index_list <- assign_index_peaks(
                         reactive_peaks$repeats_list,
                         grouped = TRUE
                       )
                     }
                     else {
                       reactive_peaks$index_list <- assign_index_peaks(
                         reactive_peaks$repeats_list,
                         grouped = FALSE
                       )
                     }

                     shinyjs::show("NextButtonPeaks")

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = T),
                                                                      menuItem("Instability Metrics", icon = icon("water-ladder"), tabName = "InstabilityMetrics", selected = F,
                                                                               badgeColor = "green", badgeLabel = "new")))
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "Analysis Failed, please check if the previous steps were completed successfully.", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observe({
    if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
    updatePickerInput(session, "sample_subset", choices = upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id)

      shinyjs::show("sample_subset2")
      shinyjs::show("text_coordinates1")
      shinyjs::show("text_coordinates2")
      shinyjs::show("plot_traces_INDEX_UI")
    }
    else {
      updatePickerInput(session, "sample_subset", choices = upload_data$metadata_table()$unique_id)

      shinyjs::hide("sample_subset2")
      shinyjs::show("text_coordinates1")
      shinyjs::hide("text_coordinates2")
      shinyjs::hide("plot_traces_INDEX_UI")
    }
  })

  observeEvent(input$sample_subset, {
    updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)

  })

  ## export ladder fixes
  output$download_RDS <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_ladder_df_list", ".rds")
    },
    content = function(file) {
      saveRDS(reactive_peaks$metadata_added_list, file)
    }
  )

  relayout_data_peak <- reactive({
    event_data(event = "plotly_relayout", source = "plot_peak")
  })

  observe({
    if (!is.null(relayout_data_peak())) {
      reactive_peaks$index_list <- metrics_override_helper(
        reactive_peaks$index_list,
        index_override_dataframe = cbind(input$sample_subset, paste(relayout_data_peak()[1]))
      )
    }
  })

  output$text_coordinates1 <- renderUI({
    if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
    if (!is.null(relayout_data_peak()) && !is.null(reactive_peaks$index_list)) {
      h3(HTML(paste0("Index Peak: ", reactive_peaks$index_list[[input$sample_subset]]$index_repeat)))
      }
    else {
      h3(HTML(paste0("Index Peak: ", reactive_peaks$index_list[[input$sample_subset]]$index_repeat)))
    }
    }
    else {
      if (!is.null(relayout_data_peak()) && !is.null(reactive_peaks$index_list)) {
        h3(HTML(paste0("Index Peak: ", reactive_peaks$index_list[[input$sample_subset]]$index_repeat, " (You can drag the red line to change the index peak)")))
      }
      else {
        h3(HTML(paste0("Index Peak: ", reactive_peaks$index_list[[input$sample_subset]]$index_repeat, " (You can drag the red line to change the index peak)")))
      }
    }
  })

  output$text_coordinates2 <- renderUI({
    if (!is.null(relayout_data_peak()) && !is.null(reactive_peaks$index_list)) {
      h3(HTML(paste0("Index Peak: ", reactive_peaks$index_list[[input$sample_subset]]$index_repeat, " (You can drag the red line to change the index peak)")))
    }
    else {
      h3(HTML(paste0("Index Peak: ", reactive_peaks$index_list[[input$sample_subset]]$index_repeat, " (You can drag the red line to change the index peak)")))
    }
  })

  output$plot_tracesUI <- renderUI({
    plotlyOutput("plot_traces", height = (300 + input$HeightPeaks*20))
  })

  output$plot_traces <- renderPlotly({

    if (is.null(reactive_peaks$index_list)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$show_peaks == "YES") {
      show_peaks = TRUE
    }
    else {
      show_peaks = FALSE
    }

    fragments <- reactive_peaks$index_list[[input$sample_subset]]
    xlim = c(input$xlim1, input$xlim2)
    ylim = c(input$ylim1, input$ylim2)
    x_axis = input$x_axis
    height_color_threshold = 0.05
    plot_title = NULL

    if (is.null(fragments$trace_bp_df)) {
      stop(
        call. = FALSE,
        paste(fragments$unique_id, "This sample does not have trace data. Use fsa files as inputs to pipeline to plot trace.")
      )
    }

    #there must be a simpler way of the following if else below
    if (is.null(x_axis) && is.null(fragments$repeat_table_df)) {
      data <- fragments$trace_bp_df
      data$x <- data$size
      x_axis_label <- "Size"
    } else if (is.null(x_axis) && !is.null(fragments$repeat_table_df)) {
      data <- fragments$trace_bp_df
      data$x <- data$calculated_repeats
      x_axis_label <- "Repeats"
    } else if (x_axis == "size") {
      data <- fragments$trace_bp_df
      data$x <- data$size
      x_axis_label <- "Size"
    } else {
      data <- fragments$trace_bp_df
      data$x <- data$calculated_repeats
      x_axis_label <- "Repeats"
    }

    if (!is.null(xlim)) {
      data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
    }

    if (show_peaks == TRUE) {
      # add points onto plot showing peaks
      if (!is.null(fragments$peak_table_df) && show_peaks) {
        if (is.null(x_axis) && is.null(fragments$repeat_table_df)) {
          peak_table <- fragments$peak_table_df
          peak_table$x <- peak_table$size
        } else if (is.null(x_axis) && !is.null(fragments$repeat_table_df)) {
          peak_table <- fragments$repeat_table_df
          peak_table$x <- peak_table$repeats
        } else if (x_axis == "size") {
          peak_table <- fragments$peak_table_df
          peak_table$x <- peak_table$size
        } else {
          peak_table <- fragments$repeat_table_df
          peak_table$x <- peak_table$repeats
        }

        if (!is.null(xlim)) {
          peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
        }

        tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
        tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
        if (!is.null(fragments$get_alleles()$allele_1_height) && !is.na(fragments$get_alleles()$allele_1_height)) {
          tallest_peak_height <- fragments$get_alleles()$allele_1_height
          #find the tallest peak x axis position
          if (is.null(x_axis) && is.na(fragments$get_alleles()$allele_1_repeat)) {
            tallest_peak_x <- fragments$get_alleles()$allele_1_size
          } else if (is.null(x_axis) && !is.na(fragments$get_alleles()$allele_1_repeat)) {
            tallest_peak_x <- fragments$get_alleles()$allele_1_repeat
          } else if (x_axis == "size") {
            tallest_peak_x <- fragments$get_alleles()$allele_1_size
          } else {
            tallest_peak_x <- fragments$get_alleles()$allele_1_repeat
          }
        }

        peaks_above <- peak_table[which(peak_table$height > tallest_peak_height * height_color_threshold), ]
        peaks_below <- peak_table[which(peak_table$height < tallest_peak_height * height_color_threshold), ]

      }
    }

    if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {

    if (show_peaks == TRUE) {
      if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                height = (300 + input$HeightPeaks*20)) %>%
          add_markers(x = peaks_above$x,
                      y = peaks_above$height,
                      colors = "blue") %>%
          add_markers(x = peaks_below$x,
                      y = peaks_below$height,
                      colors = "purple") %>%
          add_markers(x = tallest_peak_x,
                      y = tallest_peak_height,
                      colors = "green") %>%
          add_segments(x = peak_table$repeats,
                       y = peak_table$height,
                       xend = peak_table$calculated_repeats,
                       yend = peak_table$height,
                       line = list(dash = "dash")) %>%
          layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(
                   #vertical line
                   list(type = "line", x0 = ifelse(!is.null(relayout_data_peak()), reactive_peaks$index_list[[input$sample_subset]]$index_repeat, reactive_peaks$index_list[[input$sample_subset2]]$index_repeat),
                        x1 = ifelse(!is.null(relayout_data_peak()), reactive_peaks$index_list[[input$sample_subset]]$index_repeat, reactive_peaks$index_list[[input$sample_subset2]]$index_repeat),
                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
          )
      }
    }
    else {
      plot_ly(data = data,
              x = ~x, y = ~signal,
              type = "scatter",
              mode = "lines",
              height = (300 + input$HeightPeaks*20)) %>%
        layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
               xaxis = list("Repeats",
                            range = xlim),
               yaxis = list("Signal",
                            range = ylim),
               shapes = list(
                 #vertical line
                 list(type = "line", x0 = ifelse(!is.null(relayout_data_peak()), reactive_peaks$index_list[[input$sample_subset]]$index_repeat, reactive_peaks$index_list[[input$sample_subset2]]$index_repeat),
                      x1 = ifelse(!is.null(relayout_data_peak()), reactive_peaks$index_list[[input$sample_subset]]$index_repeat, reactive_peaks$index_list[[input$sample_subset2]]$index_repeat),
                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
        )
    }
    }
    else {
      if (show_peaks == TRUE) {
        if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
          plot_ly(data = data,
                  x = ~x, y = ~signal,
                  type = "scatter",
                  mode = "lines",
                  source = "plot_peak",
                  height = (300 + input$HeightPeaks*20)) %>%
            add_markers(x = peaks_above$x,
                        y = peaks_above$height,
                        colors = "blue") %>%
            add_markers(x = peaks_below$x,
                        y = peaks_below$height,
                        colors = "purple") %>%
            add_markers(x = tallest_peak_x,
                        y = tallest_peak_height,
                        colors = "green") %>%
            add_segments(x = peak_table$repeats,
                         y = peak_table$height,
                         xend = peak_table$calculated_repeats,
                         yend = peak_table$height,
                         line = list(dash = "dash")) %>%
            layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
                   xaxis = list("Repeats",
                                range = xlim),
                   yaxis = list("Signal",
                                range = ylim),
                   shapes = list(
                     #vertical line
                     list(type = "line", x0 = reactive_peaks$index_list[[input$sample_subset]]$index_repeat,
                          x1 = reactive_peaks$index_list[[input$sample_subset]]$index_repeat,
                          y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
            ) %>%
            config(edits = list(shapePosition = TRUE))
        }
      }
      else {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                source = "plot_peak",
                height = (300 + input$HeightPeaks*20)) %>%
          layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(
                   #vertical line
                   list(type = "line", x0 = reactive_peaks$index_list[[input$sample_subset]]$index_repeat,
                        x1 = reactive_peaks$index_list[[input$sample_subset]]$index_repeat,
                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
          ) %>%
          config(edits = list(shapePosition = TRUE))
      }
    }
  })

  output$plot_traces_INDEX_UI <- renderUI({
    plotlyOutput("plot_traces_INDEX", height = (300 + input$HeightPeaks*20))
  })

  output$plot_traces_INDEX <- renderPlotly({
    validate(
      need(!is.null(input$sample_subset2), 'You do not have any baseline control samples in your metadata, please check your metadata file.'))

    if (is.null(reactive_peaks$index_list)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$show_peaks == "YES") {
      show_peaks = TRUE
    }
    else {
      show_peaks = FALSE
    }

    fragments <- reactive_peaks$index_list[[input$sample_subset2]]
    xlim = c(input$xlim1, input$xlim2)
    ylim = c(input$ylim1, input$ylim2)
    x_axis = input$x_axis
    height_color_threshold = 0.05
    plot_title = NULL

    if (is.null(fragments$trace_bp_df)) {
      stop(
        call. = FALSE,
        paste(fragments$unique_id, "This sample does not have trace data. Use fsa files as inputs to pipeline to plot trace.")
      )
    }

    #there must be a simpler way of the following if else below
    if (is.null(x_axis) && is.null(fragments$repeat_table_df)) {
      data <- fragments$trace_bp_df
      data$x <- data$size
      x_axis_label <- "Size"
    } else if (is.null(x_axis) && !is.null(fragments$repeat_table_df)) {
      data <- fragments$trace_bp_df
      data$x <- data$calculated_repeats
      x_axis_label <- "Repeats"
    } else if (x_axis == "size") {
      data <- fragments$trace_bp_df
      data$x <- data$size
      x_axis_label <- "Size"
    } else {
      data <- fragments$trace_bp_df
      data$x <- data$calculated_repeats
      x_axis_label <- "Repeats"
    }

    if (!is.null(xlim)) {
      data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
    }

    if (show_peaks == TRUE) {
      # add points onto plot showing peaks
      if (!is.null(fragments$peak_table_df) && show_peaks) {
        if (is.null(x_axis) && is.null(fragments$repeat_table_df)) {
          peak_table <- fragments$peak_table_df
          peak_table$x <- peak_table$size
        } else if (is.null(x_axis) && !is.null(fragments$repeat_table_df)) {
          peak_table <- fragments$repeat_table_df
          peak_table$x <- peak_table$repeats
        } else if (x_axis == "size") {
          peak_table <- fragments$peak_table_df
          peak_table$x <- peak_table$size
        } else {
          peak_table <- fragments$repeat_table_df
          peak_table$x <- peak_table$repeats
        }

        if (!is.null(xlim)) {
          peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
        }

        tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
        tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
        if (!is.null(fragments$get_alleles()$allele_1_height) && !is.na(fragments$get_alleles()$allele_1_height)) {
          tallest_peak_height <- fragments$get_alleles()$allele_1_height
          #find the tallest peak x axis position
          if (is.null(x_axis) && is.na(fragments$get_alleles()$allele_1_repeat)) {
            tallest_peak_x <- fragments$get_alleles()$allele_1_size
          } else if (is.null(x_axis) && !is.na(fragments$get_alleles()$allele_1_repeat)) {
            tallest_peak_x <- fragments$get_alleles()$allele_1_repeat
          } else if (x_axis == "size") {
            tallest_peak_x <- fragments$get_alleles()$allele_1_size
          } else {
            tallest_peak_x <- fragments$get_alleles()$allele_1_repeat
          }
        }

        peaks_above <- peak_table[which(peak_table$height > tallest_peak_height * height_color_threshold), ]
        peaks_below <- peak_table[which(peak_table$height < tallest_peak_height * height_color_threshold), ]

      }
    }

    if (show_peaks == TRUE) {
      if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                source = "plot_peak",
                height = (300 + input$HeightPeaks*20)) %>%
          add_markers(x = peaks_above$x,
                      y = peaks_above$height,
                      colors = "blue") %>%
          add_markers(x = peaks_below$x,
                      y = peaks_below$height,
                      colors = "purple") %>%
          add_markers(x = tallest_peak_x,
                      y = tallest_peak_height,
                      colors = "green") %>%
          add_segments(x = peak_table$repeats,
                       y = peak_table$height,
                       xend = peak_table$calculated_repeats,
                       yend = peak_table$height,
                       line = list(dash = "dash")) %>%
          layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
                 xaxis = list("Repeats",
                              range = xlim),
                 yaxis = list("Signal",
                              range = ylim),
                 shapes = list(
                   #vertical line
                   list(type = "line", x0 = reactive_peaks$index_list[[input$sample_subset2]]$index_repeat,
                        x1 = reactive_peaks$index_list[[input$sample_subset2]]$index_repeat,
                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
          ) %>%
          config(edits = list(shapePosition = TRUE))
      }
    }
    else {
      plot_ly(data = data,
              x = ~x, y = ~signal,
              type = "scatter",
              mode = "lines",
              source = "plot_peak",
              height = (300 + input$HeightPeaks*20)) %>%
        layout(title = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
               xaxis = list("Repeats",
                            range = xlim),
               yaxis = list("Signal",
                            range = ylim),
               shapes = list(
                 #vertical line
                 list(type = "line", x0 = reactive_peaks$index_list[[input$sample_subset2]]$index_repeat,
                      x1 = reactive_peaks$index_list[[input$sample_subset2]]$index_repeat,
                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
        ) %>%
        config(edits = list(shapePosition = TRUE))
    }
  })

  observe({
    if (is.null(reactive_peaks$index_list)) {
      shinyjs::show("text_no_data2")
    }
    else {
      shinyjs::hide("text_no_data2")
    }
  })

  output$text_no_data2 <- renderUI({
    h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Please select your inputs and press apply to start analysis.</b>'))
  })

  return(list(
    index_list = reactive(reactive_peaks$index_list),
    min_bp_size = reactive(input$min_bp_size),
    max_bp_size = reactive(input$max_bp_size),
    smoothing_window = reactive(input$smoothing_window),
    minimum_peak_signal = reactive(input$minimum_peak_signal),
    number_of_peaks_to_return = reactive(input$number_of_peaks_to_return),
    peak_region_size_gap_threshold = reactive(input$peak_region_size_gap_threshold),
    peak_region_height_threshold_multiplier = reactive(input$peak_region_height_threshold_multiplier),
    assay_size_without_repeat = reactive(input$assay_size_without_repeat),
    repeat_size = reactive(input$repeat_size),
    force_whole_repeat_units = reactive(input$force_whole_repeat_units),
    repeat_calling_algorithm = reactive(input$repeat_calling_algorithm),
    repeat_calling_algorithm_size_window_around_allele = reactive(input$repeat_calling_algorithm_size_window_around_allele),
    repeat_calling_algorithm_size_period = reactive(input$repeat_calling_algorithm_size_period),
    repeat_calling_algorithm_peak_assignment_scan_window = reactive(input$repeat_calling_algorithm_peak_assignment_scan_window)
  ))

}
