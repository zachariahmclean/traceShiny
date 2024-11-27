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

      materialSwitch("group_controls", label = h4(HTML('<h4 style = "text-align:justify;color:#000000;margin-top:-50px;">Grouped Index Assignment')), value = TRUE, status = "primary"),

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

      p(style="text-align: right;", downloadButton("downloadmetrics2")),
      withSpinner(DT::dataTableOutput("metrics_table", width = "100%", height = "400"))
  )
}

metrics_box_ui4 <- function(id) {
  box(id = "MetricsBox3", title = p("Traces"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(2,
               pickerInput("sample_subset_metrics", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = NULL)),

        column(1,
               actionButton("up_inv", NULL, icon("arrow-up"), style='text-align: left; margin-top:50px; font-size:200%'),
               br(),
               actionButton("down_inv", NULL, icon("arrow-down"), style='text-align: left; margin-top:-20px; font-size:200%')),

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

      htmlOutput("text_no_data2"),

      fluidRow(
        column(6,
               numericInput("IndexRepeat1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
                            value = NULL)
        )
      ),

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
      htmlOutput("plot_traces_INDEX_UI"),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Index Repeat Table</b>')),
               p(style="text-align: right;", downloadButton("Index_Table_download")),
               withSpinner(DT::dataTableOutput("Index_Table"))
        )
      )
  )
}

metrics_box_ui5 <- function(id) {
  box(id = "MetricsBox4", title = p("Export Data"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      downloadBttn("downloadRDS", "Download R Object For Further Analysis"), br(), br(),
      downloadBttn("downloadmetrics", "Download Instability Metrics Table"), br(), br(),
      actionBttn("downloadPlotButton", "Download All Plots", icon = icon("download")), br(), br(),
      downloadBttn("downloadlogs", "Download Code For Current Analysis"), br(), br(),
      downloadBttn("downloadDataSave", "Save .Rdata File For Re-load into traceShiny")
  )
}


metrics_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module) {

  # help files
  help_click("metrics_params", helpfile = "data/metrics/metrics_params.html")

  reactive_metrics <- reactiveValues()

  observe(
    if (!is.null(continue_module$instability_metrics())) {
      reactive_metrics$df <- continue_module$instability_metrics()
      reactive_metrics$sample_subset_metrics <- continue_module$sample_subset_metrics()
      reactive_metrics$sample_subset2 <- continue_module$sample_subset2()
      reactive_metrics$Index_Table <- continue_module$Index_Table()
      reactive_metrics$Index_Table_original <- continue_module$Index_Table_original()
    }
  )

  observeEvent(ignoreInit = F, list(input$MetadataUpload, input$SelectionButton, input$DataFSA, input$fastq, input$fileinputLOAD), {
    reactive_metrics$df <- NULL
    reactive_metrics$sample_subset_metrics <- NULL
    reactive_metrics$sample_subset2 <- NULL
    reactive_metrics$Index_Table <- NULL
    reactive_metrics$Index_Table_original <- NULL
  })

  output$downloadlogs <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_logs", ".zip")
    },
    content = function(file) {

      tmpdir <- tempdir()
      setwd(tempdir())

      strLoad <- paste("library(trace)",
                       "##Data Upload",
                       "fsa_list <- readRDS('fsa_files.RDS')",
                       "metadata_table <- readRDS('metadata.RDS')",
                       "Ladder_Fix <- readRDS('Ladder_fix.RDS')", sep = "\n")

      strLadders <- paste("##Find Ladders", "\n",
                          paste0("find_ladders(fsa_list, ladder_channel = ", "'", paste(input$LadderChannel), "'", ", ",
                                 "signal_channel = ", "'", paste(input$SignalChannel), "'", ", ",
                                 "ladder_sizes = c(", paste(upload_data$laddertable()[which(upload_data$laddertable() == upload_data$laddertable()$Ladder_ID[1]),]$Expected_ladder_peaks), "), ",
                                 paste0(if(input$spikeswitch == T) "ladder_start_scan = NULL, " else paste0("ladder_start_scan = ", paste(input$spikelocation), ", ")),
                                 paste0(if(input$minimum_peak_signal_ladder == T) "minimum_peak_signal = NULL, " else paste0("minimum_peak_signal = ", paste(input$minimum_peak_signal_number), ", ")),
                                 paste0(if(input$scan_subset == T) "scan_subset = NULL, " else paste0("scan_subset = c(", paste(input$scan_subset1), ", ", paste(input$scan_subset2), "), ")),
                                 "ladder_selection_window = ", paste(input$ladderselectionwindow), ", ",
                                 "max_combinations = ", paste(input$maxcombinations), ")"))

      strLaddersFix <- paste("##Fix Ladders",
                             paste("fix_ladders_manual(fsa_list, Ladder_Fix)"), sep = "\n"
      )

      strPeaks <- paste("##Find Peaks", "\n",
                        paste0("fragments_list <- find_fragments(fsa_list, ",
                               "smoothing_window = ", paste(input$smoothing_window), ", ",
                               "minimum_peak_signal = ", paste(input$minimum_peak_signal), ", ",
                               "min_bp_size = ", paste(input$min_bp_size), ", ",
                               "max_bp_size = ", paste(input$max_bp_size), ")"))

      strAddMeta <- ifelse(is.null(upload_data$metadata_table()),
                           paste0("##No Metadata was uploaded"),
                           paste0("##Add Metadata", "\n",
                                  "add_metadata(fragments_list, ",
                                  "metadata_data.frame = metadata_table)")
      )

      strAlleles <- paste("##Find Alleles", "\n",
                          paste0("find_alleles(fragments_list, ",
                                 "peak_region_size_gap_threshold = ", paste(input$peak_region_size_gap_threshold), ", ",
                                 "peak_region_height_threshold_multiplier = ", paste(input$peak_region_height_threshold_multiplier), ")"))

      strRepeats <- paste("##Find Repeats", "\n",
                          paste0("call_repeats(fragments_list, assay_size_without_repeat = ", paste(input$assay_size_without_repeat), ", ",
                                 "repeat_size = ", paste(input$repeat_size), ", ",
                                 "force_repeat_pattern = ", "'", paste(input$force_repeat_pattern), "'", ", ",
                                 "force_repeat_pattern_size_window = ", paste(input$force_repeat_pattern_size_window), ", ",
                                 "force_repeat_pattern_size_period = ", paste(input$force_repeat_pattern_size_period), ", ",
                                 "force_whole_repeat_units = ", paste(ifelse(input$force_whole_repeat_units == "YES", "TRUE", "FALSE")), ", ",
                                 "correction = ", "'", paste(input$batchcorrectionswitch), "')"
                          ))

      strIndex <- ifelse (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control)),
                          paste0("##Find Index", "\n",
                                 "assign_index_peaks(fragments_list, grouped = TRUE)"),
                          paste0("##Find Index", "\n",
                                 "assign_index_peaks(fragments_list, grouped = FALSE)")
      )

      strMetrics <- paste("##Calculate Instability Metrics", "\n",
                          paste0("metrics_dataframe <- calculate_instability_metrics(fragments_list, ",
                                 "peak_threshold = ", paste(input$peak_threshold), ", ",
                                 "window_around_index_peak = c(", paste(input$window_around_index_peak_min), ", ", paste(input$window_around_index_peak_max), "))"))

      code <- paste(strLoad, strLadders, strLaddersFix, strPeaks, strAddMeta, strAlleles, strRepeats, strIndex, strMetrics, sep = '\n')

      fs <- c("code.R", "Index_Table.csv", "metadata.RDS", "fsa_files.RDS", "Ladder_fix.RDS")

      writeLines(text = code, "code.R")

      write.csv(reactive_metrics$Index_Table[,c(1,4)], "Index_Table.csv", row.names = F, col.names = T)

      saveRDS(upload_data$metadata_table(), "metadata.RDS")

      saveRDS(upload_data$fsa_list(), "fsa_files.RDS")

      ladderfix <- list()

      for (i in 1:length(ladder_module$ladders())) {

        ladderfix[[i]] <- ladder_module$ladders()[[i]]$ladder_df
        names(ladderfix)[i] <- ladder_module$ladders()[[i]]$unique_id
      }

      saveRDS(ladderfix, "Ladder_fix.RDS")

      zip(zipfile=file, files=fs)
    }
  )

  #Download
  output$Index_Table_download <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Index_Repeat_Table.csv")
    },
    content = function(file) {
      write.csv(reactive_metrics$Index_Table, file, row.names = F, col.names = T)
    }
  )

  output$downloadmetrics2 <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_InstabilityMetricsTable", ".csv")
    },
    content = function(file) {
      write.csv(reactive_metrics$df, file, row.names = F, col.names = T)
    }
  )

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

  observeEvent(ignoreInit = TRUE, list(input$MetricsBoxSTART, input$group_controls), {

    if (is.null(upload_data$metadata_table())) {
      assign_index_peaks(
        peaks_module$index_list(),
        grouped = FALSE
      )
    }
    else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
      if (input$group_controls == TRUE) {
        assign_index_peaks(
          peaks_module$index_list(),
          grouped = TRUE
        )
      }
      else {
        assign_index_peaks(
          peaks_module$index_list(),
          grouped = FALSE
        )
      }
    }
    else {
      assign_index_peaks(
        peaks_module$index_list(),
        grouped = FALSE
      )
    }

    shinyjs::hide("NextButtonPeaks")
    reactive_metrics$df <- NULL

    if(input$MetricsBoxIntro$collapsed == FALSE) {
      js$collapse("MetricsBoxIntro")
    }
    shinyjs::show("MetricsBox1")
    shinyjs::show("MetricsBox2")
    shinyjs::show("MetricsBox3")
    shinyjs::hide("NextButtonMetrics")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T)))

    if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        if (input$group_controls == TRUE) {
          updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
        }
        else {
          updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
          updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
        }
      }
      else {
        updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
        updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
      }
    }

    if (is.null(upload_data$metadata_table())) {
      reactive_metrics$sample_subset_metrics <- names(upload_data$fsa_list())
    }
    else if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        if (input$group_controls == TRUE) {
          reactive_metrics$sample_subset_metrics <- upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id
          reactive_metrics$sample_subset2 <- upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id
        }
        else {
          reactive_metrics$sample_subset_metrics <- upload_data$metadata_table()$unique_id
        }
      }
      else {
        reactive_metrics$sample_subset_metrics <- upload_data$metadata_table()$unique_id
      }
    }

    index <- list()

    for (i in 1:length(peaks_module$index_list())) {
      index[[i]] <- as.data.frame(cbind(peaks_module$index_list()[[i]]$unique_id, peaks_module$index_list()[[i]]$metrics_group_id, peaks_module$index_list()[[i]]$get_allele_peak()$allele_repeat,
                                        peaks_module$index_list()[[i]]$get_index_peak()$index_repeat))
    }

    reactive_metrics$Index_Table <- do.call(rbind, index)
    colnames(reactive_metrics$Index_Table) <- c("Unique IDs", "Metrics Group ID", "Allele Repeat", "Index Repeat")

    reactive_metrics$Index_Table_original <- reactive_metrics$Index_Table
  })

  observe({
    if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        if (input$group_controls == TRUE) {
          updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id)
        }
        else {
          updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
        }
      }
      else {
        updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
      }
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
    shinyjs::hide("NextButtonMetrics")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T,
                                                              badgeColor = "green", badgeLabel = "new")))
  })

  observe({
    if (!is.null(peaks_module$index_list()) && !is.null(reactive_metrics$Index_Table) && !is.null(input$sample_subset_metrics)) {
      updateNumericInput(session, "IndexRepeat1", value = reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat`)

      if (!is.null(input$sample_subset2) && !is.na(input$IndexRepeat1)) {
        updateNumericInput(session, "IndexRepeat2", value = reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset2),]$`Index Repeat`)
      }
    }
  })

  observeEvent(input$sample_subset_metrics, {
    if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        if (input$group_controls == TRUE) {
          updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
        }
      }
    }
  })

  observe({
    if (!is.null(peaks_module$index_list()) && !is.null(input$sample_subset_metrics)) {
      updateNumericInput(session, "xlim1_metrics", value = peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_repeat - 50)
      updateNumericInput(session, "xlim2_metrics", value = peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_repeat + 50)
      updateNumericInput(session, "ylim1_metrics", value = -200)
      updateNumericInput(session, "ylim2_metrics", value = peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height + 300)
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
      if (input$group_controls == TRUE) {
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

        if (is.null(input$sample_subset2)) {
          shinyjs::show("IndexRepeat1")
          shinyjs::hide("IndexRepeat2")
          shinyjs::hide("sample_subset2")
          shinyjs::hide("plot_traces_INDEX_UI")
        }
        else {
          shinyjs::hide("IndexRepeat1")
          shinyjs::show("IndexRepeat2")
          shinyjs::show("sample_subset2")
          shinyjs::show("plot_traces_INDEX_UI")
        }
      }
      else {
        shinyjs::hide("sample_subset2")
        shinyjs::show("IndexRepeat1")
        shinyjs::hide("IndexRepeat2")
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

  observe({
    if (is.null(upload_data$metadata_table())) {
      shinyjs::hide("group_controls")
    }
    else {
      shinyjs::show("group_controls")
    }
  })

  observeEvent(input$startbuttonMetrics, {
    tryCatch({
      withProgress(message = 'Calculating Instability Metrics ...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     if (is.null(upload_data$metadata_table())) {
                       assign_index_peaks(
                         peaks_module$index_list(),
                         grouped = FALSE
                       )
                     }
                     else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
                       if (input$group_controls == TRUE) {
                         assign_index_peaks(
                           peaks_module$index_list(),
                           grouped = TRUE,
                           index_override_dataframe = reactive_metrics$Index_Table[,c(1,4)]
                         )
                       }
                       else {
                         assign_index_peaks(
                           peaks_module$index_list(),
                           grouped = FALSE
                         )
                       }
                     }
                     else {
                       assign_index_peaks(
                         peaks_module$index_list(),
                         grouped = FALSE
                       )
                     }

                     reactive_metrics$df <- calculate_instability_metrics(
                       fragments_list = peaks_module$index_list(),
                       peak_threshold = input$peak_threshold,
                       window_around_index_peak = c(input$window_around_index_peak_min, input$window_around_index_peak_max),
                       percentile_range = seq(input$percentile_range1, input$percentile_range2, input$percentile_range3),
                       repeat_range = seq(input$repeat_range1 , input$repeat_range2, input$repeat_range3)
                     )

                     shinyjs::show("NextButtonMetrics")

                     reactive_metrics$Index_Table_original <- reactive_metrics$Index_Table

                     if(input$AnalysisBox1$collapsed == TRUE) {
                       js$collapse("AnalysisBox1")
                     }

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                                      menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T),
                                                                      menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis", selected = F,
                                                                               badgeColor = "green", badgeLabel = "new")))
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
      reactive_metrics$df <- NULL
    })
  })

  observeEvent(input$IndexRepeat1, {
    if (!is.null(reactive_metrics$Index_Table) && !is.null(peaks_module$index_list())) {
      shinyjs::disable("IndexRepeat1")
      reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat` <- input$IndexRepeat1
      shinyjs::enable("IndexRepeat1")
    }
  })

  observeEvent(input$IndexRepeat2,  {
    if (!is.null(ladder_module$ladders()) && !is.null(input$sample_subset2) && !is.na(input$IndexRepeat1)) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        if (input$group_controls == TRUE) {
          if (!is.null(reactive_metrics$Index_Table) && !is.null(peaks_module$index_list())) {
            shinyjs::disable("IndexRepeat2")
            reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat` <- input$IndexRepeat2
            reactive_metrics$Index_Table[which(reactive_metrics$Index_Table$`Unique IDs` == input$sample_subset2),]$`Index Repeat` <- input$IndexRepeat2
            shinyjs::enable("IndexRepeat2")
          }
        }
      }
    }
  })

  output$Index_Table <- DT::renderDataTable({

    datatable(reactive_metrics$Index_Table,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              selection = 'single',
              rownames = FALSE)

  },  options = list(scrollX = TRUE))

  observe({
    if (!is.null(reactive_metrics$df)) {
      shinyjs::show("downloadmetrics2")
    }
    else {
      shinyjs::hide("downloadmetrics2")
    }
    if (identical(reactive_metrics$Index_Table_original, reactive_metrics$Index_Table)) {
      shinyjs::show("downloadmetrics2")
    }
    else {
      shinyjs::hide("downloadmetrics2")
    }
  })

  output$metrics_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_metrics$df), 'You must perform the analysis first...'))
    validate(
      need(identical(reactive_metrics$Index_Table_original, reactive_metrics$Index_Table), 'Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.')
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
    height_color_threshold = input$minimum_peak_signal
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

      peaks_above <- peak_table[which(peak_table$height > height_color_threshold), ]
      peaks_below <- peak_table[which(peak_table$height < height_color_threshold), ]

    }

    if (is.null(upload_data$metadata_table())) {
      if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
        if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
          plot_ly(data = data,
                  x = ~x, y = ~signal,
                  type = "scatter",
                  mode = "lines",
                  height = (300 + input$HeightPeaks_metrics*20),
                  name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
            add_trace(x = peaks_above$x,
                      y = peaks_above$height,
                      mode = "markers",
                      name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Above Threshold")) %>%
            add_trace(x = peaks_below$x,
                      y = peaks_below$height,
                      mode = "markers",
                      name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
            add_trace(x = tallest_peak_x,
                      y = tallest_peak_height,
                      mode = "markers",
                      name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
            add_segments(x = peak_table$repeats,
                         y = peak_table$height,
                         xend = peak_table$calculated_repeats,
                         yend = peak_table$height,
                         line = list(dash = "dash"),
                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list(title = "Repeats",
                                range = xlim),
                   yaxis = list(title = "Signal",
                                range = ylim),
                   shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
                height = (300 + input$HeightPeaks_metrics*20),
                name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                 xaxis = list(title = "Repeats",
                              range = xlim),
                 yaxis = list(title = "Signal",
                              range = ylim),
                 shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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

    else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control)) && !is.null(input$sample_subset2)) {

      if (input$group_controls == TRUE) {
        if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
          if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
            plot_ly(data = data,
                    x = ~x, y = ~signal,
                    type = "scatter",
                    mode = "lines",
                    height = (300 + input$HeightPeaks_metrics*20),
                    name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
              add_trace(x = peaks_above$x,
                        y = peaks_above$height,
                        mode = "markers",
                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peak Above Threshold")) %>%
              add_trace(x = peaks_below$x,
                        y = peaks_below$height,
                        mode = "markers",
                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
              add_trace(x = tallest_peak_x,
                        y = tallest_peak_height,
                        mode = "markers",
                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
              add_segments(x = peak_table$repeats,
                           y = peak_table$height,
                           xend = peak_table$calculated_repeats,
                           yend = peak_table$height,
                           line = list(dash = "dash"),
                           name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
              layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                     xaxis = list(title = "Repeats",
                                  range = xlim),
                     yaxis = list(title = "Signal",
                                  range = ylim),
                     shapes = if(!is.na(input$IndexRepeat2)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
                  height = (300 + input$HeightPeaks_metrics*20),
                  name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list("Repeats",
                                range = xlim),
                   yaxis = list("Signal",
                                range = ylim),
                   shapes = if(!is.na(input$IndexRepeat2)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
                    height = (300 + input$HeightPeaks_metrics*20),
                    name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
              add_trace(x = peaks_above$x,
                        y = peaks_above$height,
                        mode = "markers",
                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Above Threshold")) %>%
              add_trace(x = peaks_below$x,
                        y = peaks_below$height,
                        mode = "markers",
                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
              add_trace(x = tallest_peak_x,
                        y = tallest_peak_height,
                        mode = "markers",
                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
              add_segments(x = peak_table$repeats,
                           y = peak_table$height,
                           xend = peak_table$calculated_repeats,
                           yend = peak_table$height,
                           line = list(dash = "dash"),
                           name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
              layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                     xaxis = list(title = "Repeats",
                                  range = xlim),
                     yaxis = list(title = "Signal",
                                  range = ylim),
                     shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
                  height = (300 + input$HeightPeaks_metrics*20),
                  name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list(title = "Repeats",
                                range = xlim),
                   yaxis = list(title = "Signal",
                                range = ylim),
                   shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
    }
    else {
      if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
        if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
          plot_ly(data = data,
                  x = ~x, y = ~signal,
                  type = "scatter",
                  mode = "lines",
                  height = (300 + input$HeightPeaks_metrics*20),
                  name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
            add_trace(x = peaks_above$x,
                      y = peaks_above$height,
                      mode = "markers",
                      name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Above Threshold")) %>%
            add_trace(x = peaks_below$x,
                      y = peaks_below$height,
                      mode = "markers",
                      name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
            add_trace(x = tallest_peak_x,
                      y = tallest_peak_height,
                      mode = "markers",
                      name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
            add_segments(x = peak_table$repeats,
                         y = peak_table$height,
                         xend = peak_table$calculated_repeats,
                         yend = peak_table$height,
                         line = list(dash = "dash"),
                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
            layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                   xaxis = list(title = "Repeats",
                                range = xlim),
                   yaxis = list(title = "Signal",
                                range = ylim),
                   shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
                height = (300 + input$HeightPeaks_metrics*20),
                name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
                 xaxis = list(title = "Repeats",
                              range = xlim),
                 yaxis = list(title = "Signal",
                              range = ylim),
                 shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
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
    height_color_threshold = input$minimum_peak_signal
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

      peaks_above <- peak_table[which(peak_table$height > height_color_threshold), ]
      peaks_below <- peak_table[which(peak_table$height < height_color_threshold), ]

    }

    if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
      if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
        plot_ly(data = data,
                x = ~x, y = ~signal,
                type = "scatter",
                mode = "lines",
                source = "plot_peak2",
                height = (300 + input$HeightPeaks_metrics*20),
                name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), "")) %>%
          add_trace(x = peaks_above$x,
                    y = peaks_above$height,
                    mode = "markers",
                    name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Peaks Above Threshold")) %>%
          add_trace(x = peaks_below$x,
                    y = peaks_below$height,
                    mode = "markers",
                    name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Peaks Below Threshold")) %>%
          add_trace(x = tallest_peak_x,
                    y = tallest_peak_height,
                    mode = "markers",
                    name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Modal Peak")) %>%
          add_segments(x = peak_table$repeats,
                       y = peak_table$height,
                       xend = peak_table$calculated_repeats,
                       yend = peak_table$height,
                       line = list(dash = "dash"),
                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Force Whole Repeats")) %>%
          layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset2]]$unique_id, plot_title),
                 xaxis = list(title = "Repeats",
                              range = xlim),
                 yaxis = list(title = "Signal",
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
              height = (300 + input$HeightPeaks_metrics*20),
              name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), "")) %>%
        layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset2]]$unique_id, plot_title),
               xaxis = list(title = "Repeats",
                            range = xlim),
               yaxis = list(title = "Signal",
                            range = ylim),
               shapes = list(
                 #vertical line
                 list(type = "line", x0 = input$IndexRepeat2,
                      x1 = input$IndexRepeat2,
                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
        )
    }
  })

  observeEvent(input$up_inv, {
    tryCatch({
      if (is.null(upload_data$metadata_table())) {
        updatePickerInput(session, "sample_subset_metrics", selected = if (which(names(upload_data$fsa_list()) == input$sample_subset_metrics) - 1 == 0)
          upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics)]]$unique_id
          else upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics) - 1]]$unique_id)
      }
      else if (!is.null(upload_data$metadata_table())) {
        if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
          if (input$group_controls == TRUE) {
            if (!any(which(upload_data$metadata_table()$metrics_baseline_control == TRUE) == input$Index_Table_rows_selected)) {
              updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) - 1 == 0)
                upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics)]
                else upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) - 1])
            }
          }
          else {
            updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1 == 0)
              upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
              else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1])
          }
        }
        else {
          updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1 == 0)
            upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
            else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1])
        }
      }
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$down_inv, {
    tryCatch({
      if (is.null(upload_data$metadata_table())) {
        updatePickerInput(session, "sample_subset_metrics", selected = if (which(names(upload_data$fsa_list()) == input$sample_subset_metrics) + 1 > length(names(upload_data$fsa_list())))
          upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics)]]$unique_id
          else upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics) + 1]]$unique_id)
      }
      else if (!is.null(upload_data$metadata_table())) {
        if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
          if (input$group_controls == TRUE) {
            updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) + 1 > length(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id))
              upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics)]
              else upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) + 1])
          }
          else {
            updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1 > length(upload_data$metadata_table()$unique_id))
              upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
              else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1])
          }
        }
        else {
          updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1 > length(upload_data$metadata_table()$unique_id))
            upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
            else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1])
        }
      }
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$Index_Table_rows_selected, {
    if (is.null(upload_data$metadata_table())) {
      updatePickerInput(session, "sample_subset_metrics", selected = upload_data$fsa_list()[[input$Index_Table_rows_selected]]$unique_id)
    }
    else if (!is.null(upload_data$metadata_table())) {
      if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
        if (input$group_controls == TRUE) {
          if (!any(which(upload_data$metadata_table()$metrics_baseline_control == TRUE) == input$Index_Table_rows_selected)) {
            updatePickerInput(session, "sample_subset_metrics", selected = upload_data$metadata_table()$unique_id[input$Index_Table_rows_selected])
          }
        }
        else {
          updatePickerInput(session, "sample_subset_metrics", selected = upload_data$metadata_table()$unique_id[input$Index_Table_rows_selected])
        }
      }
      else {
        updatePickerInput(session, "sample_subset_metrics", selected = upload_data$metadata_table()$unique_id[input$Index_Table_rows_selected])
      }
    }
  })

  observe({
    if (!is.null(reactive_metrics$Index_Table_original) && !is.null(reactive_metrics$Index_Table)) {
      if (identical(reactive_metrics$Index_Table_original, reactive_metrics$Index_Table)) {
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
      ladderselectionwindow <- ladder_module$ladderselectionwindow()
      maxcombinations <- ladder_module$maxcombinations()
      minimum_peak_signal_ladder <- ladder_module$minimum_peak_signal_ladder()
      minimum_peak_signal_number <- ladder_module$minimum_peak_signal_number()
      scan_subset <- ladder_module$scan_subset()
      scan_subset1 <- ladder_module$scan_subset1()
      scan_subset2 <- ladder_module$scan_subset2()

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
      force_repeat_pattern <- peaks_module$force_repeat_pattern()
      force_repeat_pattern_size_period <- peaks_module$force_repeat_pattern_size_period()
      force_repeat_pattern_size_window <- peaks_module$force_repeat_pattern_size_window()
      sample_traces_size <- peaks_module$sample_traces_size()
      sample_traces_repeats <- peaks_module$sample_traces_repeats()

      #Investigate
      instability_metrics <- reactive_metrics$df
      Index_Table <- reactive_metrics$Index_Table
      Index_Table_original <- reactive_metrics$Index_Table_original
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
      group_controls <- input$group_controls

      #Package Version
      Package_version <- sessionInfo()$otherPkgs$traceShiny$Version


      save("laddertable", "fsa_list", "metadata_table", "DataUpload", "DataUploadMeta", "Ladder_switch",
           "ladders", "scan", "size", "LadderChannel", "SignalChannel", "LadderSizes", "spikeswitch", "spikelocation", "ladderselectionwindow", "maxcombinations", "minimum_peak_signal_ladder", "minimum_peak_signal_number", "scan_subset", "scan_subset1", "scan_subset2",
           "index_list", "min_bp_size", "max_bp_size", "smoothing_window_peaks", "minimum_peak_signal", "batchcorrectionswitch", "peak_region_size_gap_threshold",
           "peak_region_height_threshold_multiplier", "assay_size_without_repeat", "repeat_size", "force_whole_repeat_units", "force_repeat_pattern",
           "force_repeat_pattern_size_period", "force_repeat_pattern_size_window", "sample_traces_size", "sample_traces_repeats",
           "instability_metrics", "peak_threshold", "window_around_index_peak_min", "window_around_index_peak_max", "repeat_range1", "repeat_range2", "repeat_range3", "percentile_range1", "percentile_range2", "percentile_range3",
           "sample_subset2", "sample_subset_metrics", "Package_version", "Index_Table", "Index_Table_original", "group_controls",
           file = file)
    }
  )

  return(list(
    metrics_table = reactive(reactive_metrics$df)
  ))
}
