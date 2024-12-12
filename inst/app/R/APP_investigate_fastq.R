metrics2_box_ui1 <- function(id) {
  box(id = "Metrics2BoxIntro", title = strong("Calculate Instability Metrics"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,
      h4(includeHTML("data/metrics/metrics_landing_page.html")),
      br(), br(),

      fluidRow(column(3,
                      valueBox("NEW", actionBttn("Metrics2BoxSTART", "START",
                                                 style = "jelly",
                                                 color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

metrics2_box_ui2 <- function(id) {
  box(id = "Metrics2Box1", title = p("Settings", help_button("metrics2_params")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      materialSwitch("advancesettings_Metrics2", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Advanced Settings')), value = FALSE, status = "primary"),

      materialSwitch("group_controls2", label = h4(HTML('<h4 style = "text-align:justify;color:#000000;margin-top:-50px;">Grouped Index Assignment')), value = TRUE, status = "primary"),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Calculate Repeat Instability Metrics</b>')),
               fluidRow(
                 column(6,
                        numericInput("peak_threshold2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Threshold')),
                                     min = 0.01,
                                     value = 0.05, step = 0.01))
               ),
               fluidRow(
                 column(6,
                        numericInput("window_around_index_peak_min2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Window')),
                                     value = -50, step = 1)),
                 column(6,
                        numericInput("window_around_index_peak_max2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Maximum Window')),
                                     value = 50, step = 1))
               )
        )
      ),
      conditionalPanel(
        condition = 'input.advancesettings_Metrics2 == true',
        fluidRow(
          column(12,
                 h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Instability Metrics Table Output</b><br>')),
                 h4(HTML('<h4 style = "text-align:justify;color:#000000"><br>Percentile Range')),
                 fluidRow(
                   column(4,
                          numericInput("percentile_range1_2", label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">From')),
                                       value = 0.5,
                                       min = 0,
                                       step = 0.1)),
                   column(4,
                          numericInput("percentile_range2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">To')),
                                       min = 0,
                                       value = 0.95, step = 0.1)),
                   column(4,
                          numericInput("percentile_range3_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Interval')),
                                       min = 0,
                                       value = 0.05, step = 0.1))
                 ),
                 h4(HTML('<h4 style = "text-align:justify;color:#000000">Repeat Range')),
                 fluidRow(
                   column(4,
                          numericInput("repeat_range1_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">From')),
                                       min = 0,
                                       value = 0, step = 1)),
                   column(4,
                          numericInput("repeat_range2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">To')),
                                       min = 0,
                                       value = 20, step = 1)),
                   column(4,
                          numericInput("repeat_range3_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Interval')),
                                       min = 0,
                                       value = 5, step = 1))
                 )
          )
        )
      ),
      p(style="text-align: center;", actionBttn("startbuttonMetrics2", "APPLY", size = "lg"))
  )
}

metrics2_box_ui3 <- function(id) {
  box(id = "Metrics2Box2", title = p("Instability Metrics Table"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      p(style="text-align: right;", downloadButton("downloadmetrics2_fastq")),
      withSpinner(DT::dataTableOutput("metrics2_table", width = "100%", height = "400"))
  )
}

metrics2_box_ui4 <- function(id) {
  box(id = "Metrics2Box3", title = p("Plot"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(2,
               pickerInput("sample_subset_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = NULL)),

        column(1,
               actionButton("up_inv2", NULL, icon("arrow-up"), style='text-align: left; margin-top:50px; font-size:200%'),
               br(),
               actionButton("down_inv2", NULL, icon("arrow-down"), style='text-align: left; margin-top:-20px; font-size:200%')),
        column(3,
               radioGroupButtons(
                 inputId = "points_histo",
                 label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Plot Option')),
                 choices = c("Points",
                             "Histogram"),
                 justified = TRUE,
                 selected = "Histogram"
               )
        ),
        column(6,
               sliderInput("HeightPeaks_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Plot Controls</b>')),
               fluidRow(
                 column(1,
                        numericInput("xlim1_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                                     value = 0)),

                 column(1,
                        numericInput("xlim2_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                                     value = 250)),
                 column(1,
                        numericInput("ylim1_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                                     value = -2)),

                 column(1,
                        numericInput("ylim2_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                                     value = 2000)),
                 column(6,
                        prettySwitch("show_line_fastq", "Show Best Fit Line",
                                     value = FALSE, fill = T, status = "success", inline = T)
                 ),
                 column(3,
                        numericInput("span_fastq", HTML('<h5 style = "text-align:justify; margin-top:-50px;">Best Fit Line Smoothness'),
                                     value = 0.1, min = 0, step = 0.01)
                 )
                 ))),

      htmlOutput("text_no_data2_2"),

      fluidRow(
        column(6,
               numericInput("IndexRepeat1_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
                            value = NULL)
        )
      ),

      htmlOutput("plot_traces_final_2_UI"),

      fluidRow(
        column(3,
               pickerInput("sample_subset2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Index Samples')),
                           choices = NULL))
      ),

      fluidRow(
        column(6,
               numericInput("IndexRepeat2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
                            value = NULL)
        )
      ),
      htmlOutput("plot_traces_INDEX_2_UI"),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Index Repeat Table</b>')),
               p(style="text-align: right;", downloadButton("Index_Table_download2")),
               withSpinner(DT::dataTableOutput("Index_Table2"))
        )
      )
  )
}

metrics2_box_ui5 <- function(id) {
  box(id = "Metrics2Box4", title = p("Export Data"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Download Data Output</b><br>')),
      downloadBttn("downloadmetrics2_fastq_2", "Instability Metrics Table"), br(), br(),
      actionBttn("downloadPlotButton2", "All Plots", icon = icon("download")), br(), br(),

      h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Download R-related Files</b><br>')),
      downloadBttn("downloadRDS2",  "R Object"), br(), br(),
      downloadBttn("downloadlogs2", "Code For Current Analysis"), br(), br(),

      h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Save and Load</b><br>')),
      downloadBttn("downloadDataSave2", "Save .Rdata File For Re-load into traceShiny")
  )
}


metrics2_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module, metrics_module, analysis_module) {

  # help files
  help_click("metrics2_params", helpfile = "data/metrics/metrics_params.html")

  reactive_metrics2 <- reactiveValues()

  #Continue
  observe(
    if (!is.null(continue_module$Index_Table())) {
      reactive_metrics2$df <- continue_module$instability_metrics_fastq()
      reactive_metrics2$peak_list <- continue_module$peak_list()
      reactive_metrics2$sample_subset_metrics2 <- continue_module$sample_subset_metrics2()
      reactive_metrics2$sample_subset2_2 <- continue_module$sample_subset2_2()
      reactive_metrics2$Index_Table <- continue_module$Index_Table()
      reactive_metrics2$Index_Table_original <- continue_module$Index_Table_original()
    }
  )

  observeEvent(ignoreInit = F, list(input$MetadataUpload, input$SelectionButton, input$DataFSA, input$fastq, input$fileinputLOAD), {
    reactive_metrics2$df <- NULL
    reactive_metrics2$peak_list <- NULL
    reactive_metrics2$sample_subset_metrics2 <- NULL
    reactive_metrics2$sample_subset2_2 <- NULL
    reactive_metrics2$Index_Table <- NULL
    reactive_metrics2$Index_Table_original <- NULL
  })

  output$downloadlogs2 <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_logs", ".zip")
    },
    content = function(file) {

      tmpdir <- tempdir()
      setwd(tempdir())

      strLoad <- paste("library(trace)",
                       "##Data Upload",
                       "repeat_distribution <- readRDS('repeat_distribution.RDS')",
                       "metadata_table_fastq <- readRDS('metadata.RDS')", sep = "\n")

      strAddMeta <- ifelse(is.null(upload_data$metadata_table_fastq()),
                           paste0("##No Metadata was uploaded"),
                           paste0("##Add Metadata", "\n",
                                  "add_metadata(repeat_distribution, ",
                                  "metadata_data.frame = metadata_table_fastq)")
      )

      strIndex <- ifelse (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control)),
                          paste0("##Find Index", "\n",
                                 "assign_index_peaks(repeat_distribution, grouped = TRUE)"),
                          paste0("##Find Index", "\n",
                                 "assign_index_peaks(repeat_distribution, grouped = FALSE)")
      )

      strMetrics <- paste("##Calculate Instability Metrics", "\n",
                          paste0("metrics_dataframe <- calculate_instability_metrics(repeat_distribution, ",
                                 "peak_threshold = ", paste(input$peak_threshold2), ", ",
                                 "window_around_index_peak = c(", paste(input$window_around_index_peak_min2), ", ", paste(input$window_around_index_peak_max2), "))"))

      code <- paste(strLoad, strAddMeta, strIndex, strMetrics, sep = '\n')

      fs <- c("code.R", "Index_Table.csv", "metadata.RDS", "repeat_distribution.RDS")

      writeLines(text = code, "code.R")

      write.csv(reactive_metrics2$Index_Table[,c(1,4)], "Index_Table.csv", row.names = F, col.names = T)

      saveRDS(upload_data$metadata_table_fastq(), "metadata.RDS")

      saveRDS(reactive_metrics2$peak_list, "repeat_distribution.RDS")

      zip(zipfile=file, files=fs)
    }
  )

  #Download
  output$Index_Table_download2 <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Index_Repeat_Table.csv")
    },
    content = function(file) {
      write.csv(reactive_metrics2$Index_Table, file, row.names = F, col.names = T)
    }
  )

  output$downloadmetrics2_fastq <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_InstabilityMetricsTable", ".csv")
    },
    content = function(file) {
      write.csv(reactive_metrics2$df, file, row.names = F, col.names = T)
    }
  )

  output$downloadmetrics2_fastq_2 <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_InstabilityMetricsTable", ".csv")
    },
    content = function(file) {
      write.csv(reactive_metrics2$df, file, row.names = F, col.names = T)
    }
  )

  output$downloadRDS2 <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_RObjects", ".rds")
    },
    content = function(file) {
      saveRDS(reactive_metrics2$peak_list, file)
    }
  )

  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Instability_Plots.pdf")
    },

    content = function(file) {

      pdf(file,
          width = input$downloadPlotWidth2,
          height = input$downloadPlotHeight2)

      trace <- extract_fragments(reactive_metrics2$peak_list)

      if (input$points_histo == "Points") {
        for (i in unique(trace$unique_id)) {
        p <- ggplot(trace[which(trace$unique_id == i), ], aes(x=repeats, y = signal, colour = unique_id)) +
          xlim(c(input$xlim1_plot2, input$xlim2_plot2)) +
          ylim(c(input$ylim1_plot2, input$ylim2_plot2)) +
          ggtitle(i) +
          {if (input$show_line_fastq == T)
          list(geom_smooth(method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
          geom_point(show.legend = FALSE) +
          theme_bw()
        print(p)
        }
      }
      else {
        for (i in unique(upload_data$fastq()$SampleID)) {
        p <- ggplot(upload_data$fastq()[which(upload_data$fastq()$SampleID == i),], aes(x=`Repeat Length`, fill = SampleID)) +
          xlim(c(input$xlim1_plot2, input$xlim2_plot2)) +
          ylim(c(input$ylim1_plot2, input$ylim2_plot2)) +
          ggtitle(i) +
          {if (input$show_line_fastq == T)
          list(geom_smooth(inherit.aes=F, data = trace[which(trace$unique_id == i), ], aes(x=repeats, y = signal, colour = unique_id), method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
          geom_histogram(binwidth = 1, show.legend = FALSE, fill= "grey") +
          ylab("Signal") +
          theme_bw()
        print(p)
        }
      }

      dev.off()
    }
  )

  observeEvent(input$downloadPlotButton2, {
    showModal(modalDialog(
      title = strong("Download Plots"),
      h4(HTML('<h4 style = "text-align:justify;color:#000000"><br>Plot Sizes')),
      fluidRow(
        column(6,
               numericInput("downloadPlotHeight2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Height of plot')),
                            value = 7,
                            min = 0,
                            max = 20)),
        column(6,
               numericInput("downloadPlotWidth2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Width of plot')),
                            value = 7,
                            min = 0,
                            max = 20))
      ),

      h4(HTML('<h4 style = "text-align:justify;color:#000000"><br>Axis Limits')),
      fluidRow(
        column(3,
               numericInput("xlim1_plot2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                            value = 0)),

        column(3,
               numericInput("xlim2_plot2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                            value = 250)),
        column(3,
               numericInput("ylim1_plot2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                            value = 0)),

        column(3,
               numericInput("ylim2_plot2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                            value = 2000))),
      downloadBttn("downloadPlot2", "Download"),
      size = "m",
      easyClose = TRUE,
      footer = NULL
    ))
  })


  observeEvent(ignoreInit = TRUE, list(input$Metrics2BoxSTART, input$group_controls2), {

    #Transform data
    Instability <- upload_data$fastq()[,c(2,5)]
    Instability <- Instability %>% group_by(`Repeat Length`, SampleID) %>%
      summarise(signal = n())

    Instability <- Instability[,c(2,1,3)]
    colnames(Instability) <- c("unique_id", "Repeat Length", "Counts")
    Instability <- as.data.frame(Instability)

    peak_list <- trace::repeat_table_to_repeats(
      Instability,
      repeat_col = "Repeat Length",
      frequency_col = "Counts",
      unique_id = "unique_id"
    )

    if (!is.null(upload_data$metadata_table_fastq())) {

      add_metadata(
        fragments_list = peak_list,
        metadata_data.frame = upload_data$metadata_table_fastq(),
        unique_id = "unique_id",
        metrics_baseline_control = "metrics_baseline_control",
        batch_sample_modal_repeat = "batch_sample_modal_repeat"
      )
    }

    find_alleles_fastq(
      fragments_list = peak_list
    )

    if (is.null(upload_data$metadata_table_fastq())) {
      assign_index_peaks(
        peak_list,
        grouped = FALSE
      )
    }
    else if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
      if (input$group_controls2 == TRUE) {
        assign_index_peaks(
          peak_list,
          grouped = TRUE
        )
      }
      else {
        assign_index_peaks(
          peak_list,
          grouped = FALSE
        )
      }
    }
    else {
      assign_index_peaks(
        peak_list,
        grouped = FALSE
      )
    }

    reactive_metrics2$peak_list <- peak_list
    shinyjs::hide("NextButtonLoad2")
    reactive_metrics2$df <- NULL

    if(input$Metrics2BoxIntro$collapsed == FALSE) {
      js$collapse("Metrics2BoxIntro")
    }
    shinyjs::show("Metrics2Box1")
    shinyjs::show("Metrics2Box2")
    shinyjs::show("Metrics2Box3")
    shinyjs::hide("NextButtonMetrics2")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = T)))

    if (!is.null(upload_data$metadata_table_fastq())) {
      if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
        if (input$group_controls2 == TRUE) {
          updatePickerInput(session, "sample_subset2_2", choices = upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$metrics_group_id == upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2),]$metrics_group_id),][which(upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$metrics_group_id == upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
        }
        else {
          updatePickerInput(session, "sample_subset2_2", choices = NULL)
          updatePickerInput(session, "sample_subset_metrics2", choices = upload_data$metadata_table_fastq()$unique_id)
        }
      }
      else {
        updatePickerInput(session, "sample_subset2_2", choices = NULL)
        updatePickerInput(session, "sample_subset_metrics2", choices = upload_data$metadata_table_fastq()$unique_id)
      }
    }

    if (is.null(upload_data$metadata_table_fastq())) {
      reactive_metrics2$sample_subset_metrics2 <- names(reactive_metrics2$peak_list)
    }
    else if (!is.null(upload_data$metadata_table_fastq())) {
      if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
        if (input$group_controls2 == TRUE) {
          reactive_metrics2$sample_subset_metrics2 <- upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id
          reactive_metrics2$sample_subset2_2 <- upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$metrics_group_id == upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2),]$metrics_group_id),][which(upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$metrics_group_id == upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id
        }
        else {
          reactive_metrics2$sample_subset_metrics2 <- upload_data$metadata_table_fastq()$unique_id
        }
      }
      else {
        reactive_metrics2$sample_subset_metrics2 <- upload_data$metadata_table_fastq()$unique_id
      }
    }

    index <- list()

    for (i in 1:length(peak_list)) {
      index[[i]] <- as.data.frame(cbind(peak_list[[i]]$unique_id, peak_list[[i]]$metrics_group_id, peak_list[[i]]$get_allele_peak()$allele_repeat,
                                        peak_list[[i]]$get_index_peak()$index_repeat))
    }

    reactive_metrics2$Index_Table <- do.call(rbind, index)
    colnames(reactive_metrics2$Index_Table) <- c("Unique IDs", "Metrics Group ID", "Allele Repeat", "Index Repeat")

    reactive_metrics2$Index_Table_original <- reactive_metrics2$Index_Table

    updateNumericInput(session, "IndexRepeat1_2", value = reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics2),]$`Index Repeat`)
    updateNumericInput(session, "IndexRepeat2_2", value = reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset2_2),]$`Index Repeat`)
  })

  observe({
    if (!is.null(upload_data$metadata_table_fastq())) {
      if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
        if (input$group_controls2 == TRUE) {
          updatePickerInput(session, "sample_subset_metrics2", choices = upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id)
        }
        else {
          updatePickerInput(session, "sample_subset_metrics2", choices = upload_data$metadata_table_fastq()$unique_id)
        }
      }
      else {
        updatePickerInput(session, "sample_subset_metrics2", choices = upload_data$metadata_table_fastq()$unique_id)
      }
    }
  })

  observeEvent(input$NextButtonLoad2, {

    if (is.null(upload_data$metadata_table_fastq())) {
      shinyalert("WARNING!", "No metadata was loaded!", type = "warning", confirmButtonCol = "#337ab7")
    }

    reactive_metrics2$df <- NULL

    shinyjs::hide("NextButtonLoad2")

    if(input$Metrics2BoxIntro$collapsed == TRUE) {
      js$collapse("Metrics2BoxIntro")
    }
    shinyjs::hide("Metrics2Box1")
    shinyjs::hide("Metrics2Box2")
    shinyjs::hide("Metrics2Box3")
    shinyjs::hide("Metrics2Box4")
    shinyjs::hide("NextButtonMetrics2")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = T,
                                                              badgeColor = "green", badgeLabel = "new")))
  })

  observeEvent(input$sample_subset_metrics2, {
    if (!is.null(reactive_metrics2$Index_Table) && !is.null(input$sample_subset_metrics2)) {
      updateNumericInput(session, "IndexRepeat1_2", value = reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics2),]$`Index Repeat`)

      if (!is.null(input$sample_subset2_2) && !is.na(debounced_IndexRepeat1_2())) {
        updateNumericInput(session, "IndexRepeat2_2", value = reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset2_2),]$`Index Repeat`)
      }
    }
  })

  observeEvent(input$sample_subset_metrics2, {
    if (!is.null(upload_data$metadata_table_fastq())) {
      if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
        if (input$group_controls2 == TRUE) {
          updatePickerInput(session, "sample_subset2_2", choices = upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$metrics_group_id == upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2),]$metrics_group_id),][which(upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$metrics_group_id == upload_data$metadata_table_fastq()[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
        }
      }
    }
  })

  observe({
    if (!is.null(reactive_metrics2$peak_list) && !is.null(input$sample_subset_metrics2)) {
      updateNumericInput(session, "xlim1_metrics2", value = reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$get_allele_peak()$allele_repeat - 50)
      updateNumericInput(session, "xlim2_metrics2", value = reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$get_allele_peak()$allele_repeat + 50)
      updateNumericInput(session, "ylim1_metrics2", value = -2)
      updateNumericInput(session, "ylim2_metrics2", value = reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$get_allele_peak()$allele_signal + 300)
    }
  })

  observe({
    if (is.null(upload_data$metadata_table_fastq())) {
      shinyjs::hide("sample_subset2_2")
      shinyjs::show("IndexRepeat1_2")
      shinyjs::hide("IndexRepeat2_2")
      shinyjs::hide("plot_traces_INDEX_2_UI")
      updatePickerInput(session, "sample_subset2_2", choices = NULL)
      updateNumericInput(session, "IndexRepeat2_2", value = NULL)
    }
    else if (!is.null(upload_data$metadata_table_fastq())) {
      if (input$group_controls2 == TRUE) {
        if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
          shinyjs::hide("IndexRepeat1_2")
          shinyjs::show("IndexRepeat2_2")
          shinyjs::show("sample_subset2_2")
          shinyjs::show("plot_traces_INDEX_2_UI")
        }
        else {
          shinyjs::show("IndexRepeat1_2")
          shinyjs::hide("IndexRepeat2_2")
          shinyjs::hide("sample_subset2_2")
          shinyjs::hide("plot_traces_INDEX_2_UI")
        }

        if (is.null(input$sample_subset2_2)) {
          shinyjs::show("IndexRepeat1_2")
          shinyjs::hide("IndexRepeat2_2")
          shinyjs::hide("sample_subset2_2")
          shinyjs::hide("plot_traces_INDEX_2_UI")
        }
        else {
          shinyjs::hide("IndexRepeat1_2")
          shinyjs::show("IndexRepeat2_2")
          shinyjs::show("sample_subset2_2")
          shinyjs::show("plot_traces_INDEX_2_UI")
        }
      }
      else {
        shinyjs::hide("sample_subset2_2")
        shinyjs::show("IndexRepeat1_2")
        shinyjs::hide("IndexRepeat2_2")
        shinyjs::hide("plot_traces_INDEX_2_UI")
        updatePickerInput(session, "sample_subset2_2", choices = NULL)
        updateNumericInput(session, "IndexRepeat2_2", value = NULL)
      }
    }
  })

  observe({
    if (!is.null(reactive_metrics2$df)) {
      shinyjs::show("Metrics2Box4")
    }
    else {
      shinyjs::hide("Metrics2Box4")
    }
  })

  observe({
    if (is.null(upload_data$metadata_table_fastq())) {
      shinyjs::hide("group_controls2")
    }
    else {
      shinyjs::show("group_controls2")
    }
  })

  observeEvent(input$startbuttonMetrics2, {
    tryCatch({
      withProgress(message = 'Calculating Instability Metrics ...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     if (is.null(upload_data$metadata_table_fastq())) {
                       assign_index_peaks(
                         reactive_metrics2$peak_list,
                         grouped = FALSE,
                         index_override_dataframe = reactive_metrics2$Index_Table[,c(1,4)]
                       )
                     }
                     else if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
                       if (input$group_controls2 == TRUE) {
                         assign_index_peaks(
                           reactive_metrics2$peak_list,
                           grouped = TRUE,
                           index_override_dataframe = reactive_metrics2$Index_Table[,c(1,4)]
                         )
                       }
                       else {
                         assign_index_peaks(
                           reactive_metrics2$peak_list,
                           grouped = FALSE,
                           index_override_dataframe = reactive_metrics2$Index_Table[,c(1,4)]
                         )
                       }
                     }
                     else {
                       assign_index_peaks(
                         reactive_metrics2$peak_list,
                         grouped = FALSE,
                         index_override_dataframe = reactive_metrics2$Index_Table[,c(1,4)]
                       )
                     }

                     reactive_metrics2$df <- calculate_instability_metrics_fastq(
                       fragments_list = reactive_metrics2$peak_list,
                       peak_threshold = input$peak_threshold2,
                       window_around_index_peak = c(input$window_around_index_peak_min2, input$window_around_index_peak_max2),
                       percentile_range = seq(input$percentile_range1_2, input$percentile_range2_2, input$percentile_range3_2),
                       repeat_range = seq(input$repeat_range1_2 , input$repeat_range2_2, input$repeat_range3_2)
                     )

                     shinyjs::show("NextButtonMetrics2")

                     reactive_metrics2$Index_Table_original <- reactive_metrics2$Index_Table

                     if(input$Analysis2Box1$collapsed == TRUE) {
                       js$collapse("Analysis2Box1")
                     }

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                                      menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = T),
                                                                      menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis2", selected = F,
                                                                               badgeColor = "green", badgeLabel = "new")))
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
      reactive_metrics2$df <- NULL
    })
  })

  debounced_IndexRepeat1_2 <- debounce(input$IndexRepeat1_2, 2000)
  debounced_IndexRepeat2_2 <- debounce(input$IndexRepeat2_2, 2000)

  observeEvent(debounced_IndexRepeat1_2(), {
    if (!is.null(reactive_metrics2$Index_Table) && !is.null(reactive_metrics2$peak_list)) {
      reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics2),]$`Index Repeat` <- debounced_IndexRepeat1_2()
    }
  })

  observeEvent(debounced_IndexRepeat2_2(),  {
    if (!is.null(upload_data$fastq()) && !is.null(input$sample_subset2_2) && !is.na(debounced_IndexRepeat1_2())) {
      if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
        if (input$group_controls2 == TRUE) {
          if (!is.null(reactive_metrics2$Index_Table) && !is.null(reactive_metrics2$peak_list)) {
            reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics2),]$`Index Repeat` <- debounced_IndexRepeat2_2()
            reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset2_2),]$`Index Repeat` <- debounced_IndexRepeat2_2()
          }
        }
      }
    }
  })

  output$Index_Table2 <- DT::renderDataTable({

    datatable(reactive_metrics2$Index_Table,
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
    if (!is.null(reactive_metrics2$df)) {
      shinyjs::show("downloadmetrics2_fastq")
    }
    else {
      shinyjs::hide("downloadmetrics2_fastq")
    }
    if (identical(reactive_metrics2$Index_Table_original, reactive_metrics2$Index_Table)) {
      shinyjs::show("downloadmetrics2_fastq")
    }
    else {
      shinyjs::hide("downloadmetrics2_fastq")
    }
  })

  output$metrics2_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive_metrics2$df), 'You must perform the analysis first...'))
    validate(
      need(identical(reactive_metrics2$Index_Table_original, reactive_metrics2$Index_Table), 'Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.')
    )
    reactive_metrics2$df
  },  options = list(scrollX = TRUE))

  output$plot_traces_final_2_UI <- renderUI({
    plotlyOutput("plot_traces_final_2", height = (300 + input$HeightPeaks_metrics2*20))
  })

  output$plot_traces_final_2 <- renderPlotly({

    if (is.null(reactive_metrics2$peak_list)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    xlim = c(input$xlim1_metrics2, input$xlim2_metrics2)
    ylim = c(input$ylim1_metrics2, input$ylim2_metrics2)

    trace <- extract_fragments(reactive_metrics2$peak_list)
    trace <- trace[which(trace$unique_id == input$sample_subset_metrics2), ]

    if (is.null(upload_data$metadata_table_fastq())) {

      if (input$points_histo == "Points") {
        p <- ggplot(trace, aes(x=repeats, y = signal, colour = unique_id)) +
          xlim(xlim) +
          ylim(ylim) +
          {if(!is.na(debounced_IndexRepeat1_2()))
            list(geom_hline(yintercept = input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$.__enclos_env__$private$index_signal,
                     color = "black", size=1),
          geom_vline(xintercept = debounced_IndexRepeat1_2(),
                     color = "red", size=1),
          geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat1_2() + input$window_around_index_peak_min2,
                                       xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat1_2(),
                                       ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
          {if (input$show_line_fastq == T)
          list(geom_smooth(method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
          geom_point(show.legend = FALSE) +
          theme_bw()
      }
      else {
        p <- ggplot(upload_data$fastq()[which(upload_data$fastq()$SampleID == input$sample_subset_metrics2),], aes(x=`Repeat Length`, fill = SampleID)) +
          xlim(xlim) +
          ylim(ylim) +
          {if(!is.na(debounced_IndexRepeat1_2()))
            list(geom_hline(yintercept = input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
              geom_vline(xintercept = debounced_IndexRepeat1_2(),
                         color = "red", size=1),
              geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat1_2() + input$window_around_index_peak_min2,
                                           xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat1_2(),
                                           ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
          {if (input$show_line_fastq == T)
          list(geom_smooth(inherit.aes=F, data = trace, aes(x=repeats, y = signal, colour = unique_id), method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
          geom_histogram(binwidth = 1, show.legend = FALSE, fill= "grey") +
          ylab("Signal") +
          theme_bw()
      }
    }

    else if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control)) && !is.null(input$sample_subset2_2)) {

      if (input$group_controls2 == TRUE) {
        if (input$points_histo == "Points") {
          p <- ggplot(trace, aes(x=repeats, y = signal, colour = unique_id)) +
            xlim(xlim) +
            ylim(ylim) +
            {if(!is.na(debounced_IndexRepeat1_2()))
              list(geom_hline(yintercept = if(!is.na(debounced_IndexRepeat2_2())) input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset2_2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
            geom_vline(xintercept = debounced_IndexRepeat2_2(),
                       color = "red", size=1),
            geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat2_2() + input$window_around_index_peak_min2,
                                         xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat2_2(),
                                         ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
            {if (input$show_line_fastq == T)
            list(geom_smooth(method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
            geom_point(show.legend = FALSE) +
            theme_bw()
        }
        else {
          p <- ggplot(upload_data$fastq()[which(upload_data$fastq()$SampleID == input$sample_subset_metrics2),], aes(x=`Repeat Length`, fill = SampleID)) +
            xlim(xlim) +
            ylim(ylim) +
            {if(!is.na(debounced_IndexRepeat1_2()))
              list(geom_hline(yintercept = if(!is.na(debounced_IndexRepeat2_2())) input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset2_2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
            geom_vline(xintercept = debounced_IndexRepeat2_2(),
                       color = "red", size=1),
            geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat2_2() + input$window_around_index_peak_min2,
                                         xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat2_2(),
                                         ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
            {if (input$show_line_fastq == T)
            list(geom_smooth(inherit.aes=F, data = trace, aes(x=repeats, y = signal, colour = unique_id), method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
            geom_histogram(binwidth = 1, show.legend = FALSE, fill= "grey") +
            ylab("Signal") +
            theme_bw()
        }
      }
      else {
        if (input$points_histo == "Points") {
          p <- ggplot(trace, aes(x=repeats, y = signal, colour = unique_id)) +
            xlim(xlim) +
            ylim(ylim) +
            {if(!is.na(debounced_IndexRepeat1_2()))
              list(geom_hline(yintercept = if(!is.na(debounced_IndexRepeat1_2())) input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
            geom_vline(xintercept = debounced_IndexRepeat1_2(),
                       color = "red", size=1),
            geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat1_2() + input$window_around_index_peak_min2,
                                         xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat1_2(),
                                         ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
            {if (input$show_line_fastq == T)
            list(geom_smooth(method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
            geom_point(show.legend = FALSE) +
            theme_bw()
        }
        else {
          p <- ggplot(upload_data$fastq()[which(upload_data$fastq()$SampleID == input$sample_subset_metrics2),], aes(x=`Repeat Length`, fill = SampleID)) +
            xlim(xlim) +
            ylim(ylim) +
            {if(!is.na(debounced_IndexRepeat1_2()))
              list(geom_hline(yintercept = if(!is.na(debounced_IndexRepeat1_2())) input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
            geom_vline(xintercept = debounced_IndexRepeat1_2(),
                       color = "red", size=1),
            geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat1_2() + input$window_around_index_peak_min2,
                                         xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat1_2(),
                                         ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
            {if (input$show_line_fastq == T)
            list(geom_smooth(inherit.aes=F, data = trace, aes(x=repeats, y = signal, colour = unique_id), method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
            geom_histogram(binwidth = 1, show.legend = FALSE, fill= "grey") +
            ylab("Signal") +
            theme_bw()
        }
      }
    }
      else {
        if (input$points_histo == "Points") {
          p <- ggplot(trace, aes(x=repeats, y = signal, colour = unique_id)) +
            xlim(xlim) +
            ylim(ylim) +
            {if(!is.na(debounced_IndexRepeat1_2()))
            list(geom_hline(yintercept = if(!is.na(debounced_IndexRepeat1_2())) input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
            geom_vline(xintercept = debounced_IndexRepeat1_2(),
                       color = "red", size=1),
            geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat1_2() + input$window_around_index_peak_min2,
                                         xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat1_2(),
                                         ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
            {if (input$show_line_fastq == T)
            list(geom_smooth(method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
            geom_point(show.legend = FALSE) +
            theme_bw()
        }
        else {
          p <- ggplot(upload_data$fastq()[which(upload_data$fastq()$SampleID == input$sample_subset_metrics2),], aes(x=`Repeat Length`, fill = SampleID)) +
            xlim(xlim) +
            ylim(ylim) +
            {if(!is.na(debounced_IndexRepeat1_2()))
              list(geom_hline(yintercept = if(!is.na(debounced_IndexRepeat1_2())) input$peak_threshold2*reactive_metrics2$peak_list[[input$sample_subset_metrics2]]$.__enclos_env__$private$index_signal,
                       color = "black", size=1),
            geom_vline(xintercept = debounced_IndexRepeat1_2(),
                       color = "red", size=1),
            geom_rect(inherit.aes=F, aes(xmin=debounced_IndexRepeat1_2() + input$window_around_index_peak_min2,
                                         xmax=input$window_around_index_peak_max2 + debounced_IndexRepeat1_2(),
                                         ymin=0, ymax=input$ylim2_metrics2), alpha=0.1, fill="red"))} +
            {if (input$show_line_fastq == T)
            list(geom_smooth(inherit.aes=F, data = trace, aes(x=repeats, y = signal, colour = unique_id), method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
            geom_histogram(binwidth = 1, show.legend = FALSE, fill= "grey") +
            ylab("Signal") +
            theme_bw()
        }
      }

    ggplotly(p, height = (300 + input$HeightPeaks_metrics2*20))
  })

  output$plot_traces_INDEX_2_UI <- renderUI({
    plotlyOutput("plot_traces_INDEX_2", height = (300 + input$HeightPeaks_metrics2*20))
  })

  output$plot_traces_INDEX_2 <- renderPlotly({
    validate(
      need(!is.null(input$sample_subset2_2), 'You do not have any baseline control samples in your metadata, please check your metadata file.'))

    if (is.null(reactive_metrics2$peak_list)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    xlim = c(input$xlim1_metrics2, input$xlim2_metrics2)
    ylim = c(input$ylim1_metrics2, input$ylim2_metrics2)

    trace <- extract_fragments(reactive_metrics2$peak_list)
    trace <- trace[which(trace$unique_id == input$sample_subset2_2), ]

    if (input$points_histo == "Points") {
      p <- ggplot(trace, aes(x=repeats, y = signal, colour = unique_id)) +
        xlim(xlim) +
        ylim(ylim) +
        {if(!is.na(debounced_IndexRepeat2_2()))
          list(geom_vline(xintercept = debounced_IndexRepeat2_2(),
                          color = "red", size=1))} +
        {if (input$show_line_fastq == T)
        list(geom_smooth(method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
        geom_point(show.legend = FALSE) +
        theme_bw()
    }
    else {
      p <- ggplot(upload_data$fastq()[which(upload_data$fastq()$SampleID == input$sample_subset2_2),], aes(x=`Repeat Length`, fill = SampleID)) +
        xlim(xlim) +
        ylim(ylim) +
        {if(!is.na(debounced_IndexRepeat2_2()))
          list(geom_vline(xintercept = debounced_IndexRepeat2_2(),
                          color = "red", size=1))} +
        {if (input$show_line_fastq == T)
        list(geom_smooth(inherit.aes=F, data = trace, aes(x=repeats, y = signal, colour = unique_id), method = "loess", span=input$span_fastq, se = F, show.legend = FALSE))} +
        geom_histogram(binwidth = 1, show.legend = FALSE, fill= "grey") +
        ylab("Signal") +
        theme_bw()
    }

  })

  observeEvent(input$up_inv2, {
    tryCatch({
      if (is.null(upload_data$metadata_table_fastq())) {
        updatePickerInput(session, "sample_subset_metrics2", selected = if (which(names(reactive_metrics2$peak_list) == input$sample_subset_metrics2) - 1 == 0)
          reactive_metrics2$peak_list[[which(names(reactive_metrics2$peak_list) == input$sample_subset_metrics2)]]$unique_id
          else reactive_metrics2$peak_list[[which(names(reactive_metrics2$peak_list) == input$sample_subset_metrics2) - 1]]$unique_id)
      }
      else if (!is.null(upload_data$metadata_table_fastq())) {
        if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
          if (input$group_controls2 == TRUE) {
            if (!any(which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE) == input$Index_Table2_rows_selected)) {
              updatePickerInput(session, "sample_subset_metrics2", selected = if (which(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics2) - 1 == 0)
                upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics2)]
                else upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics2) - 1])
            }
          }
          else {
            updatePickerInput(session, "sample_subset_metrics2", selected = if (which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) - 1 == 0)
              upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2)]
              else upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) - 1])
          }
        }
        else {
          updatePickerInput(session, "sample_subset_metrics2", selected = if (which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) - 1 == 0)
            upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2)]
            else upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) - 1])
        }
      }
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$down_inv2, {
    tryCatch({
      if (is.null(upload_data$metadata_table_fastq())) {
        updatePickerInput(session, "sample_subset_metrics2", selected = if (which(names(reactive_metrics2$peak_list) == input$sample_subset_metrics2) + 1 > length(names(reactive_metrics2$peak_list)))
          reactive_metrics2$peak_list[[which(names(reactive_metrics2$peak_list) == input$sample_subset_metrics2)]]$unique_id
          else reactive_metrics2$peak_list[[which(names(reactive_metrics2$peak_list) == input$sample_subset_metrics2) + 1]]$unique_id)
      }
      else if (!is.null(upload_data$metadata_table_fastq())) {
        if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
          if (input$group_controls2 == TRUE) {
            updatePickerInput(session, "sample_subset_metrics2", selected = if (which(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics2) + 1 > length(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id))
              upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics2)]
              else upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table_fastq()[-which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics2) + 1])
          }
          else {
            updatePickerInput(session, "sample_subset_metrics2", selected = if (which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) + 1 > length(upload_data$metadata_table_fastq()$unique_id))
              upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2)]
              else upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) + 1])
          }
        }
        else {
          updatePickerInput(session, "sample_subset_metrics2", selected = if (which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) + 1 > length(upload_data$metadata_table_fastq()$unique_id))
            upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2)]
            else upload_data$metadata_table_fastq()$unique_id[which(upload_data$metadata_table_fastq()$unique_id == input$sample_subset_metrics2) + 1])
        }
      }
    },
    error = function(e) {
      shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$Index_Table2_rows_selected, {
    if (is.null(upload_data$metadata_table_fastq())) {
      updatePickerInput(session, "sample_subset_metrics2", selected = reactive_metrics2$peak_list[[input$Index_Table2_rows_selected]]$unique_id)
    }
    else if (!is.null(upload_data$metadata_table_fastq())) {
      if (any(grepl("TRUE", upload_data$metadata_table_fastq()$metrics_baseline_control))) {
        if (input$group_controls2 == TRUE) {
          if (!any(which(upload_data$metadata_table_fastq()$metrics_baseline_control == TRUE) == input$Index_Table2_rows_selected)) {
            updatePickerInput(session, "sample_subset_metrics2", selected = upload_data$metadata_table_fastq()$unique_id[input$Index_Table2_rows_selected])
          }
        }
        else {
          updatePickerInput(session, "sample_subset_metrics2", selected = upload_data$metadata_table_fastq()$unique_id[input$Index_Table2_rows_selected])
        }
      }
      else {
        updatePickerInput(session, "sample_subset_metrics2", selected = upload_data$metadata_table_fastq()$unique_id[input$Index_Table2_rows_selected])
      }
    }
  })

  observe({
    if (!is.null(reactive_metrics2$Index_Table_original) && !is.null(reactive_metrics2$Index_Table)) {
      if (identical(reactive_metrics2$Index_Table_original, reactive_metrics2$Index_Table)) {
        shinyjs::hide("text_no_data2_2")
      }
      else {
        shinyjs::show("text_no_data2_2")
      }
    }
  })

  output$text_no_data2_2 <- renderUI({
    if (is.null(reactive_metrics2$df)) {
      h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Instability Metrics not computed! Press Apply on the left to compute metrics</b>'))
    }
    else {
      h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.</b>'))
    }
  })

  ###SAVE FUNCTION
  output$downloadDataSave2 <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_SAVED_OBJECT_", sessionInfo()$otherPkgs$traceShiny$Version, ".Rdata")
    },
    content = function(file) {

      #Upload
      fastq = upload_data$fastq()
      All = upload_data$All()
      metadata_table_fastq = upload_data$metadata_table_fastq()
      DataUpload = upload_data$DataUpload()
      DataUploadMeta = upload_data$DataUploadMeta()

      peak_list <- reactive_metrics2$peak_list

      #Investigate
      instability_metrics_fastq <- reactive_metrics2$df
      Index_Table <- reactive_metrics2$Index_Table
      Index_Table_original <- reactive_metrics2$Index_Table_original
      sample_subset_metrics2 <- reactive_metrics2$sample_subset_metrics2
      sample_subset2_2 <- reactive_metrics2$sample_subset2_2
      peak_threshold2 <- input$peak_threshold2
      repeat_range1 <- input$repeat_range1_2
      repeat_range2 <- input$repeat_range2_2
      repeat_range3 <- input$repeat_range3_2
      percentile_range1 <- input$percentile_range1_2
      percentile_range2 <- input$percentile_range2_2
      percentile_range3 <- input$percentile_range3_2
      window_around_index_peak_min <- input$window_around_index_peak_min2
      window_around_index_peak_max <- input$window_around_index_peak_max2
      group_controls2 <- input$group_controls2

      #Package Version
      Package_version <- sessionInfo()$otherPkgs$traceShiny$Version

      save("All", "fastq", "metadata_table_fastq", "DataUpload", "DataUploadMeta", "peak_list",
           "instability_metrics_fastq", "peak_threshold2", "window_around_index_peak_min", "window_around_index_peak_max", "repeat_range1", "repeat_range2", "repeat_range3", "percentile_range1", "percentile_range2", "percentile_range3",
           "sample_subset2_2", "sample_subset_metrics2", "Package_version", "Index_Table", "Index_Table_original", "group_controls2",
           file = file)
    }
  )

  return(list(
    metrics_table = reactive(reactive_metrics2$df),
    peak_list = reactive(reactive_metrics2$peak_list)
  ))
}
