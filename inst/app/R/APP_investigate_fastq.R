# metrics2_box_ui1 <- function(id) {
#   box(id = "Metrics2BoxIntro", title = strong("Calculate Instability Metrics"), status = "warning", solidHeader = F,
#       collapsible = T, collapsed = T, width = 12,
#       h4(includeHTML("data/metrics/metrics_landing_page.html")),
#       br(), br(),
#
#       fluidRow(column(3,
#                       valueBox("NEW", actionBttn("Metrics2BoxSTART", "START",
#                                                  style = "jelly",
#                                                  color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
#       ))
# }
#
# metrics2_box_ui2 <- function(id) {
#   box(id = "Metrics2Box1", title = p("Settings", help_button("metrics2_params")), status = "warning", solidHeader = F,
#       collapsible = T, width = NULL,
#
#       materialSwitch("advancesettings_Metrics2", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Advanced Settings')), value = FALSE, status = "primary"),
#
#       materialSwitch("group_controls2", label = h4(HTML('<h4 style = "text-align:justify;color:#000000;margin-top:-50px;">Grouped Index Assignment')), value = TRUE, status = "primary"),
#
#       fluidRow(
#         column(12,
#                h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Calculate Repeat Instability Metrics</b>')),
#                fluidRow(
#                  column(6,
#                         numericInput("peak_threshold2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Peak Threshold')),
#                                      min = 0.01,
#                                      value = 0.05, step = 0.01))
#                ),
#                fluidRow(
#                  column(6,
#                         numericInput("window_around_index_peak_min2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Minimum Window')),
#                                      value = -5, step = 1)),
#                  column(6,
#                         numericInput("window_around_index_peak_max2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Maximum Window')),
#                                      value = 40, step = 1))
#                )
#         )
#       ),
#       conditionalPanel(
#         condition = 'input.advancesettings_Metrics2 == true',
#         fluidRow(
#           column(12,
#                  h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Instability Metrics Table Output</b><br>')),
#                  h4(HTML('<h4 style = "text-align:justify;color:#000000"><br>Percentile Range')),
#                  fluidRow(
#                    column(4,
#                           numericInput("percentile_range1_2", label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">From')),
#                                        value = 0.5,
#                                        min = 0,
#                                        step = 0.1)),
#                    column(4,
#                           numericInput("percentile_range2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">To')),
#                                        min = 0,
#                                        value = 0.95, step = 0.1)),
#                    column(4,
#                           numericInput("percentile_range3_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Interval')),
#                                        min = 0,
#                                        value = 0.05, step = 0.1))
#                  ),
#                  h4(HTML('<h4 style = "text-align:justify;color:#000000">Repeat Range')),
#                  fluidRow(
#                    column(4,
#                           numericInput("repeat_range1_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">From')),
#                                        min = 0,
#                                        value = 0, step = 1)),
#                    column(4,
#                           numericInput("repeat_range2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">To')),
#                                        min = 0,
#                                        value = 20, step = 1)),
#                    column(4,
#                           numericInput("repeat_range3_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Interval')),
#                                        min = 0,
#                                        value = 5, step = 1))
#                  ),
#           )
#         )
#       ),
#       p(style="text-align: center;", actionBttn("startbuttonMetrics2", "APPLY", size = "lg"))
#   )
# }
#
# metrics2_box_ui3 <- function(id) {
#   box(id = "Metrics2Box2", title = p("Instability Metrics Table"), status = "warning", solidHeader = F,
#       collapsible = T, width = NULL,
#
#       p(style="text-align: right;", downloadButton("downloadmetrics2_2")),
#       withSpinner(DT::dataTableOutput("metrics2_table", width = "100%", height = "400"))
#   )
# }
#
# metrics2_box_ui4 <- function(id) {
#   box(id = "Metrics2Box3", title = p("Histogram"), status = "warning", solidHeader = F,
#       collapsible = T, width = NULL,
#
#       fluidRow(
#         column(2,
#                pickerInput("sample_subset_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
#                            choices = NULL)),
#
#         column(1,
#                actionButton("up_inv2", NULL, icon("arrow-up"), style='text-align: left; margin-top:50px; font-size:200%'),
#                br(),
#                actionButton("down_inv2", NULL, icon("arrow-down"), style='text-align: left; margin-top:-20px; font-size:200%')),
#
#         column(3,
#                radioGroupButtons(
#                  inputId = "show_peaks_metrics2",
#                  label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Show Peaks')),
#                  choices = c("YES",
#                              "NO"),
#                  justified = TRUE,
#                  selected = "YES"
#                )
#         ),
#         column(6,
#                sliderInput("HeightPeaks_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
#                            min = 1, max = 100,
#                            value = 20, step = 1)
#         )),
#       fluidRow(
#         column(12,
#                h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Plot Controls</b>')),
#                fluidRow(
#                  column(1,
#                         numericInput("xlim1_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
#                                      value = 0)),
#
#                  column(1,
#                         numericInput("xlim2_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
#                                      value = 250)),
#                  column(1,
#                         numericInput("ylim1_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
#                                      value = 0)),
#
#                  column(1,
#                         numericInput("ylim2_metrics2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
#                                      value = 2000))))),
#
#       htmlOutput("text_no_data2_2"),
#
#       fluidRow(
#         column(6,
#                numericInput("IndexRepeat1_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
#                             value = NULL)
#         )
#       ),
#
#       htmlOutput("plot_traces_final_UI_2"),
#
#       fluidRow(
#         column(3,
#                pickerInput("sample_subset2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Index Samples')),
#                            choices = NULL))
#       ),
#
#       fluidRow(
#         column(6,
#                numericInput("IndexRepeat2_2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Index Repeat (can be manually changed)')),
#                             value = NULL)
#         )
#       ),
#       htmlOutput("plot_traces_INDEX_UI_2"),
#
#       fluidRow(
#         column(12,
#                h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Index Repeat Table</b>')),
#                p(style="text-align: right;", downloadButton("Index_Table_download_2")),
#                withSpinner(DT::dataTableOutput("Index_Table_2"))
#         )
#       )
#   )
# }
#
# metrics2_box_ui5 <- function(id) {
#   box(id = "Metrics2Box4", title = p("Export Data"), status = "warning", solidHeader = F,
#       collapsible = T, width = NULL,
#
#       downloadBttn("downloadRDS2", "Download R Object For Further Analysis"), br(), br(),
#       downloadBttn("downloadmetrics2", "Download Instability Metrics Table"), br(), br(),
#       actionBttn("downloadPlotButton2", "Download All Plots", icon = icon("download")), br(), br(),
#       downloadBttn("downloadlogs2", "Download Code For Current Analysis"), br(), br(),
#       downloadBttn("downloadDataSave2", "Save .Rdata File For Re-load into traceShiny")
#   )
# }
#
#
# metrics2_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module, metrics_module, analysis_module) {
#
#   # help files
#   help_click("metrics2_params", helpfile = "data/metrics/metrics_params.html")
#
#   reactive_metrics2 <- reactiveValues()
#
#   observe(
#     if (!is.null(continue_module$instability_metrics())) {
#       reactive_metrics2$df <- continue_module$instability_metrics()
#       reactive_metrics2$sample_subset_metrics <- continue_module$sample_subset_metrics()
#       reactive_metrics2$sample_subset2 <- continue_module$sample_subset2()
#       reactive_metrics2$Index_Table <- continue_module$Index_Table()
#       reactive_metrics2$Index_Table_original <- continue_module$Index_Table_original()
#     }
#   )
#
#   output$downloadlogs2 <- shiny::downloadHandler(
#     filename = function() {
#       paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_logs", ".zip")
#     },
#     content = function(file) {
#
#       tmpdir <- tempdir()
#       setwd(tempdir())
#
#       strLoad <- paste("library(trace)",
#                        "##Data Upload",
#                        "fsa_list <- readRDS('fsa_files.RDS')",
#                        "metadata_table <- readRDS('metadata.RDS')", sep = "\n")
#
#       strAddMeta <- ifelse(is.null(upload_data$metadata_table()),
#                            paste0("##No Metadata was uploaded"),
#                            paste0("##Add Metadata", "\n",
#                                   "add_metadata(fragments_list, ",
#                                   "metadata_data.frame = metadata_table)")
#       )
#
#       strIndex <- ifelse (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control)),
#                           paste0("##Find Index", "\n",
#                                  "assign_index_peaks(fragments_list, grouped = TRUE)"),
#                           paste0("##Find Index", "\n",
#                                  "assign_index_peaks(fragments_list, grouped = FALSE)")
#       )
#
#       strMetrics <- paste("##Calculate Instability Metrics", "\n",
#                           paste0("metrics_dataframe <- calculate_instability_metrics(fragments_list, ",
#                                  "peak_threshold = ", paste(input$peak_threshold), ", ",
#                                  "window_around_index_peak = c(", paste(input$window_around_index_peak_min), ", ", paste(input$window_around_index_peak_max), "))"))
#
#       code <- paste(strLoad, strAddMeta, strIndex, strMetrics, sep = '\n')
#
#       fs <- c("code.R", "Index_Table.csv", "metadata.RDS", "fsa_files.RDS")
#
#       writeLines(text = code, "code.R")
#
#       write.csv(reactive_metrics2$Index_Table[,c(1,4)], "Index_Table.csv", row.names = F, col.names = T)
#
#       saveRDS(upload_data$metadata_table(), "metadata.RDS")
#
#       saveRDS(upload_data$fsa_list(), "fsa_files.RDS")
#
#       zip(zipfile=file, files=fs)
#     }
#   )
#
#   #Download
#   output$Index_Table_download2 <- shiny::downloadHandler(
#     filename = function() {
#       paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Index_Repeat_Table.csv")
#     },
#     content = function(file) {
#       write.csv(reactive_metrics2$Index_Table, file, row.names = F, col.names = T)
#     }
#   )
#
#   output$downloadmetrics2 <- shiny::downloadHandler(
#     filename = function() {
#       paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_InstabilityMetricsTable", ".csv")
#     },
#     content = function(file) {
#       write.csv(reactive_metrics2$df, file, row.names = F, col.names = T)
#     }
#   )
#
#   output$downloadmetrics2 <- shiny::downloadHandler(
#     filename = function() {
#       paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_InstabilityMetricsTable", ".csv")
#     },
#     content = function(file) {
#       write.csv(reactive_metrics2$df, file, row.names = F, col.names = T)
#     }
#   )
#
#   output$downloadRDS2 <- shiny::downloadHandler(
#     filename = function() {
#       paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_RObjects", ".rds")
#     },
#     content = function(file) {
#       saveRDS(peaks_module$index_list(), file)
#     }
#   )
#
#   # output$downloadPlot2 <- downloadHandler(
#   #   filename = function() {
#   #     "Instability_Plots.pdf"
#   #   },
#   #
#   #   content = function(file) {
#   #   }
#   # )
#
#   # observeEvent(input$downloadPlotButton, {
#   #   showModal(modalDialog(
#   #     title = strong("Download Plots"),
#   #     numericInput("downloadPlotHeight", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Height of plot')),
#   #                  value = 7,
#   #                  min = 0,
#   #                  max = 20),
#   #     numericInput("downloadPlotWidth", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Width of plot')),
#   #                  value = 7,
#   #                  min = 0,
#   #                  max = 20),
#   #     downloadBttn("downloadPlot", "Download"),
#   #     size = "s",
#   #     easyClose = TRUE,
#   #     footer = NULL
#   #   ))
#   # })
#
#   observeEvent(ignoreInit = TRUE, list(input$Metrics2BoxSTART, input$group_controls2), {
#
#     if (is.null(upload_data$metadata_table())) {
#       assign_index_peaks(
#         peaks_module$index_list(),
#         grouped = FALSE
#       )
#     }
#     else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#       if (input$group_controls == TRUE) {
#         assign_index_peaks(
#           peaks_module$index_list(),
#           grouped = TRUE
#         )
#       }
#       else {
#         assign_index_peaks(
#           peaks_module$index_list(),
#           grouped = FALSE
#         )
#       }
#     }
#     else {
#       assign_index_peaks(
#         peaks_module$index_list(),
#         grouped = FALSE
#       )
#     }
#
#     shinyjs::hide("NextButtonPeaks")
#     reactive_metrics2$df <- NULL
#
#     if(input$MetricsBoxIntro$collapsed == FALSE) {
#       js$collapse("MetricsBoxIntro")
#     }
#     shinyjs::show("MetricsBox1")
#     shinyjs::show("MetricsBox2")
#     shinyjs::show("MetricsBox3")
#     shinyjs::hide("NextButtonMetrics")
#
#     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
#                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
#                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
#                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
#                                                      menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T)))
#
#     if (!is.null(upload_data$metadata_table())) {
#       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#         if (input$group_controls == TRUE) {
#           updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
#         }
#         else {
#           updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
#           updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
#         }
#       }
#       else {
#         updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
#         updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
#       }
#     }
#
#     if (is.null(upload_data$metadata_table())) {
#       reactive_metrics2$sample_subset_metrics <- names(upload_data$fsa_list())
#     }
#     else if (!is.null(upload_data$metadata_table())) {
#       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#         if (input$group_controls == TRUE) {
#           reactive_metrics2$sample_subset_metrics <- upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id
#           reactive_metrics2$sample_subset2 <- upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id
#         }
#         else {
#           reactive_metrics2$sample_subset_metrics <- upload_data$metadata_table()$unique_id
#         }
#       }
#       else {
#         reactive_metrics2$sample_subset_metrics <- upload_data$metadata_table()$unique_id
#       }
#     }
#
#     index <- list()
#
#     for (i in 1:length(peaks_module$index_list())) {
#       index[[i]] <- as.data.frame(cbind(peaks_module$index_list()[[i]]$unique_id, peaks_module$index_list()[[i]]$metrics_group_id, peaks_module$index_list()[[i]]$get_allele_peak()$allele_repeat, peaks_module$index_list()[[i]]$get_index_peak()$index_repeat))
#     }
#
#     reactive_metrics2$Index_Table <- do.call(rbind, index)
#     colnames(reactive_metrics2$Index_Table) <- c("Unique IDs", "Metrics Group ID", "Allele Repeat", "Index Repeat")
#
#     reactive_metrics2$Index_Table_original <- reactive_metrics2$Index_Table
#   })
#
#   observe({
#     if (!is.null(upload_data$metadata_table())) {
#       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#         if (input$group_controls == TRUE) {
#           updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id)
#         }
#         else {
#           updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
#         }
#       }
#       else {
#         updatePickerInput(session, "sample_subset_metrics", choices = upload_data$metadata_table()$unique_id)
#       }
#     }
#   })
#
#   observeEvent(input$NextButtonPeaks, {
#
#     reactive_metrics2$df <- NULL
#
#     shinyjs::hide("NextButtonLadder")
#
#     if(input$MetricsBoxIntro$collapsed == TRUE) {
#       js$collapse("MetricsBoxIntro")
#     }
#     shinyjs::hide("MetricsBox1")
#     shinyjs::hide("MetricsBox2")
#     shinyjs::hide("MetricsBox3")
#     shinyjs::hide("MetricsBox4")
#     shinyjs::hide("NextButtonMetrics")
#
#     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
#                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
#                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
#                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
#                                                      menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T,
#                                                               badgeColor = "green", badgeLabel = "new")))
#   })
#
#   observe({
#     if (!is.null(peaks_module$index_list()) && !is.null(reactive_metrics2$Index_Table) && !is.null(input$sample_subset_metrics)) {
#       updateNumericInput(session, "IndexRepeat1", value = reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat`)
#
#       if (!is.null(input$sample_subset2) && !is.na(input$IndexRepeat1)) {
#         updateNumericInput(session, "IndexRepeat2", value = reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset2),]$`Index Repeat`)
#       }
#     }
#   })
#
#   observeEvent(input$sample_subset_metrics, {
#     if (!is.null(upload_data$metadata_table()) && !is.na(input$IndexRepeat1)) {
#       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#         if (input$group_controls == TRUE) {
#           updatePickerInput(session, "sample_subset2", choices = upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),][which(upload_data$metadata_table()[which(upload_data$metadata_table()$metrics_group_id == upload_data$metadata_table()[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics),]$metrics_group_id),]$metrics_baseline_control == "TRUE"),]$unique_id)
#         }
#       }
#     }
#   })
#
#   observe({
#     if (!is.null(peaks_module$index_list()) && !is.null(input$sample_subset_metrics)) {
#       updateNumericInput(session, "xlim1_metrics", value = peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_repeat - 50)
#       updateNumericInput(session, "xlim2_metrics", value = peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_repeat + 50)
#       updateNumericInput(session, "ylim1_metrics", value = -200)
#       updateNumericInput(session, "ylim2_metrics", value = peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height + 100)
#     }
#   })
#
#   observe({
#     if (is.null(upload_data$metadata_table())) {
#       shinyjs::hide("sample_subset2")
#       shinyjs::show("IndexRepeat1")
#       shinyjs::hide("IndexRepeat2")
#       shinyjs::hide("plot_traces_INDEX_UI")
#     }
#     else if (!is.null(upload_data$metadata_table())) {
#       if (input$group_controls == TRUE) {
#         if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#           shinyjs::hide("IndexRepeat1")
#           shinyjs::show("IndexRepeat2")
#           shinyjs::show("sample_subset2")
#           shinyjs::show("plot_traces_INDEX_UI")
#         }
#         else {
#           shinyjs::show("IndexRepeat1")
#           shinyjs::hide("IndexRepeat2")
#           shinyjs::hide("sample_subset2")
#           shinyjs::hide("plot_traces_INDEX_UI")
#         }
#
#         if (is.null(input$sample_subset2)) {
#           shinyjs::show("IndexRepeat1")
#           shinyjs::hide("IndexRepeat2")
#           shinyjs::hide("sample_subset2")
#           shinyjs::hide("plot_traces_INDEX_UI")
#         }
#         else {
#           shinyjs::hide("IndexRepeat1")
#           shinyjs::show("IndexRepeat2")
#           shinyjs::show("sample_subset2")
#           shinyjs::show("plot_traces_INDEX_UI")
#         }
#       }
#       else {
#         shinyjs::hide("sample_subset2")
#         shinyjs::show("IndexRepeat1")
#         shinyjs::hide("IndexRepeat2")
#         shinyjs::hide("plot_traces_INDEX_UI")
#       }
#     }
#   })
#
#   observe({
#     if (!is.null(reactive_metrics2$df)) {
#       shinyjs::show("MetricsBox4")
#     }
#     else {
#       shinyjs::hide("MetricsBox4")
#     }
#   })
#
#   observe({
#     if (is.null(upload_data$metadata_table())) {
#       shinyjs::hide("group_controls")
#     }
#     else {
#       shinyjs::show("group_controls")
#     }
#   })
#
#   observeEvent(input$startbuttonMetrics, {
#     tryCatch({
#       withProgress(message = 'Calculating Instability Metrics ...', style = "old",
#                    value = 0, {
#                      incProgress(0.1)
#
#                      if (is.null(upload_data$metadata_table())) {
#                        assign_index_peaks(
#                          peaks_module$index_list(),
#                          grouped = FALSE
#                        )
#                      }
#                      else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#                        if (input$group_controls == TRUE) {
#                          assign_index_peaks(
#                            peaks_module$index_list(),
#                            grouped = TRUE,
#                            index_override_dataframe = reactive_metrics2$Index_Table[,c(1,4)]
#                          )
#                        }
#                        else {
#                          assign_index_peaks(
#                            peaks_module$index_list(),
#                            grouped = FALSE
#                          )
#                        }
#                      }
#                      else {
#                        assign_index_peaks(
#                          peaks_module$index_list(),
#                          grouped = FALSE
#                        )
#                      }
#
#                      reactive_metrics2$df <- calculate_instability_metrics(
#                        fragments_list = peaks_module$index_list(),
#                        peak_threshold = input$peak_threshold,
#                        window_around_index_peak = c(input$window_around_index_peak_min, input$window_around_index_peak_max),
#                        percentile_range = seq(input$percentile_range1, input$percentile_range2, input$percentile_range3),
#                        repeat_range = seq(input$repeat_range1 , input$repeat_range2, input$repeat_range3)
#                      )
#
#                      shinyjs::show("NextButtonMetrics")
#
#                      reactive_metrics2$Index_Table_original <- reactive_metrics2$Index_Table
#
#                      if(input$AnalysisBox1$collapsed == TRUE) {
#                        js$collapse("AnalysisBox1")
#                      }
#
#                      output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
#                                                                       menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
#                                                                       menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
#                                                                       menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
#                                                                       menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = T),
#                                                                       menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis", selected = F,
#                                                                                badgeColor = "green", badgeLabel = "new")))
#                    })
#     },
#     error = function(e) {
#       shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
#       reactive_metrics2$df <- NULL
#     })
#   })
#
#   observeEvent(input$IndexRepeat1, {
#     if (!is.null(reactive_metrics2$Index_Table) && !is.null(peaks_module$index_list())) {
#       reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat` <- input$IndexRepeat1
#     }
#   })
#
#   observeEvent(list(input$IndexRepeat2, input$sample_subset_metrics),  {
#     if (!is.null(ladder_module$ladders()) && !is.null(input$sample_subset2) && !is.na(input$IndexRepeat1)) {
#       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#         if (input$group_controls == TRUE) {
#           if (!is.null(reactive_metrics2$Index_Table) && !is.null(peaks_module$index_list())) {
#             reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset_metrics),]$`Index Repeat` <- input$IndexRepeat2
#             reactive_metrics2$Index_Table[which(reactive_metrics2$Index_Table$`Unique IDs` == input$sample_subset2),]$`Index Repeat` <- input$IndexRepeat2
#           }
#         }
#       }
#     }
#   })
#
#   output$Index_Table <- DT::renderDataTable({
#
#     datatable(reactive_metrics2$Index_Table,
#               options = list(scrollX = TRUE,
#                              scrollY = TRUE,
#                              server = TRUE,
#                              paging = TRUE,
#                              pageLength = 15
#               ),
#               selection = 'single',
#               rownames = FALSE)
#
#   },  options = list(scrollX = TRUE))
#
#   observe({
#     if (!is.null(reactive_metrics2$df)) {
#       shinyjs::show("downloadmetrics2")
#     }
#     else {
#       shinyjs::hide("downloadmetrics2")
#     }
#     if (identical(reactive_metrics2$Index_Table_original, reactive_metrics2$Index_Table)) {
#       shinyjs::show("downloadmetrics2")
#     }
#     else {
#       shinyjs::hide("downloadmetrics2")
#     }
#   })
#
#   output$metrics_table <- DT::renderDataTable({
#     validate(
#       need(!is.null(reactive_metrics2$df), 'You must perform the analysis first...'))
#     validate(
#       need(identical(reactive_metrics2$Index_Table_original, reactive_metrics2$Index_Table), 'Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.')
#     )
#     reactive_metrics2$df
#   },  options = list(scrollX = TRUE))
#
#   output$plot_traces_final_UI <- renderUI({
#     plotlyOutput("plot_traces_final", height = (300 + input$HeightPeaks_metrics*20))
#   })
#
#   output$plot_traces_final <- renderPlotly({
#
#     if (is.null(peaks_module$index_list())) {
#       # Return a blank plot if object is missing
#       return(plotly::plot_ly())
#     }
#
#     xlim = c(input$xlim1_metrics, input$xlim2_metrics)
#     ylim = c(input$ylim1_metrics, input$ylim2_metrics)
#     height_color_threshold = input$minimum_peak_signal
#     plot_title = NULL
#
#     #there must be a simpler way of the following if else below
#     data <- peaks_module$index_list()[[input$sample_subset_metrics]]$trace_bp_df
#     data$x <- data$calculated_repeats
#
#     if (!is.null(xlim)) {
#       data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
#     }
#
#     if (show_peaks_metrics == TRUE) {
#       # add points onto plot showing peaks
#       peak_table <- peaks_module$index_list()[[input$sample_subset_metrics]]$repeat_table_df
#       peak_table$x <- peak_table$repeats
#
#       if (!is.null(xlim)) {
#         peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
#       }
#
#       tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
#       tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
#       if (!is.null(peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height) && !is.na(peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height)) {
#         tallest_peak_height <- peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_height
#         tallest_peak_x <- peaks_module$index_list()[[input$sample_subset_metrics]]$get_allele_peak()$allele_repeat
#       }
#
#       peaks_above <- peak_table[which(peak_table$height > height_color_threshold), ]
#       peaks_below <- peak_table[which(peak_table$height < height_color_threshold), ]
#
#     }
#
#     if (is.null(upload_data$metadata_table())) {
#       if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
#         if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
#           plot_ly(data = data,
#                   x = ~x, y = ~signal,
#                   type = "scatter",
#                   mode = "lines",
#                   height = (300 + input$HeightPeaks_metrics*20),
#                   name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#             add_trace(x = peaks_above$x,
#                       y = peaks_above$height,
#                       mode = "markers",
#                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Above Threshold")) %>%
#             add_trace(x = peaks_below$x,
#                       y = peaks_below$height,
#                       mode = "markers",
#                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
#             add_trace(x = tallest_peak_x,
#                       y = tallest_peak_height,
#                       mode = "markers",
#                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
#             add_segments(x = peak_table$repeats,
#                          y = peak_table$height,
#                          xend = peak_table$calculated_repeats,
#                          yend = peak_table$height,
#                          line = list(dash = "dash"),
#                          name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
#             layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                    xaxis = list(title = "Repeats",
#                                 range = xlim),
#                    yaxis = list(title = "Signal",
#                                 range = ylim),
#                    shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                                 #vertical line
#                                                                 list(type = "line", x0 = input$IndexRepeat1,
#                                                                      x1 = input$IndexRepeat1,
#                                                                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                                 list(type = "rect",
#                                                                      fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                      y0 = 0, y1 = input$ylim2_metrics,
#                                                                      x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
#                                                                      x1 = input$window_around_index_peak_max + input$IndexRepeat1))
#             )
#         }
#       }
#       else {
#         plot_ly(data = data,
#                 x = ~x, y = ~signal,
#                 type = "scatter",
#                 mode = "lines",
#                 height = (300 + input$HeightPeaks_metrics*20),
#                 name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#           layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                  xaxis = list(title = "Repeats",
#                               range = xlim),
#                  yaxis = list(title = "Signal",
#                               range = ylim),
#                  shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                               #vertical line
#                                                               list(type = "line", x0 = input$IndexRepeat1,
#                                                                    x1 = input$IndexRepeat1,
#                                                                    y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                               list(type = "rect",
#                                                                    fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                    y0 = 0, y1 = input$ylim2_metrics,
#                                                                    x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
#                                                                    x1 = input$window_around_index_peak_max + input$IndexRepeat1))
#           )
#       }
#     }
#
#     else if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control)) && !is.null(input$sample_subset2)) {
#
#       if (input$group_controls == TRUE) {
#         if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
#           if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
#             plot_ly(data = data,
#                     x = ~x, y = ~signal,
#                     type = "scatter",
#                     mode = "lines",
#                     height = (300 + input$HeightPeaks_metrics*20),
#                     name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#               add_trace(x = peaks_above$x,
#                         y = peaks_above$height,
#                         mode = "markers",
#                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peak Above Threshold")) %>%
#               add_trace(x = peaks_below$x,
#                         y = peaks_below$height,
#                         mode = "markers",
#                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
#               add_trace(x = tallest_peak_x,
#                         y = tallest_peak_height,
#                         mode = "markers",
#                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
#               add_segments(x = peak_table$repeats,
#                            y = peak_table$height,
#                            xend = peak_table$calculated_repeats,
#                            yend = peak_table$height,
#                            line = list(dash = "dash"),
#                            name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
#               layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                      xaxis = list(title = "Repeats",
#                                   range = xlim),
#                      yaxis = list(title = "Signal",
#                                   range = ylim),
#                      shapes = if(!is.na(input$IndexRepeat2)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                                   #vertical line
#                                                                   list(type = "line", x0 = input$IndexRepeat2,
#                                                                        x1 = input$IndexRepeat2,
#                                                                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                                   list(type = "rect",
#                                                                        fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                        y0 = 0, y1 = input$ylim2_metrics,
#                                                                        x0 = input$IndexRepeat2 + input$window_around_index_peak_min,
#                                                                        x1 = input$window_around_index_peak_max + input$IndexRepeat2))
#               )
#           }
#         }
#         else {
#           plot_ly(data = data,
#                   x = ~x, y = ~signal,
#                   type = "scatter",
#                   mode = "lines",
#                   height = (300 + input$HeightPeaks_metrics*20),
#                   name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#             layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                    xaxis = list("Repeats",
#                                 range = xlim),
#                    yaxis = list("Signal",
#                                 range = ylim),
#                    shapes = if(!is.na(input$IndexRepeat2)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                                 #vertical line
#                                                                 list(type = "line", x0 = input$IndexRepeat2,
#                                                                      x1 = input$IndexRepeat2,
#                                                                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                                 list(type = "rect",
#                                                                      fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                      y0 = 0, y1 = input$ylim2_metrics,
#                                                                      x0 = input$IndexRepeat2 + input$window_around_index_peak_min,
#                                                                      x1 = input$window_around_index_peak_max + input$IndexRepeat2))
#             )
#         }
#       }
#       else {
#         if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
#           if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
#             plot_ly(data = data,
#                     x = ~x, y = ~signal,
#                     type = "scatter",
#                     mode = "lines",
#                     height = (300 + input$HeightPeaks_metrics*20),
#                     name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#               add_trace(x = peaks_above$x,
#                         y = peaks_above$height,
#                         mode = "markers",
#                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Above Threshold")) %>%
#               add_trace(x = peaks_below$x,
#                         y = peaks_below$height,
#                         mode = "markers",
#                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
#               add_trace(x = tallest_peak_x,
#                         y = tallest_peak_height,
#                         mode = "markers",
#                         name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
#               add_segments(x = peak_table$repeats,
#                            y = peak_table$height,
#                            xend = peak_table$calculated_repeats,
#                            yend = peak_table$height,
#                            line = list(dash = "dash"),
#                            name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
#               layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                      xaxis = list(title = "Repeats",
#                                   range = xlim),
#                      yaxis = list(title = "Signal",
#                                   range = ylim),
#                      shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                                   #vertical line
#                                                                   list(type = "line", x0 = input$IndexRepeat1,
#                                                                        x1 = input$IndexRepeat1,
#                                                                        y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                                   list(type = "rect",
#                                                                        fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                        y0 = 0, y1 = input$ylim2_metrics,
#                                                                        x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
#                                                                        x1 = input$window_around_index_peak_max + input$IndexRepeat1))
#               )
#           }
#         }
#         else {
#           plot_ly(data = data,
#                   x = ~x, y = ~signal,
#                   type = "scatter",
#                   mode = "lines",
#                   height = (300 + input$HeightPeaks_metrics*20),
#                   name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#             layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                    xaxis = list(title = "Repeats",
#                                 range = xlim),
#                    yaxis = list(title = "Signal",
#                                 range = ylim),
#                    shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                                 #vertical line
#                                                                 list(type = "line", x0 = input$IndexRepeat1,
#                                                                      x1 = input$IndexRepeat1,
#                                                                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                                 list(type = "rect",
#                                                                      fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                      y0 = 0, y1 = input$ylim2_metrics,
#                                                                      x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
#                                                                      x1 = input$window_around_index_peak_max + input$IndexRepeat1))
#             )
#         }
#       }
#     }
#     else {
#       if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
#         if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
#           plot_ly(data = data,
#                   x = ~x, y = ~signal,
#                   type = "scatter",
#                   mode = "lines",
#                   height = (300 + input$HeightPeaks_metrics*20),
#                   name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#             add_trace(x = peaks_above$x,
#                       y = peaks_above$height,
#                       mode = "markers",
#                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Above Threshold")) %>%
#             add_trace(x = peaks_below$x,
#                       y = peaks_below$height,
#                       mode = "markers",
#                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Peaks Below Threshold")) %>%
#             add_trace(x = tallest_peak_x,
#                       y = tallest_peak_height,
#                       mode = "markers",
#                       name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Modal Peak")) %>%
#             add_segments(x = peak_table$repeats,
#                          y = peak_table$height,
#                          xend = peak_table$calculated_repeats,
#                          yend = peak_table$height,
#                          line = list(dash = "dash"),
#                          name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), " Force Whole Repeats")) %>%
#             layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                    xaxis = list(title = "Repeats",
#                                 range = xlim),
#                    yaxis = list(title = "Signal",
#                                 range = ylim),
#                    shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                                 #vertical line
#                                                                 list(type = "line", x0 = input$IndexRepeat1,
#                                                                      x1 = input$IndexRepeat1,
#                                                                      y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                                 list(type = "rect",
#                                                                      fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                      y0 = 0, y1 = input$ylim2_metrics,
#                                                                      x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
#                                                                      x1 = input$window_around_index_peak_max + input$IndexRepeat1))
#             )
#         }
#       }
#       else {
#         plot_ly(data = data,
#                 x = ~x, y = ~signal,
#                 type = "scatter",
#                 mode = "lines",
#                 height = (300 + input$HeightPeaks_metrics*20),
#                 name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id)), "")) %>%
#           layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset_metrics]]$unique_id, plot_title),
#                  xaxis = list(title = "Repeats",
#                               range = xlim),
#                  yaxis = list(title = "Signal",
#                               range = ylim),
#                  shapes = if(!is.na(input$IndexRepeat1)) list(hline(input$peak_threshold*peaks_module$index_list()[[input$sample_subset_metrics]]$.__enclos_env__$private$index_height),
#                                                               #vertical line
#                                                               list(type = "line", x0 = input$IndexRepeat1,
#                                                                    x1 = input$IndexRepeat1,
#                                                                    y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")),
#                                                               list(type = "rect",
#                                                                    fillcolor = "red", line = list(color = "red"), opacity = 0.1,
#                                                                    y0 = 0, y1 = input$ylim2_metrics,
#                                                                    x0 = input$IndexRepeat1 + input$window_around_index_peak_min,
#                                                                    x1 = input$window_around_index_peak_max + input$IndexRepeat1))
#           )
#       }
#     }
#   })
#
#   output$plot_traces_INDEX_UI <- renderUI({
#     plotlyOutput("plot_traces_INDEX", height = (300 + input$HeightPeaks_metrics*20))
#   })
#
#   output$plot_traces_INDEX <- renderPlotly({
#     validate(
#       need(!is.null(input$sample_subset2), 'You do not have any baseline control samples in your metadata, please check your metadata file.'))
#
#     if (is.null(peaks_module$index_list())) {
#       # Return a blank plot if object is missing
#       return(plotly::plot_ly())
#     }
#
#     if (input$show_peaks_metrics == "YES") {
#       show_peaks_metrics = TRUE
#     }
#     else {
#       show_peaks_metrics = FALSE
#     }
#
#     xlim = c(input$xlim1_metrics, input$xlim2_metrics)
#     ylim = c(input$ylim1_metrics, input$ylim2_metrics)
#     height_color_threshold = input$minimum_peak_signal
#     plot_title = NULL
#
#     #there must be a simpler way of the following if else below
#     data <- peaks_module$index_list()[[input$sample_subset2]]$trace_bp_df
#     data$x <- data$calculated_repeats
#
#     if (!is.null(xlim)) {
#       data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
#     }
#
#     if (show_peaks_metrics == TRUE) {
#       # add points onto plot showing peaks
#       peak_table <- peaks_module$index_list()[[input$sample_subset2]]$repeat_table_df
#       peak_table$x <- peak_table$repeats
#
#       if (!is.null(xlim)) {
#         peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
#       }
#
#       tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
#       tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
#       if (!is.null(peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_height) && !is.na(peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_height)) {
#         tallest_peak_height <- peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_height
#         tallest_peak_x <- peaks_module$index_list()[[input$sample_subset2]]$get_allele_peak()$allele_repeat
#       }
#
#       peaks_above <- peak_table[which(peak_table$height > height_color_threshold), ]
#       peaks_below <- peak_table[which(peak_table$height < height_color_threshold), ]
#
#     }
#
#     if (show_peaks_metrics == TRUE && nrow(peak_table) > 0) {
#       if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
#         plot_ly(data = data,
#                 x = ~x, y = ~signal,
#                 type = "scatter",
#                 mode = "lines",
#                 source = "plot_peak2",
#                 height = (300 + input$HeightPeaks_metrics*20),
#                 name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), "")) %>%
#           add_trace(x = peaks_above$x,
#                     y = peaks_above$height,
#                     mode = "markers",
#                     name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Peaks Above Threshold")) %>%
#           add_trace(x = peaks_below$x,
#                     y = peaks_below$height,
#                     mode = "markers",
#                     name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Peaks Below Threshold")) %>%
#           add_trace(x = tallest_peak_x,
#                     y = tallest_peak_height,
#                     mode = "markers",
#                     name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Modal Peak")) %>%
#           add_segments(x = peak_table$repeats,
#                        y = peak_table$height,
#                        xend = peak_table$calculated_repeats,
#                        yend = peak_table$height,
#                        line = list(dash = "dash"),
#                        name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), " Force Whole Repeats")) %>%
#           layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset2]]$unique_id, plot_title),
#                  xaxis = list(title = "Repeats",
#                               range = xlim),
#                  yaxis = list(title = "Signal",
#                               range = ylim),
#                  shapes = list(
#                    #vertical line
#                    list(type = "line", x0 = input$IndexRepeat2,
#                         x1 = input$IndexRepeat2,
#                         y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
#           )
#       }
#     }
#     else {
#       plot_ly(data = data,
#               x = ~x, y = ~signal,
#               type = "scatter",
#               mode = "lines",
#               source = "plot_peak2",
#               height = (300 + input$HeightPeaks_metrics*20),
#               name = paste0(gsub(".fsa", "", unique(peaks_module$index_list()[[input$sample_subset2]]$unique_id)), "")) %>%
#         layout(title = ifelse(is.null(plot_title), peaks_module$index_list()[[input$sample_subset2]]$unique_id, plot_title),
#                xaxis = list(title = "Repeats",
#                             range = xlim),
#                yaxis = list(title = "Signal",
#                             range = ylim),
#                shapes = list(
#                  #vertical line
#                  list(type = "line", x0 = input$IndexRepeat2,
#                       x1 = input$IndexRepeat2,
#                       y0 = 0, y1 = 1, yref = "paper", line = list(color = "red")))
#         )
#     }
#   })
#
#   observeEvent(input$up_inv, {
#     tryCatch({
#       if (is.null(upload_data$metadata_table())) {
#         updatePickerInput(session, "sample_subset_metrics", selected = if (which(names(upload_data$fsa_list()) == input$sample_subset_metrics) - 1 == 0)
#           upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics)]]$unique_id
#           else upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics) - 1]]$unique_id)
#       }
#       else if (!is.null(upload_data$metadata_table())) {
#         if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#           if (input$group_controls == TRUE) {
#             if (!any(which(upload_data$metadata_table()$metrics_baseline_control == TRUE) == input$Index_Table_rows_selected)) {
#               updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) - 1 == 0)
#                 upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics)]
#                 else upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) - 1])
#             }
#           }
#           else {
#             updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1 == 0)
#               upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
#               else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1])
#           }
#         }
#         else {
#           updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1 == 0)
#             upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
#             else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) - 1])
#         }
#       }
#     },
#     error = function(e) {
#       shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
#     })
#   })
#
#   observeEvent(input$down_inv, {
#     tryCatch({
#       if (is.null(upload_data$metadata_table())) {
#         updatePickerInput(session, "sample_subset_metrics", selected = if (which(names(upload_data$fsa_list()) == input$sample_subset_metrics) + 1 > length(names(upload_data$fsa_list())))
#           upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics)]]$unique_id
#           else upload_data$fsa_list()[[which(names(upload_data$fsa_list()) == input$sample_subset_metrics) + 1]]$unique_id)
#       }
#       else if (!is.null(upload_data$metadata_table())) {
#         if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#           if (input$group_controls == TRUE) {
#             updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) + 1 > length(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id))
#               upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics)]
#               else upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id[which(upload_data$metadata_table()[-which(upload_data$metadata_table()$metrics_baseline_control == TRUE),]$unique_id == input$sample_subset_metrics) + 1])
#           }
#           else {
#             updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1 > length(upload_data$metadata_table()$unique_id))
#               upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
#               else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1])
#           }
#         }
#         else {
#           updatePickerInput(session, "sample_subset_metrics", selected = if (which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1 > length(upload_data$metadata_table()$unique_id))
#             upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics)]
#             else upload_data$metadata_table()$unique_id[which(upload_data$metadata_table()$unique_id == input$sample_subset_metrics) + 1])
#         }
#       }
#     },
#     error = function(e) {
#       shinyalert("ERROR!", "You've reached the end of the selection, please go the other way!", type = "error", confirmButtonCol = "#337ab7")
#     })
#   })
#
#   observeEvent(input$Index_Table_rows_selected, {
#     if (is.null(upload_data$metadata_table())) {
#       updatePickerInput(session, "sample_subset_metrics", selected = upload_data$fsa_list()[[input$Index_Table_rows_selected]]$unique_id)
#     }
#     else if (!is.null(upload_data$metadata_table())) {
#       if (any(grepl("TRUE", upload_data$metadata_table()$metrics_baseline_control))) {
#         if (input$group_controls == TRUE) {
#           if (!any(which(upload_data$metadata_table()$metrics_baseline_control == TRUE) == input$Index_Table_rows_selected)) {
#             updatePickerInput(session, "sample_subset_metrics", selected = upload_data$metadata_table()$unique_id[input$Index_Table_rows_selected])
#           }
#         }
#         else {
#           updatePickerInput(session, "sample_subset_metrics", selected = upload_data$metadata_table()$unique_id[input$Index_Table_rows_selected])
#         }
#       }
#       else {
#         updatePickerInput(session, "sample_subset_metrics", selected = upload_data$metadata_table()$unique_id[input$Index_Table_rows_selected])
#       }
#     }
#   })
#
#   observe({
#     if (!is.null(reactive_metrics2$Index_Table_original) && !is.null(reactive_metrics2$Index_Table)) {
#       if (identical(reactive_metrics2$Index_Table_original, reactive_metrics2$Index_Table)) {
#         shinyjs::hide("text_no_data2")
#       }
#       else {
#         shinyjs::show("text_no_data2")
#       }
#     }
#   })
#
#   output$text_no_data2 <- renderUI({
#     if (is.null(reactive_metrics2$df)) {
#       h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Instability Metrics not computed! Press Apply on the left to compute metrics</b>'))
#     }
#     else {
#       h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Changes detected in index repeat for sample(s). Press apply on the left to incorporate these changes.</b>'))
#     }
#   })
#
#   ###SAVE FUNCTION
#   output$downloadDataSave2 <- downloadHandler(
#     filename = function() {
#       paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_SAVED_OBJECT_", sessionInfo()$otherPkgs$traceShiny$Version, ".Rdata")
#     },
#     content = function(file) {
#
#       #Upload
#       fastq = upload_data$fastq()
#       metadata_table = upload_data$metadata_table()
#       DataUpload = upload_data$DataUpload()
#       DataUploadMeta = upload_data$DataUploadMeta()
#
#       #Investigate
#       instability_metrics_fastq <- reactive_metrics2$df
#       Index_Table <- reactive_metrics2$Index_Table
#       Index_Table_original <- reactive_metrics2$Index_Table_original
#       sample_subset_metrics <- reactive_metrics2$sample_subset_metrics
#       sample_subset2 <- reactive_metrics2$sample_subset2
#       peak_threshold <- input$peak_threshold2
#       repeat_range1 <- input$repeat_range1_2
#       repeat_range2 <- input$repeat_range2_2
#       repeat_range3 <- input$repeat_range3_2
#       percentile_range1 <- input$percentile_range1_2
#       percentile_range2 <- input$percentile_range2_2
#       percentile_range3 <- input$percentile_range3_2
#       window_around_index_peak_min <- input$window_around_index_peak_min2
#       window_around_index_peak_max <- input$window_around_index_peak_max2
#       group_controls <- input$group_controls2
#
#       #Package Version
#       Package_version <- sessionInfo()$otherPkgs$traceShiny$Version
#
#
#       save("fastq", "metadata_table", "DataUpload", "DataUploadMeta",
#            "instability_metrics_fastq", "peak_threshold", "window_around_index_peak_min", "window_around_index_peak_max", "repeat_range1", "repeat_range2", "repeat_range3", "percentile_range1", "percentile_range2", "percentile_range3",
#            "sample_subset2", "sample_subset_metrics", "Package_version", "Index_Table", "Index_Table_original", "group_controls",
#            file = file)
#     }
#   )
#
#   return(list(
#     metrics_table = reactive(reactive_metrics2$df)
#   ))
# }
