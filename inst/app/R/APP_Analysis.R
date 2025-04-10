Analysis_box_ui1 <- function(id) {
  box(id = "AnalysisBox1", title = p("Overlay Plots"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(1,
               numericInput("xlim1_analysis", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                            value = 0)),

        column(1,
               numericInput("xlim2_analysis", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                            value = 250)),
        column(1,
               numericInput("ylim1_analysis", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                            value = -200)),

        column(1,
               numericInput("ylim2_analysis", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                            value = 2000)),

        column(4,
               sliderInput("opacity", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Opacity')),
                           min = 0, max = 1,
                           value = 0.6, step = 0.1)
        ),

        column(4,
               sliderInput("HeightAnalysis", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )
      ),

      withSpinner(htmlOutput("overlay_plotUI"))
  )
}

Analysis_box_ui2 <- function(id) {
  box(id = "AnalysisBox2", title = p("Instability Metrics Table", help_button("Metrics_analysis")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(6,
               p(style="text-align: left;", actionButton("clear", "Clear Rows"))
        ),
        column(6,
               p(style="text-align: right;", downloadButton("metrics_table_analysis_download"))
        )
      ),
        withSpinner(DT::dataTableOutput("metrics_table_analysis", width = "100%", height = "400"))
      )
}

# Analysis_box_ui2_2 <- function(id) {
#   box(id = "AnalysisBox2_2", title = p("Span Modelling"), status = "warning", solidHeader = F,
#       collapsible = T, width = NULL,
#
#       withSpinner(DT::dataTableOutput("span_model", width = "100%", height = "400"))
#   )
# }

Analysis_box_ui3 <- function(id) {
  box(id = "AnalysisBox3", title = p("Settings", help_button("settings")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(12,
               radioGroupButtons(
                 inputId = "peaks_traces",
                 label = h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Show Peaks or Traces')),
                 choices = c("Peaks",
                             "Traces"),
                 justified = TRUE,
                 selected = "Peaks"
               ),

               virtualSelectInput("Analysis_samples", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot to Overlay')),
                                  choices = NULL,
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your samples"
               ),

               virtualSelectInput("group_samples", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Group Samples By')),
                                  choices = "unique_id",
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your groupings"
               ),

               pickerInput("normalize", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Normalization Method')),
                           choices = c("None", "Highest Peak", "Sum Of All Peaks")),

               htmlOutput("Normalization_settings"),
               fluidRow(
                 column(6,
                        numericInput("threshold_min", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Min Repeat Size')),
                                     value = 10)),
                 column(6,
                        numericInput("threshold_max", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Max Repeat Size')),
                                     value = 500))
               ),
               materialSwitch("index_normalize", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Set Index Repeat as Zero')), value = FALSE, status = "primary"),
               materialSwitch("dot_show", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show peaks/traces')), value = TRUE, status = "primary"),
               materialSwitch("line_show", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show Best Fit Line')), value = FALSE, status = "primary"),

               conditionalPanel(
                 condition = 'input.line_show == true',
                 numericInput("span", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Span')),
                              value = 0.1, max = 1,
                              step = 0.01),
                 materialSwitch("CI_show", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show Confidence Intervals')), value = FALSE, status = "primary"),

                 conditionalPanel(
                   condition = 'input.CI_show == true',
                   numericInput("CI", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Confidence Interval')),
                                value = 0.95, max = 1,
                                step = 0.01)
                 )
               )
        ))
  )
}

analysis_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module, metrics_module) {

  help_click("settings", helpfile = "data/analysis/Settings.html")
  help_click("Metrics_analysis", helpfile = "data/metrics/metrics_table.html")

  reactive_analysis <- reactiveValues()

  #Colours
  set.seed(001)
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  color = color[-grep("white", color)]
  color = c(palette(), sample(color))
  color = color[-grep("black", color)]

  observe({
    updateVirtualSelect("Analysis_samples", choices = names(upload_data$fsa_list()))
    updateVirtualSelect("group_samples", choices = colnames(upload_data$metadata_table()))
  })

  #Download
  output$metrics_table_analysis_download <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Instability_Metrics_Table_with_Metadata.csv")
    },
    content = function(file) {
      if (!is.null(upload_data$metadata_table())) {
        df <- dplyr::left_join(upload_data$metadata_table(), metrics_module$metrics_table())
      }
      else {
        df <- arrange(metrics_module$metrics_table(), unique_id)
      }

      write.csv(df, file, row.names = F, col.names = T)
    }
  )

  observeEvent(input$clear, {
    proxy %>% selectRows(NULL)
    updateVirtualSelect("Analysis_samples", choices = names(upload_data$fsa_list()))
  })

  observeEvent(input$NextButtonMetrics, {

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics", selected = F),
                                                     menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis", selected = T,
                                                              badgeColor = "green", badgeLabel = "new"),
                                                     menuItem("Documentation", icon = icon("file"), startExpanded = T,
                                                              menuSubItem("Step 1: Upload", tabName = "Documentation1"),
                                                              menuSubItem("Step 2: Find Ladders", tabName = "Documentation2"),
                                                              menuSubItem("Step 3: Find Peaks", tabName = "Documentation3"),
                                                              menuSubItem("Step 4: Instability Metrics", tabName = "Documentation4"),
                                                              menuSubItem("Step 5: Analysis", tabName = "Documentation5"))
                                                     ))
  })

  output$Normalization_settings <- renderUI({
    h5(HTML('<b><h5 style = "text-align:justify;">Normalization Settings</b>'))
  })

  observe({
    if (input$normalize == "None") {
      shinyjs::hide("threshold_min")
      shinyjs::hide("threshold_max")
      shinyjs::hide("Normalization_settings")
    }
    else {
      shinyjs::show("threshold_max")
      shinyjs::show("threshold_min")
      shinyjs::show("Normalization_settings")
    }
  })

  observeEvent(input$normalize, {
    if (!is.null(peaks_module$index_list())) {
      if (input$normalize == "Highest Peak") {
        updateNumericInput(session, "ylim1_analysis", value = -0.3)
      }
      else if (input$normalize == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim1_analysis", value = -0.03)
      }
      else if (input$normalize == "None") {
        updateNumericInput(session, "ylim1_analysis", value = -200)
      }

      if (input$normalize == "Highest Peak") {
        updateNumericInput(session, "ylim2_analysis", value = 1.2)
      }
      else if (input$normalize == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim2_analysis", value = 0.3)
      }
      else if (input$normalize == "None") {
        updateNumericInput(session, "ylim2_analysis", value = max(metrics_module$metrics_table()$modal_peak_signal) + 300)
      }
    }
  })

  observe({
    if (!is.null(peaks_module$index_list()) && !is.null(input$sample_subset_metrics)) {
      updateNumericInput(session, "xlim1_analysis", value = min(metrics_module$metrics_table()$modal_peak_repeat) - 50)
      updateNumericInput(session, "xlim2_analysis", value = max(metrics_module$metrics_table()$modal_peak_repeat) + 50)
      updateNumericInput(session, "ylim1_analysis", value = -200)
      updateNumericInput(session, "ylim2_analysis", value = max(metrics_module$metrics_table()$modal_peak_signal) + 300)

    if (input$index_normalize == TRUE) {
      updateNumericInput(session, "xlim1_analysis", value = -50)
      updateNumericInput(session, "xlim2_analysis", value = 50)

      if (input$normalize == "Highest Peak") {
        updateNumericInput(session, "ylim1_analysis", value = -0.3)
      }
      else if (input$normalize == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim1_analysis", value = -0.03)
      }
      else if (input$normalize == "None") {
        updateNumericInput(session, "ylim1_analysis", value = -200)
      }

      if (input$normalize == "Highest Peak") {
        updateNumericInput(session, "ylim2_analysis", value = 1.2)
      }
      else if (input$normalize == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim2_analysis", value = 0.3)
      }
      else if (input$normalize == "None") {
        updateNumericInput(session, "ylim2_analysis", value = max(metrics_module$metrics_table()$modal_peak_signal) + 300)
      }
    }
    else {
      updateNumericInput(session, "xlim1_analysis", value = min(metrics_module$metrics_table()$modal_peak_repeat) - 50)
      updateNumericInput(session, "xlim2_analysis", value = max(metrics_module$metrics_table()$modal_peak_repeat) + 50)

      if (input$normalize == "Highest Peak") {
        updateNumericInput(session, "ylim1_analysis", value = -0.3)
      }
      else if (input$normalize == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim1_analysis", value = -0.03)
      }
      else if (input$normalize == "None") {
        updateNumericInput(session, "ylim1_analysis", value = -200)
      }

      if (input$normalize == "Highest Peak") {
        updateNumericInput(session, "ylim2_analysis", value = 1.2)
      }
      else if (input$normalize == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim2_analysis", value = 0.3)
      }
      else if (input$normalize == "None") {
        updateNumericInput(session, "ylim2_analysis", value = max(metrics_module$metrics_table()$modal_peak_signal) + 300)
      }
    }
    }
  })

  output$overlay_plotUI <- renderUI({
    plotlyOutput("overlay_plot", height = (300 + input$HeightAnalysis*20))
  })

  output$overlay_plot <- renderPlotly({

    if (is.null(input$Analysis_samples)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$peaks_traces == "Traces") {
      trace <- extract_trace_table(peaks_module$index_list())

      trace <- mutate(trace, Normalised_Signal = signal)

      trace <- trace[which(trace$unique_id %in% input$Analysis_samples),]

      if (input$normalize == "None") {

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")
      }

      else if (input$normalize == "Sum Of All Peaks") {
        lookup <- trace[trace$calculated_repeats > input$threshold_min, ]
        lookup <- lookup[lookup$calculated_repeats < input$threshold_max, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(sum(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")
      }

      else if (input$normalize == "Highest Peak") {
        lookup <- trace[trace$calculated_repeats > input$threshold_min, ]
        lookup <- lookup[lookup$calculated_repeats < input$threshold_max, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(max(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")
      }

      if (input$index_normalize == TRUE) {
        trace$calculated_repeats <- trace$calculated_repeats - trace$index_repeat
      }

      reactive_analysis$trace <- trace

      if (input$line_show == TRUE) {
        if (input$dot_show == TRUE) {
          p <- ggplot(trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", calculated_repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_smooth(aes(x=calculated_repeats, y=Normalised_Signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
            geom_line(aes(group = unique_id), alpha = input$opacity) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
        else {
          p <- ggplot(trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", calculated_repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_smooth(aes(x=calculated_repeats, y=Normalised_Signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
      }
      else {
        if (input$dot_show == TRUE) {
          p <- ggplot(trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", calculated_repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_line(aes(group = unique_id), alpha = input$opacity) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
        else {
          p <- ggplot(trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", calculated_repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
      }
      ggplotly(p, tooltip="text", height = (300 + input$HeightAnalysis*20))
    }
    else {
      trace <- extract_fragments(peaks_module$index_list())

      trace <- trace[which(trace$unique_id %in% input$Analysis_samples),]

      if (input$normalize == "None") {

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace <- mutate(trace, Normalised_Signal = signal)

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")
      }

      else if (input$normalize == "Sum Of All Peaks") {
        lookup <- trace[trace$repeats > input$threshold_min, ]
        lookup <- lookup[lookup$repeats < input$threshold_max, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(sum(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")
      }

      else if (input$normalize == "Highest Peak") {
        lookup <- trace[trace$repeats > input$threshold_min, ]
        lookup <- lookup[lookup$repeats < input$threshold_max, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(max(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")
      }

      if (input$index_normalize == TRUE) {
        trace$repeats <- trace$repeats - trace$index_repeat
      }

      reactive_analysis$trace <- trace


      if (input$line_show == TRUE) {
        if (input$dot_show == TRUE) {

          p <- ggplot(trace, aes(x=repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_point(alpha = input$opacity) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            geom_smooth(aes(x=repeats, y=Normalised_Signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
        else {

          p <- ggplot(trace, aes(x=repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            geom_smooth(aes(x=repeats, y=Normalised_Signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
      }
      else {

        if (input$dot_show == TRUE) {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_point(alpha = input$opacity)+
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
        else {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_Signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize == "None") "Signal: " else "Normalised_Signal: ", Normalised_Signal, "\n",
                                                               "Grouping: ", plot))) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_classic()
        }
      }
      ggplotly(p, tooltip="text", height = (300 + input$HeightAnalysis*20))
    }
  })

  proxy = dataTableProxy('metrics_table_analysis')

  output$metrics_table_analysis <- DT::renderDataTable({

    if (!is.null(upload_data$metadata_table())) {
      df <- dplyr::left_join(upload_data$metadata_table(), metrics_module$metrics_table())
    }
    else {
      df <- arrange(metrics_module$metrics_table(), unique_id)
    }

    datatable(df,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              filter = "top",
              rownames = FALSE)
  },  options = list(scrollX = TRUE))

  output$span_model <- DT::renderDataTable({

    #define k-fold cross validation method
    ctrl <- trainControl(method = "cv", number = 10)
    grid <- expand.grid(span = input$span, degree = 1)

    #perform cross-validation
    if (input$peaks_traces == "Traces") {
      model <- train(signal_corrected ~ calculated_repeats, data = reactive_analysis$trace, method = "gamLoess", tuneGrid=grid, trControl = ctrl)
    }
    else {
      model <- train(signal_corrected ~ repeats, data = reactive_analysis$trace, method = "gamLoess", tuneGrid=grid, trControl = ctrl)
    }

    datatable(model$results,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              rownames = FALSE)
  },  options = list(scrollX = TRUE))

  observeEvent(input$metrics_table_analysis_rows_selected, {

    list <- list()
    for (i in input$metrics_table_analysis_rows_selected) {
      list[i] <- paste(upload_data$fsa_list()[[i]]$unique_id)
    }

    list <- unlist(list)

    updateVirtualSelect("Analysis_samples", selected = list)
  })

}
