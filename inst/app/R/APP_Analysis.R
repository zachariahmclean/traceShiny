Analysis_box_ui1 <- function(id) {
  box(id = "AnalysisBox1", title = p("Overlay Plots"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(1,
               numericInput("xlim1_analysis", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                            value = 0)),

        column(1,
               numericInput("xlim2_analysis", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                            value = 250)),
        column(1,
               numericInput("ylim1_analysis", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                            value = -100)),

        column(1,
               numericInput("ylim2_analysis", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                            value = 2000)),

        column(4,
               sliderInput("opacity", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Opacity')),
                           min = 0, max = 1,
                           value = 0.6, step = 0.1)
        ),

        column(4,
               sliderInput("HeightAnalysis", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )
      ),

      withSpinner(htmlOutput("overlay_plotUI"))
  )
}

Analysis_box_ui2 <- function(id) {
  box(id = "AnalysisBox2", title = p("Instability Metrics Table"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      withSpinner(DT::dataTableOutput("metrics_table_analysis", width = "100%", height = "400"))
  )
}

Analysis_box_ui2_2 <- function(id) {
  box(id = "AnalysisBox2_2", title = p("Span Modelling"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      withSpinner(DT::dataTableOutput("span_model", width = "100%", height = "400"))
  )
}

Analysis_box_ui3 <- function(id) {
  box(id = "AnalysisBox3", title = p("Settings"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(12,
               radioGroupButtons(
                 inputId = "peaks_traces",
                 label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Show Peaks or Traces')),
                 choices = c("Peaks",
                             "Traces"),
                 justified = TRUE,
                 selected = "Peaks"
               ),

               virtualSelectInput("Analysis_samples", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot to Overlay')),
                                  choices = NULL,
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your samples"
               ),

               virtualSelectInput("group_samples", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Group Samples By')),
                                  choices = "unique_id",
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your groupings"
               ),

               pickerInput("normalize", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Normalization Method')),
                           choices = c("None", "Zach", "Ricardo")),
               materialSwitch("dot_show", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show peaks/traces')), value = TRUE, status = "primary"),
               materialSwitch("line_show", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Best Fit Line')), value = FALSE, status = "primary"),

               conditionalPanel(
                 condition = 'input.line_show == true',
                 numericInput("threshold", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Only take into account peaks/traces beyond this repreat')),
                              value = 87),
                 numericInput("span", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Span')),
                              value = 0.1, max = 1,
                              step = 0.01),
               materialSwitch("CI_show", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Confidence Intervals')), value = FALSE, status = "primary"),

               conditionalPanel(
                 condition = 'input.CI_show == true',
                 numericInput("CI", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Confidence Interval')),
                              value = 0.95, max = 1,
                              step = 0.01)
               )
               )
        ))
  )
}

analysis_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module, metrics_module) {

  reactive_analysis <- reactiveValues()

  #Colours
  safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  color = color[-grep("white", color)]
  color = c(safe_colorblind_palette, color)

  observe({
    updateVirtualSelect("Analysis_samples", choices = names(upload_data$fsa_list()))
    updateVirtualSelect("group_samples", choices = colnames(upload_data$metadata_table()))
  })

  observeEvent(input$normalize, {
    if (input$normalize == "Zach") {
      updateNumericInput(session, "ylim1_analysis", value = -0.2)
    }
    else if (input$normalize == "Ricardo") {
      updateNumericInput(session, "ylim1_analysis", value = -0.02)
    }
    else if (input$normalize == "None") {
      updateNumericInput(session, "ylim1_analysis", value = -200)
    }

    if (input$normalize == "Zach") {
      updateNumericInput(session, "ylim2_analysis", value = 2)
    }
    else if (input$normalize == "Ricardo") {
      updateNumericInput(session, "ylim2_analysis", value = 0.4)
    }
    else if (input$normalize == "None") {
      updateNumericInput(session, "ylim2_analysis", value = 2000)
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
      trace <- trace::extract_trace_table(peaks_module$index_list())

      trace <- mutate(trace, Normalised_Signal = signal)

      trace <- trace[which(trace$unique_id %in% input$Analysis_samples),]

      if (input$normalize == "None") {

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")

        reactive_analysis$trace <- trace
      }

      else if (input$normalize == "Ricardo") {
        lookup <- trace[trace$calculated_repeats > input$threshold, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(sum(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")

        reactive_analysis$trace <- trace
      }

      else if (input$normalize == "Zach") {
        lookup <- trace[trace$calculated_repeats > input$threshold, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(max(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")

        reactive_analysis$trace <- trace
      }

      if (input$line_show == TRUE) {
        if (input$dot_show == TRUE) {
        p <- ggplot(reactive_analysis$trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot)) +
          geom_smooth(method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
          geom_line(alpha = input$opacity) +
          xlab("Repeats") +
          ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
          xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
          ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
          scale_color_manual(values= color) +
          labs(colour = paste(input$group_samples, collapse = ":")) +
          theme_bw()
        }
        else {
          p <- ggplot(reactive_analysis$trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot)) +
            geom_smooth(method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_bw()
        }
      }
      else {
        if (input$dot_show == TRUE) {
        p <- ggplot(reactive_analysis$trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot)) +
          geom_line(alpha = input$opacity) +
          xlab("Repeats") +
          ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
          xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
          ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
          scale_color_manual(values= color) +
          labs(colour = paste(input$group_samples, collapse = ":")) +
          theme_bw()
        }
        else {
          p <- ggplot(reactive_analysis$trace, aes(x=calculated_repeats, y=Normalised_Signal, colour = plot)) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_bw()
        }
      }
      ggplotly(p)
    }
    else {
      trace <- extract_fragments(peaks_module$index_list())

      trace <- trace[which(trace$unique_id %in% input$Analysis_samples),]

      if (input$normalize == "None") {

        if (!is.null(upload_data$metadata_table())) {
        trace <- left_join(trace, upload_data$metadata_table())
        }

        trace <- mutate(trace, Normalised_Signal = height)

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")

        reactive_analysis$trace <- trace
      }

      else if (input$normalize == "Ricardo") {
        lookup <- trace[trace$repeats > input$threshold, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(sum(height))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = height/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")

        reactive_analysis$trace <- trace
      }

      else if (input$normalize == "Zach") {
        lookup <- trace[trace$repeats > input$threshold, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(max(height))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_Signal = height/norm_factor)

        if (!is.null(upload_data$metadata_table())) {
          trace <- left_join(trace, upload_data$metadata_table())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples)]), sep = ":")

        reactive_analysis$trace <- trace
      }

      if (input$line_show == TRUE) {
        if (input$dot_show == TRUE) {
        p <- ggplot(reactive_analysis$trace, aes(x=repeats, y=Normalised_Signal, colour = plot)) +
          geom_point(alpha = input$opacity) +
          xlab("Repeats") +
          ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
          geom_smooth(method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
          xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
          ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
          scale_color_manual(values= color) +
          labs(colour = paste(input$group_samples, collapse = ":")) +
          theme_bw()
        }
        else {
          p <- ggplot(reactive_analysis$trace, aes(x=repeats, y=Normalised_Signal, colour = plot)) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            geom_smooth(method = "loess", span=input$span, level=input$CI, se = input$CI_show) +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_bw()
        }
      }
      else {
        if (input$dot_show == TRUE) {
        p <- ggplot(reactive_analysis$trace, aes(x=repeats, y=Normalised_Signal, colour = plot)) +
          geom_point(alpha = input$opacity)+
          xlab("Repeats") +
          ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
          xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
          ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
          scale_color_manual(values= color) +
          labs(colour = paste(input$group_samples, collapse = ":")) +
          theme_bw()
        }
        else {
          p <- ggplot(reactive_analysis$trace, aes(x=repeats, y=Normalised_Signal, colour = plot)) +
            xlab("Repeats") +
            ylab(if (input$normalize == "None") "Signal" else "Normalised_Signal") +
            xlim(c(input$xlim1_analysis, input$xlim2_analysis)) +
            ylim(c(input$ylim1_analysis, input$ylim2_analysis)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples, collapse = ":")) +
            theme_bw()
        }
      }
      ggplotly(p)
    }
  })

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
      model <- train(height_corrected ~ calculated_repeats, data = reactive_analysis$trace, method = "gamLoess", tuneGrid=grid, trControl = ctrl)
    }
    else {
      model <- train(height_corrected ~ repeats, data = reactive_analysis$trace, method = "gamLoess", tuneGrid=grid, trControl = ctrl)
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
