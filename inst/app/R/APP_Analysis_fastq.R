Analysis2_box_ui1 <- function(id) {
  box(id = "Analysis2Box1", title = p("Overlay Plots"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(1,
               numericInput("xlim1_analysis2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                            value = 0)),

        column(1,
               numericInput("xlim2_analysis2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                            value = 250)),
        column(1,
               numericInput("ylim1_analysis2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                            value = -100)),

        column(1,
               numericInput("ylim2_analysis2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                            value = 2000)),

        column(4,
               sliderInput("opacity2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Opacity')),
                           min = 0, max = 1,
                           value = 0.6, step = 0.1)
        ),

        column(4,
               sliderInput("HeightAnalysis2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )
      ),

      withSpinner(htmlOutput("overlay_plot2UI")),

      fluidRow(
        column(1,
               colorSelectorInput(
                 inputId = "my2color1", label = "Pick a color for trace 1:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#88CCEE"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color2", label = "Pick a color for trace 2:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#CC6677"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color3", label = "Pick a color for trace 3:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#DDCC77"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color4", label = "Pick a color for trace 4:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#117733"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color5", label = "Pick a color for trace 5:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#332288"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color6", label = "Pick a color for trace 6:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#AA4499"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color7", label = "Pick a color for trace 7:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#44AA99"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color8", label = "Pick a color for trace 8:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#999933"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color9", label = "Pick a color for trace 9:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#882255"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color10", label = "Pick a color for trace 10:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#661100"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color11", label = "Pick a color for trace 11:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#6699CC"
               )),
        column(1,
               colorSelectorInput(
                 inputId = "my2color12", label = "Pick a color for trace 12:",
                 choices = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"),
                 selected = "#888888"
               )),
      )
  )
}

Analysis2_box_ui2 <- function(id) {
  box(id = "Analysis2Box2", title = p("Instability Metrics Table"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(6,
               p(style="text-align: left;", actionButton("clear2", "Clear Rows"))
        ),
        column(6,
               p(style="text-align: right;", downloadButton("metrics_table_analysis_download2"))
        )
      ),
        withSpinner(DT::dataTableOutput("metrics_table_analysis2", width = "100%", height = "400"))
      )
}

Analysis2_box_ui3 <- function(id) {
  box(id = "Analysis2Box3", title = p("Settings"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(12,
               radioGroupButtons(
                 inputId = "points_histo2",
                 label = h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Plot Option')),
                 choices = c("Points",
                             "Histogram"),
                 justified = TRUE,
                 selected = "Histogram"
               ),

               virtualSelectInput("Analysis_samples2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot to Overlay')),
                                  choices = NULL,
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your samples"
               ),

               virtualSelectInput("group_samples2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Group Samples By')),
                                  choices = "unique_id",
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your groupings"
               ),

               pickerInput("normalize2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Normalization Method')),
                           choices = c("None", "Highest Peak", "Sum Of All Peaks")),

               htmlOutput("Normalization_settings2"),
               fluidRow(
                 column(6,
                        numericInput("threshold_min2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Min Repeat Size')),
                                     value = 10)),
                 column(6,
                        numericInput("threshold_max2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Max Repeat Size')),
                                     value = 500))
               ),
               materialSwitch("index_normalize2", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Set Index Repeat as Zero')), value = FALSE, status = "primary"),
               materialSwitch("dot_show2", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show peaks/traces')), value = TRUE, status = "primary"),
               materialSwitch("line_show2", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show Best Fit Line')), value = FALSE, status = "primary"),

               conditionalPanel(
                 condition = 'input.line_show2 == true',
                 numericInput("span2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Span')),
                              value = 0.1, max = 1,
                              step = 0.01),
                 materialSwitch("CI_show2", label = h5(HTML('<h5 style = "text-align:justify;color:#000000">Show Confidence Intervals')), value = FALSE, status = "primary"),

                 conditionalPanel(
                   condition = 'input.CI_show2 == true',
                   numericInput("CI2", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-50px;">Confidence Interval')),
                                value = 0.95, max = 1,
                                step = 0.01)
                 )
               )
        ))
  )
}

analysis2_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module, metrics_module, analysis_module, metrics2_module) {

  reactive_analysis2 <- reactiveValues()

  observe({
    updateVirtualSelect("Analysis_samples2", choices = names(metrics2_module$peak_list()))
    updateVirtualSelect("group_samples2", choices = colnames(upload_data$metadata_table_fastq()))
  })

  #Download
  output$metrics_table_analysis_download2 <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_Instability_Metrics_Table_with_Metadata.csv")
    },
    content = function(file) {
      if (!is.null(upload_data$metadata_table_fastq())) {
        df <- dplyr::left_join(upload_data$metadata_table_fastq(), metrics2_module$metrics_table())
      }
      else {
        df <- arrange(metrics2_module$metrics_table(), unique_id)
      }

      write.csv(df, file, row.names = F, col.names = T)
    }
  )

  observeEvent(input$clear2, {
    proxy %>% selectRows(NULL)
    updateVirtualSelect("Analysis_samples2", choices = names(metrics2_module$peak_list()))
  })

  observeEvent(input$NextButtonMetrics2, {

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = F),
                                                     menuItem("Analysis", icon = icon("magnifying-glass-chart"), tabName = "Analysis2", selected = T,
                                                              badgeColor = "green", badgeLabel = "new")))
  })

  output$Normalization_settings2 <- renderUI({
    h5(HTML('<b><h5 style = "text-align:justify;">Normalization Settings</b>'))
  })

  observe({
    if (input$normalize2 == "None") {
      shinyjs::hide("threshold_min2")
      shinyjs::hide("threshold_max2")
      shinyjs::hide("Normalization_settings2")
    }
    else {
      shinyjs::show("threshold_max2")
      shinyjs::show("threshold_min2")
      shinyjs::show("Normalization_settings2")
    }
  })

  observeEvent(input$normalize2, {
    if (!is.null(metrics2_module$peak_list())) {
      if (input$normalize2 == "Highest Peak") {
        updateNumericInput(session, "ylim1_analysis2", value = -0.3)
      }
      else if (input$normalize2 == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim1_analysis2", value = -0.03)
      }
      else if (input$normalize2 == "None") {
        updateNumericInput(session, "ylim1_analysis2", value = -2)
      }

      if (input$normalize2 == "Highest Peak") {
        updateNumericInput(session, "ylim2_analysis2", value = 1.2)
      }
      else if (input$normalize2 == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim2_analysis2", value = 0.3)
      }
      else if (input$normalize2 == "None") {
        updateNumericInput(session, "ylim2_analysis2", value = max(metrics2_module$metrics_table()$modal_peak_repeat, na.rm = T) + 300)
      }
    }
  })

  observe({
    if (!is.null(metrics2_module$peak_list()) && !is.null(input$sample_subset_metrics2)) {
      if (!is.null(metrics2_module$metrics_table()) && any(!is.na(metrics2_module$metrics_table()$modal_peak_repeat))) {
      updateNumericInput(session, "xlim1_analysis2", value = min(metrics2_module$metrics_table()$modal_peak_repeat, na.rm = T) - 50)
      updateNumericInput(session, "xlim2_analysis2", value = max(metrics2_module$metrics_table()$modal_peak_repeat, na.rm = T) + 50)
      updateNumericInput(session, "ylim1_analysis2", value = -2)
      updateNumericInput(session, "ylim2_analysis2", value = max(metrics2_module$metrics_table()$modal_peak_signal, na.rm = T) + 300)
      }
      else {
        updateNumericInput(session, "xlim1_analysis2", value = 0)
        updateNumericInput(session, "xlim2_analysis2", value = 250)
        updateNumericInput(session, "ylim1_analysis2", value = -200)
        updateNumericInput(session, "ylim2_analysis2", value = 2000)
      }

    if (input$index_normalize2 == TRUE) {
      updateNumericInput(session, "xlim1_analysis2", value = -50)
      updateNumericInput(session, "xlim2_analysis2", value = 50)

      if (input$normalize2 == "Highest Peak") {
        updateNumericInput(session, "ylim1_analysis2", value = -0.3)
      }
      else if (input$normalize2 == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim1_analysis2", value = -0.03)
      }
      else if (input$normalize2 == "None") {
        updateNumericInput(session, "ylim1_analysis2", value = -2)
      }

      if (input$normalize2 == "Highest Peak") {
        updateNumericInput(session, "ylim2_analysis2", value = 1.2)
      }
      else if (input$normalize2 == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim2_analysis2", value = 0.3)
      }
      else if (input$normalize2 == "None") {
        updateNumericInput(session, "ylim2_analysis2", value = max(metrics2_module$metrics_table()$modal_peak_signal, na.rm = T) + 300)
      }
    }
    else {
      if (!is.null(metrics2_module$metrics_table()) && any(!is.na(metrics2_module$metrics_table()$modal_peak_repeat))) {
      updateNumericInput(session, "xlim1_analysis2", value = min(metrics2_module$metrics_table()$modal_peak_repeat, na.rm = T) - 50)
      updateNumericInput(session, "xlim2_analysis2", value = max(metrics2_module$metrics_table()$modal_peak_repeat, na.rm = T) + 50)
      }
      else {
        updateNumericInput(session, "xlim1_analysis2", value = 0)
        updateNumericInput(session, "xlim2_analysis2", value = 250)
      }

      if (input$normalize2 == "Highest Peak") {
        updateNumericInput(session, "ylim1_analysis2", value = -0.3)
      }
      else if (input$normalize2 == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim1_analysis2", value = -0.03)
      }
      else if (input$normalize2 == "None") {
        updateNumericInput(session, "ylim1_analysis2", value = -2)
      }

      if (input$normalize2 == "Highest Peak") {
        updateNumericInput(session, "ylim2_analysis2", value = 1.2)
      }
      else if (input$normalize2 == "Sum Of All Peaks") {
        updateNumericInput(session, "ylim2_analysis2", value = 0.3)
      }
      else if (input$normalize2 == "None") {
        updateNumericInput(session, "ylim2_analysis2", value = max(metrics2_module$metrics_table()$modal_peak_signal, na.rm = T) + 300)
      }
    }
    }
  })

  output$overlay_plot2UI <- renderUI({
    plotlyOutput("overlay_plot2", height = (300 + input$HeightAnalysis2*20))
  })

  output$overlay_plot2 <- renderPlotly({

    #Colours
    set.seed(001)
    color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    color = color[-grep("white", color)]
    color = c(input$my2color1, input$my2color2, input$my2color3, input$my2color4, input$my2color5, input$my2color6,
              input$my2color7, input$my2color8, input$my2color9, input$my2color10, input$my2color11, input$my2color12,
              palette(), sample(color))
    color = color[-grep("black", color)]

    if (is.null(input$Analysis_samples2)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    if (input$points_histo2 == "Histogram") {

      trace <- extract_fragments(metrics2_module$peak_list())

      trace <- mutate(trace, Normalised_signal = signal)

      trace <- trace[which(trace$unique_id %in% input$Analysis_samples2),]

      if (input$normalize2 == "None") {

        if (!is.null(upload_data$metadata_table_fastq())) {
          trace <- left_join(trace, upload_data$metadata_table_fastq())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples2)]), sep = ":")
      }

      else if (input$normalize2 == "Sum Of All Peaks") {
        lookup <- trace[trace$repeats > input$threshold_min2, ]
        lookup <- lookup[lookup$repeats < input$threshold_max2, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(sum(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table_fastq())) {
          trace <- left_join(trace, upload_data$metadata_table_fastq())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples2)]), sep = ":")
      }

      else if (input$normalize2 == "Highest Peak") {
        lookup <- trace[trace$repeats > input$threshold_min2, ]
        lookup <- lookup[lookup$repeats < input$threshold_max2, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(max(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table_fastq())) {
          trace <- left_join(trace, upload_data$metadata_table_fastq())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples2)]), sep = ":")
      }

      if (input$index_normalize2 == TRUE) {
        trace$repeats <- trace$repeats - trace$index_repeat
      }

      reactive_analysis2$trace <- trace

      if (input$line_show2 == TRUE) {
        if (input$dot_show2 == TRUE) {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, fill = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_smooth(aes(x=repeats, y=Normalised_signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span2, level=input$CI2, se = input$CI_show2) +
            geom_bar(aes(group = unique_id), alpha = input$opacity2, stat='identity', position = position_dodge(width = 0, preserve = "single"), width = length(unique(trace$unique_id))) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_fill_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
        else {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_smooth(aes(x=repeats, y=Normalised_signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span2, level=input$CI2, se = input$CI_show2) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
      }
      else {
        if (input$dot_show2 == TRUE) {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, fill = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_bar(aes(group = unique_id), alpha = input$opacity2, stat='identity', position = position_dodge(width = 0, preserve = "single"), width = length(unique(trace$unique_id))) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_fill_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
        else {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_fill_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
      }
      ggplotly(p, tooltip="text", height = (300 + input$HeightAnalysis2*20))
    }
    else {
      trace <- extract_fragments(metrics2_module$peak_list())

      trace <- mutate(trace, Normalised_signal = signal)

      trace <- trace[which(trace$unique_id %in% input$Analysis_samples2),]

      if (input$normalize2 == "None") {

        if (!is.null(upload_data$metadata_table_fastq())) {
          trace <- left_join(trace, upload_data$metadata_table_fastq())
        }

        trace <- mutate(trace, Normalised_signal = signal)

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples2)]), sep = ":")
      }

      else if (input$normalize2 == "Sum Of All Peaks") {
        lookup <- trace[trace$repeats > input$threshold_min2, ]
        lookup <- lookup[lookup$repeats < input$threshold_max2, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(sum(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table_fastq())) {
          trace <- left_join(trace, upload_data$metadata_table_fastq())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples2)]), sep = ":")
      }

      else if (input$normalize2 == "Highest Peak") {
        lookup <- trace[trace$repeats > input$threshold_min2, ]
        lookup <- lookup[lookup$repeats < input$threshold_max2, ]

        lookup <- lookup %>% group_by(unique_id) %>% dplyr::summarise(max(signal))
        colnames(lookup)[2] <- c("norm_factor")

        trace <- left_join(trace, lookup)

        trace <- mutate(trace, Normalised_signal = signal/norm_factor)

        if (!is.null(upload_data$metadata_table_fastq())) {
          trace <- left_join(trace, upload_data$metadata_table_fastq())
        }

        trace$plot <- col_concat(as.data.frame(trace[, which(colnames(trace) %in% input$group_samples2)]), sep = ":")
      }

      if (input$index_normalize2 == TRUE) {
        trace$repeats <- trace$repeats - trace$index_repeat
      }

      reactive_analysis2$trace <- trace


      if (input$line_show2 == TRUE) {
        if (input$dot_show2 == TRUE) {

          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_point(alpha = input$opacity2) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            geom_smooth(aes(x=repeats, y=Normalised_signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span2, level=input$CI2, se = input$CI_show2) +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
        else {

          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            geom_smooth(aes(x=repeats, y=Normalised_signal, colour = plot), inherit.aes = FALSE, method = "loess", span=input$span2, level=input$CI2, se = input$CI_show2) +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
      }
      else {

        if (input$dot_show2 == TRUE) {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            geom_point(alpha = input$opacity2)+
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
        else {
          p <- ggplot(trace, aes(x=repeats, y=Normalised_signal, colour = plot,
                                                   text=paste0("Repeats: ", repeats, "\n",
                                                               if (input$normalize2 == "None") "Signal: " else "Normalised_signal: ", Normalised_signal, "\n",
                                                               "Grouping: ", plot))) +
            xlab("Repeats") +
            ylab(if (input$normalize2 == "None") "Signal" else "Normalised_signal") +
            xlim(c(input$xlim1_analysis2, input$xlim2_analysis2)) +
            ylim(c(input$ylim1_analysis2, input$ylim2_analysis2)) +
            scale_color_manual(values= color) +
            labs(colour = paste(input$group_samples2, collapse = ":")) +
            theme_bw()
        }
      }
      ggplotly(p, tooltip="text", height = (300 + input$HeightAnalysis2*20))
    }
  })

  proxy = dataTableProxy('metrics_table_analysis2')

  output$metrics_table_analysis2 <- DT::renderDataTable({

    if (!is.null(upload_data$metadata_table_fastq())) {
      df <- dplyr::left_join(upload_data$metadata_table_fastq(), metrics2_module$metrics_table())
    }
    else {
      df <- arrange(metrics2_module$metrics_table(), unique_id)
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

  observeEvent(input$metrics_table_analysis2_rows_selected, {

    list <- list()
    for (i in input$metrics_table_analysis2_rows_selected) {
      list[i] <- paste(metrics2_module$peak_list()[[i]]$unique_id)
    }

    list <- unlist(list)

    updateVirtualSelect("Analysis_samples2", selected = list)
  })

}
