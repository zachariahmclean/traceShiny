Analysis_box_ui1 <- function(id) {
  box(id = "AnalysisBox1", title = p("Overlay Plots"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(6,
               virtualSelectInput("Analysis_samples", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot to Overlay')),
                                  choices = NULL,
                                  multiple = T,
                                  selected = NULL,
                                  search = T,
                                  hideClearButton = F,
                                  placeholder = "Please select your samples"
               )
        ),
        column(6,
               sliderInput("HeightAnalysis", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
        )),

      fluidRow(
        column(12,
               h4(HTML('<h4 style = "text-align:justify;color:#000000"><b>Plot Controls</b>')),
               fluidRow(
                 column(1,
                        numericInput("xlim_analysis1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X min')),
                                     value = 0)),

                 column(1,
                        numericInput("xlim_analysis2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">X max')),
                                     value = 250)),
                 column(1,
                        numericInput("ylim_analysis1", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y min')),
                                     value = 0)),

                 column(1,
                        numericInput("ylim_analysis2", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Y max')),
                                     value = 2000))))),

      withSpinner(htmlOutput("overlay_plotUI"))
  )
}

Analysis_box_ui2 <- function(id) {
  box(id = "AnalysisBox2", title = p("Instability Metrics Table"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      withSpinner(DT::dataTableOutput("metrics_table_analysis", width = "100%", height = "400"))
  )
}

analysis_server <- function(input, output, session, continue_module, upload_data, ladder_module, peaks_module, metrics_module) {

  reactive_analysis <- reactiveValues()

  observe({
    updateVirtualSelect("Analysis_samples", choices = names(upload_data$fsa_list()))
  })

  output$overlay_plotUI <- renderUI({
    plotlyOutput("overlay_plot", height = (300 + input$HeightAnalysis*20))
  })

  output$overlay_plot <- renderPlotly({

    if (is.null(input$Analysis_samples)) {
      # Return a blank plot if object is missing
      return(plotly::plot_ly())
    }

    xlim = c(input$xlim_analysis1, input$xlim_analysis2)
    ylim = c(input$ylim_analysis1, input$ylim_analysis2)

    trace <- trace::extract_trace_table(peaks_module$index_list())

    trace <- trace[which(trace$unique_id == input$Analysis_samples),]

    #Size
    p1 <- plot_ly(height = (300 + input$HeightAnalysis*20)) %>%
      layout(title = "",
             xaxis = list(title = "Repeats", range = xlim),
             yaxis = list(title = "Signal", range = ylim)
      )

    ## Add the traces one at a time
    for (i in unique(trace$unique_id)) {
      p1 <- p1 %>% add_trace(y = trace[which(trace$unique_id == i),]$signal, x = trace[which(trace$unique_id == i),]$calculated_repeats, name = gsub(".fsa", "", unique(trace[which(trace$unique_id == i),]$unique_id)),
                             mode="lines")
    }
    p1
  })

  output$metrics_table_analysis <- DT::renderDataTable({
    metrics_module$metrics_table()

    datatable(metrics_module$metrics_table(),
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
