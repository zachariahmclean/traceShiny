ladder_box_ui1 <- function(id) {
  box(id = "LadderBoxIntro", title = strong("Find Ladders"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,

      h4(HTML('<h4 style="text-align:justify">This function takes a list of fsa files (the output from read_fsa) and identifies the ladders in the ladder channel which is used to
      call the bp size. The output is a list of fragments_traces. bp sizes are assigned using the local Southern method. Basically, for each data point, linear models are made for
      the lower and upper 3 size standard and the predicted sizes are averaged. <br>

      The ladder peaks are assigned from largest to smallest. I would recommend excluding size standard peaks less than 50 bp (eg size standard 35 bp). <br><br>
      Each ladder should be manually inspected to make sure that is has been correctly assigned.')),
      br(), br(),

      fluidRow(column(3,
                      valueBox("NEW", actionBttn("LadderBoxSTART", "START",
                                                 style = "jelly",
                                                 color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

ladder_box_ui2 <- function(id) {
  box(id = "LadderBox1", title = p("Settings", help_button("Ladder_Upload")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      materialSwitch("advancesettings_Ladder", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Show Advanced Settings')), value = FALSE, status = "primary"),

      pickerInput("LadderChannel", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Ladder Channel')),
                  choices = c("DATA.105"),
                  selected = "DATA.105"),
      pickerInput("SignalChannel", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Signal Channel')),
                  choices = c("DATA.1"),
                  selected = "DATA.1"),
      pickerInput("LadderSizes", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Ladder Sizes')), choices = NULL),

      materialSwitch("spikeswitch", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Use Default Ladder Peak')), value = TRUE, status = "primary"),

      numericInput("spikelocation", h5(HTML('<h5 style = "text-align:justify;color:#000000; margin-top:-60px;">Input Custom Ladder Peak')),
                   min = 1,
                   value = 1, step = 1),

      conditionalPanel(
        condition = 'input.advancesettings_Ladder == true',
        materialSwitch("zerofloor", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Apply Zero Floor')), value = TRUE, status = "primary"),

        numericInput("ladderselectionwindow", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Spike Location')),
                     min = 1,
                     value = 5, step = 1),

        numericInput("smoothingwindow", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Smoothing Window')),
                     min = 1,
                     value = 21, step = 1),

        numericInput("maxcombinations", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Max Combinations')),
                     min = 1,
                     value = 2500000, step = 1),
      ),

      p(style="text-align: center;", actionBttn("startbuttonFindLadders", "APPLY", size = "lg"))
  )
}

ladder_box_ui3 <- function(id) {
  box(id = "LadderBox2", title = p("Interactive Ladder Fixing"),
      status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(6,
               pickerInput("unique_id_selection", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Sample Selection')), choices = NULL)
               )
        ),
      fluidRow(
        column(6,
               prettySwitch("warning_checkbox", "Select only samples with ladder warnings",
                            value = FALSE, fill = T, status = "success", inline = T),
               )
      ),
      fluidRow(
        column(6,
               sliderInput("find_scan_max", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Snap to tallest scan window')),
                           min = 1, max = 50,
                           value = 10, step = 1)
               ),
        column(6,
               sliderInput("HeightLadder", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1)
               )
      ),
      htmlOutput("text_no_data1"),
      withSpinner(htmlOutput("plotUI")),
      tableOutput("rsq_table")
  )
}


ladder_server <- function(input, output, session, upload_data, continue_module) {

  fragment_trace_list_reactive <- shiny::reactiveValues()
  manual_ladder_list <- shiny::reactiveValues()
  reactive_ladder <- reactiveValues()
  relayout_data <- shiny::reactiveVal(NULL)

  # Initialize ladders as NULL
  ladders <- shiny::reactiveValues(scan = NULL, size = NULL)

  #Load saved objects if applicable
  observe({
      reactive_ladder$ladder <- continue_module$ladders()
      ladders$scan <- continue_module$scan()
      ladders$size <- continue_module$size()
  })

  observe({
    updatePickerInput(session, 'LadderSizes', choices = upload_data$laddertable()$Expected_ladder_peaks)
    updatePickerInput(session, "unique_id_selection", choices = names(upload_data$fsa_list()))
  })

  observeEvent(input$NextButtonLoad, {

    shinyjs::hide("NextButtonLoad")

    if(input$LadderBoxIntro$collapsed == TRUE) {
      js$collapse("LadderBoxIntro")
    }

    shinyjs::hide("LadderBox1")
    shinyjs::hide("LadderBox2")
    shinyjs::hide("NextButtonLadder")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = T,
                                                              badgeColor = "green", badgeLabel = "new")
    ))
  })

  observeEvent(input$LadderBoxSTART, {

    shinyjs::hide("NextButtonLoad")

    if(input$LadderBoxIntro$collapsed == FALSE) {
      js$collapse("LadderBoxIntro")
    }
    shinyjs::show("LadderBox1")
    shinyjs::show("LadderBox2")
    shinyjs::hide("NextButtonLadder")

    updatePickerInput(session, 'LadderSizes', choices = upload_data$laddertable()$Expected_ladder_peaks)
    updatePickerInput(session, "unique_id_selection", choices = names(upload_data$fsa_list()))
  })

  observe({
    if (input$spikeswitch == T) {
      shinyjs::hide("spikelocation")
    }
    else {
      shinyjs::show("spikelocation")
    }
  })


  observeEvent(input$startbuttonFindLadders, {
    tryCatch({
      withProgress(message = 'Finding Ladders ...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     if (input$spikeswitch == T) {
                       reactive_ladder$ladder <- find_ladders(upload_data$fsa_list(),
                                                              # ladder_channel = input$LadderChannel, TOODO need to move to read_fsa!
                                                              # signal_channel = input$SignalChannel, TOODO need to move to read_fsa!
                                                              ladder_sizes = as.numeric(strsplit(input$LadderSizes, split = ",")[[1]]),
                                                              spike_location = NULL,
                                                              zero_floor = input$zerofloor,
                                                              ladder_selection_window = input$ladderselectionwindow,
                                                              smoothing_window = input$smoothingwindow,
                                                              max_combinations = input$maxcombinations,
                                                              show_progress_bar = FALSE)
                     }
                     else {
                       reactive_ladder$ladder <- find_ladders(upload_data$fsa_list(),
                                                              ladder_channel = input$LadderChannel,
                                                              signal_channel = input$SignalChannel,
                                                              ladder_sizes = as.numeric(strsplit(input$LadderSizes, split = ",")[[1]]),
                                                              spike_location = input$spikelocation,
                                                              zero_floor = input$zerofloor,
                                                              ladder_selection_window = input$ladderselectionwindow,
                                                              smoothing_window = input$smoothingwindow,
                                                              max_combinations = input$maxcombinations,
                                                              show_progress_bar = FALSE)
                     }

                     shinyjs::show("NextButtonLadder")

                     output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                      menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                                      menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = T),
                                                                      menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = F,
                                                                               badgeColor = "green", badgeLabel = "new")))
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "Analysis Failed", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observeEvent(input$NextButtonLadder, {

    shinyjs::hide("NextButtonLadder")

    if(input$PeaksBoxIntro$collapsed == TRUE) {
      js$collapse("PeaksBoxIntro")
    }
    shinyjs::hide("PeaksBox1")
    shinyjs::hide("PeaksBox2")
    shinyjs::hide("NextButtonPeaks")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload"),
                                                     menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F),
                                                     menuItem("Find Peaks", icon = icon("mountain"), tabName = "FindPeaks", selected = T,
                                                              badgeColor = "green", badgeLabel = "new")))
  })


  #####Functions
  shiny::observe({
    if (!is.null(reactive_ladder$ladder)) {
      if (input$warning_checkbox == T) {
        warning_list <- list()

        for (i in 1:length(names(reactive_ladder$ladder))) {
          warning_list[[i]] <- ladder_rsq_warning_helper(reactive_ladder$ladder[[i]], 0.998)
        }

        if (length(warning_list) == 0) {
          updatePickerInput(session, "unique_id_selection",
                            choices = names(reactive_ladder$ladder)
          )
          shinyalert("Success!", "No samples have badly fitting ladders", type = "success", confirmButtonCol = "#337ab7")
          updatePrettySwitch(session, "warning_checkbox", value = FALSE)
        }
        else {
          updatePickerInput(session, "unique_id_selection",
                            choices = names(reactive_ladder$ladder)[which(names(reactive_ladder$ladder) %in% unlist(warning_list))]
          )
        }
      }
    }
  })

  shiny::observe({
    if (is.null(reactive_ladder$ladder)) {
      if (input$warning_checkbox == T) {
        shinyalert("ERROR!", "Please run your analysis first", type = "error", confirmButtonCol = "#337ab7")
        updatePrettySwitch(session, "warning_checkbox", value = FALSE)
      }
    }
  })

  # Reset ladders and relayout_data when unique_id_selection changes
  shiny::observeEvent(input$unique_id_selection, {
    ladders$scan <- NULL
    ladders$size <- NULL
  })

  shiny::observe({
    if (!is.null(input$unique_id_selection)) {
    ladders$scan <- reactive_ladder$ladder[[input$unique_id_selection]]$ladder_df$scan
    ladders$size <- reactive_ladder$ladder[[input$unique_id_selection]]$ladder_df$size
    }
  })

  # Reset relayout_data when plot is clicked or dragged
  relayout_data <- reactive({
    event_data(event = "plotly_relayout", source = "plot_graph")
  })

  output$plotUI <- renderUI({
    plotlyOutput("plot", height = (300 + input$HeightLadder*20))
  })

  output$plot <- plotly::renderPlotly({
    if (is.null(ladders$scan) || is.null(ladders$size)) {
      # Return a blank plot if ladders are not initialized
      return(plotly::plot_ly())
    }

      shapes_with_labels <- list()
      text_annotations <- list()
      for (i in 1:length(ladders$scan)) {
        shapes_with_labels[[i]] <- list(
          type = "line",
          x0 = ladders$scan[i], # Adjust as needed for the positions of your shapes
          x1 = ladders$scan[i], # Adjust as needed for the positions of your shapes
          y0 = 0.05,
          y1 = 0.45,
          yref = "paper",
          fillcolor = "rgba(0,0,0,0)", # Transparent fill
          line = list(
            color = "black",
            width = 1
          ),
          editable = TRUE # Allow shape editing
        )

        # Add text annotation
        text_annotations[[i]] <- list(
          x = ladders$scan[i], # X-position of the text
          y = max(reactive_ladder$ladder[[input$unique_id_selection]]$trace_bp_df$ladder_signal) / 2, # Adjust Y-position as needed
          text = ladders$size[i],
          showarrow = FALSE, # Remove arrow if not desired
          textanchor = "end", # Horizontal text alignment
          yanchor = "middle", # Vertical text alignment
          font = list(
            color = "black",
            size = 10
          ),
          textangle = 270
        )
      }

      p <- plotly::plot_ly(reactive_ladder$ladder[[input$unique_id_selection]]$trace_bp_df, x = ~scan, y = ~ladder_signal, type = "scatter", mode = "lines", source = "plot_graph",
                           height = 300 + input$HeightLadder*20)
      p <- plotly::layout(p, shapes = shapes_with_labels, annotations = text_annotations, title = reactive_ladder$ladder[[input$unique_id_selection]]$unique_id)
      # allow to edit plot by dragging lines
      plotly::config(p, edits = list(shapePosition = TRUE))
  })

  # Capture relayout_data
  shiny::observe({
    if (!is.null(relayout_data())) {
      ed <- relayout_data()
      scan_positions <- ed[grepl("^shapes.*x.*", names(ed))]
      if (length(scan_positions) != 2) {
        return()
      }
      row_index <- unique(as.numeric(sub(".*\\[(.*?)\\].*", "\\1", names(scan_positions)[1])) + 1)

      # find maximal signal in the user defined region
      selected_scan <- round(as.numeric(scan_positions))[1]
      window_df <- reactive_ladder$ladder[[input$unique_id_selection]]$trace_bp_df[which(reactive_ladder$ladder[[input$unique_id_selection]]$trace_bp_df$scan > selected_scan - input$find_scan_max & reactive_ladder$ladder[[input$unique_id_selection]]$trace_bp_df$scan < selected_scan + input$find_scan_max), ]
      new_scan <- window_df[which(window_df$ladder_signal == max(window_df$ladder_signal)), "scan"]

      # assign scan
      ladders$scan[row_index] <- new_scan[1]
    }
  })

  rsq_table <- shiny::reactive({
    rsq <- sapply(reactive_ladder$ladder[[input$unique_id_selection]]$mod_parameters, function(y) suppressWarnings(summary(y$mod)$r.squared))
    size_ranges <- sapply(reactive_ladder$ladder[[input$unique_id_selection]]$mod_parameters, function(y) y$mod$model$yi)
    size_ranges_vector <- vector("numeric", ncol(size_ranges))
    for (j in seq_along(size_ranges_vector)) {
      size_ranges_vector[j] <- paste0(size_ranges[1, j], ", ", size_ranges[2, j], ", ", size_ranges[3, j])
    }

    data.frame(
      sizes = size_ranges_vector,
      r_squared = as.character(round(rsq, digits = 4))
    )
  })

  output$rsq_table <- shiny::renderTable({
    validate(
      need(!is.null(reactive_ladder$ladder), 'You must run the analysis first'))
    rsq_table()
  })

  #have a reactive list that gets updated when you change the stuff
  shiny::observe({
    if (!is.null(input$unique_id_selection)) {
    sample_unique_id <- reactive_ladder$ladder[[input$unique_id_selection]]$unique_id

    selected_ladder_df <- reactive_ladder$ladder[[input$unique_id_selection]]$ladder_df
    selected_sample_scans <- selected_ladder_df[which(!is.na(selected_ladder_df$size)), "scan"]

    plot_ladder_df <- as.data.frame(shiny::reactiveValuesToList(ladders))
    plot_scans <- plot_ladder_df[which(!is.na(plot_ladder_df$size)), "scan"]

    # skip if ladder info hasn't been updated
    if (identical(selected_sample_scans, plot_scans)) {
      return()
    } else if (nrow(as.data.frame(shiny::reactiveValuesToList(ladders))) == 0) {
      return()
    }
      manual_ladder_list[[sample_unique_id]] <- as.data.frame(shiny::reactiveValuesToList(ladders))
      reactive_ladder$ladder[[sample_unique_id]] <- ladder_fix_helper(
        reactive_ladder$ladder[[sample_unique_id]],
        shiny::reactiveValuesToList(manual_ladder_list)[[sample_unique_id]]
      )
    }
  })

  observe({
    if (is.null(ladders$scan) || is.null(ladders$size)) {
      shinyjs::show("text_no_data1")
      shinyjs::hide("rsq_table")
    }
    else {
      shinyjs::hide("text_no_data1")
      shinyjs::show("rsq_table")
    }
  })

  output$text_no_data1 <- renderUI({
        h3(HTML('<b><h3 style = "text-align:justify;color:#FF0000">Please select your inputs and press apply to start analysis.</b>'))
  })

  return(list(
    ladders = reactive(reactive_ladder$ladder),
    scan = reactive(ladders$scan),
    size = reactive(ladders$size),
    LadderChannel = reactive(input$LadderChannel),
    SignalChannel = reactive(input$SignalChannel),
    LadderSizes = reactive(input$LadderSizes),
    spikeswitch = reactive(input$spikeswitch),
    spikelocation = reactive(input$spikelocation),
    zerofloor = reactive(input$zerofloor),
    ladderselectionwindow = reactive(input$ladderselectionwindow),
    smoothingwindow = reactive(input$smoothingwindow),
    maxcombinations = reactive(input$maxcombinations)
  ))

}
