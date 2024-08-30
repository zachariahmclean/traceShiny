# modules -----------------------------------------------------------------
sample_selection_module <- function(id, fragment_trace_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ladder_warning_samples <- shiny::reactive({
      sapply(
        shiny::reactiveValuesToList(fragment_trace_list),
        function(x) {
          if (is.null(tryCatch(ladder_rsq_warning_helper(x, 0.998),
                               warning = function(w) w
          ))) {
            FALSE
          } else {
            TRUE
          }
        }
      )
    })

    choices <- shiny::reactive({
      if (input$warning_checkbox) {
        names(fragment_trace_list)[which(ladder_warning_samples())]
      } else {
        names(fragment_trace_list)
      }
    })


    shiny::observe({
      shiny::updateSelectInput(session, "unique_id_selection",
                               choices = choices()
      )
    })


    selected_fragments_trace <- shiny::reactive({
      # if(is.null(input$unique_id_selection)){
      if (input$unique_id_selection == "") {
        fragment_trace_list[[choices()[1]]]
      } else {
        fragment_trace_list[[input$unique_id_selection]]
      }
    })

    return(list(
      sample = selected_fragments_trace,
      input_unique_id_selection = shiny::reactive(input$unique_id_selection)
    ))
  })
}


## plot module
plot_module_ui <- function(id) {
  shiny::tagList(
    plotly::plotlyOutput(shiny::NS(id, "plot"))
  )
}

plot_module_server <- function(id, fragment_ladder, input_unique_id_selection,
                               find_scan_max) {
  shiny::moduleServer(id, function(input, output, session) {
    # Initialize ladders as NULL
    ladders <- shiny::reactiveValues(scan = NULL, size = NULL)
    relayout_data <- shiny::reactiveVal(NULL) # Initialize relayout_data

    # Reset ladders and relayout_data when unique_id_selection changes
    shiny::observeEvent(input_unique_id_selection(), {
      ladders$scan <- NULL
      ladders$size <- NULL
      relayout_data(NULL)
    })

    shiny::observe({
      ladders$scan <- fragment_ladder()$ladder_df$scan
      ladders$size <- fragment_ladder()$ladder_df$size
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
          y = max(fragment_ladder()$trace_bp_df$ladder_signal) / 2, # Adjust Y-position as needed
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

      p <- plotly::plot_ly(fragment_ladder()$trace_bp_df, x = ~scan, y = ~ladder_signal, type = "scatter", mode = "lines")
      p <- plotly::layout(p, shapes = shapes_with_labels, annotations = text_annotations, title = fragment_ladder()$unique_id)
      # allow to edit plot by dragging lines
      plotly::config(p, edits = list(shapePosition = TRUE))
    })

    # Reset relayout_data when plot is clicked or dragged
    shiny::observeEvent(plotly::event_data("plotly_relayout"), {
      relayout_data(plotly::event_data("plotly_relayout"))
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
        window_df <- fragment_ladder()$trace_bp_df[which(fragment_ladder()$trace_bp_df$scan > selected_scan - find_scan_max() & fragment_ladder()$trace_bp_df$scan < selected_scan + find_scan_max()), ]
        new_scan <- window_df[which(window_df$ladder_signal == max(window_df$ladder_signal)), "scan"]

        # assign scan
        ladders$scan[row_index] <- new_scan[1]
      }
    })

    # Reset other reactive values if needed

    return(list(ladders = shiny::reactive(ladders)))
  })
}

## r squared table


rsq_table_ui <- function(id) {
  shiny::tagList(
    shiny::tableOutput(shiny::NS(id, "rsq_table"))
  )
}

rsq_table_server <- function(id, fragment_ladder) {
  shiny::moduleServer(id, function(input, output, session) {
    rsq_table <- shiny::reactive({
      rsq <- sapply(fragment_ladder()$mod_parameters, function(y) suppressWarnings(summary(y$mod)$r.squared))
      size_ranges <- sapply(fragment_ladder()$mod_parameters, function(y) y$mod$model$yi)
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
      rsq_table()
    })
  })
}

###
server_function <- function(input, output, session, fragment_trace_list) {
  fragment_trace_list_reactive <- shiny::reactiveValues()
  for (sample_name in names(fragment_trace_list)) {
    fragment_trace_list_reactive[[sample_name]] <- fragment_trace_list[[sample_name]]
  }
  manual_ladder_list <- shiny::reactiveValues()

  selected_fragments_trace <- sample_selection_module("sample_selection", fragment_trace_list_reactive)


  plot_output <- plot_module_server(
    "plot_module",
    selected_fragments_trace$sample,
    selected_fragments_trace$input_unique_id_selection,
    shiny::reactive(input$find_scan_max)
  )


  rsq_table_server("rsq_table", selected_fragments_trace$sample)


  # have a reactive list that gets updated when you change the stuff
  shiny::observe({
    sample_unique_id <- selected_fragments_trace$sample()$unique_id

    selected_ladder_df <- selected_fragments_trace$sample()$ladder_df
    selected_sample_scans <- selected_ladder_df[which(!is.na(selected_ladder_df$size)), "scan"]

    plot_ladder_df <- as.data.frame(shiny::reactiveValuesToList(plot_output$ladders()))
    plot_scans <- plot_ladder_df[which(!is.na(plot_ladder_df$size)), "scan"]

    # skip if ladder info hasn't been updated
    if (identical(selected_sample_scans, plot_scans)) {
      return()
    } else if (nrow(as.data.frame(shiny::reactiveValuesToList(plot_output$ladders()))) == 0) {
      return()
    }

    manual_ladder_list[[sample_unique_id]] <- as.data.frame(shiny::reactiveValuesToList(plot_output$ladders()))
    fragment_trace_list_reactive[[sample_unique_id]] <- fix_ladders_manual(
      shiny::reactiveValuesToList(fragment_trace_list_reactive)[sample_unique_id],
      shiny::reactiveValuesToList(manual_ladder_list)[sample_unique_id]
    )[[1]]
  })
}
