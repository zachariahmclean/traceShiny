upload_data_box_ui1 <- function(id) {
  box(id = "LoadBoxIntro", title = strong("Load your data"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,

      h4(includeHTML("data/upload/upload.html")),
      br(),

      fluidRow(column(3,
                      valueBox("NEW", actionBttn("LoadBoxSTART", "START",
                                                 style = "jelly",
                                                 color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

upload_data_box_ui2 <- function(id) {
  box(id = "LoadBox2", title = p("Data Upload", help_button("Data_Upload")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,
      radioGroupButtons(
        inputId = "DataUpload",
        label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Upload Method')),
        choices = c("fsa",
                    "Peak Table",
                    "Repeat Table",
                    "Use Example"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o",
                      style = "color: steelblue")),
        selected = "fsa"
      ),
      conditionalPanel(
        condition = 'input.DataUpload == "fsa"',
        fileInput("DataFSA", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Upload fsa File(s)')), multiple = T)
      ),
      conditionalPanel(
        condition = 'input.DataUpload == "Peak Table"',
        fileInput("DataPeak", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Upload Peak Table')))
      ),
      conditionalPanel(
        condition = 'input.DataUpload == "Repeat Table"',
        fileInput("DataTable", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Upload Repeat Table')))
      )
  )
}

upload_data_box_ui3 <- function(id) {
  box(id = "LoadBox5", title = p("Select Data Channels", help_button("Data_Channels")), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(3,
               pickerInput("sample_subset_upload", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Samples')),
                           choices = NULL)),
        column(3,
               sliderInput("HeightDataChannels", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Plot Height')),
                           min = 1, max = 100,
                           value = 20, step = 1))
      ),

      htmlOutput("plot_data_channels_UI"),

      fluidRow(
        column(3,
               pickerInput("LadderChannel", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Ladder Channel')),
                           choices = NULL)),
        column(3,
               pickerInput("SignalChannel", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Sample Channel')),
                           choices = NULL))
        ),
      p(style="text-align: center;", actionBttn("SelectionButton", "APPLY", size = "lg"))
  )
}

upload_data_box_ui4 <- function(id) {
  box(
    id = "LoadBox3", title = p("Metadata Upload", help_button("MetaData")), status = "warning", solidHeader = F,
    collapsible = T, width = NULL,

    materialSwitch(
      inputId = "DataUploadMeta",
      label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Use Example Metadata')),
      status = "primary",
      right = TRUE,
      value = FALSE
    ),

    conditionalPanel(
      condition = 'input.DataUploadMeta == false',
      fileInput("MetadataUpload", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-75px;">Metadata Upload'), downloadBttn("downloadExampleMetadata", "Download Example MetaData")),
                multiple = F),
    ),

    withSpinner(DT::dataTableOutput("Metadata_table", width = "100%", height = 400))
  )
}

upload_data_box_ui5 <- function(id) {
  box(
    id = "LoadBox4", title = p("Ladder Selection", help_button("ladder")), status = "warning", solidHeader = F,
    collapsible = T, width = NULL,

    materialSwitch("Ladder_switch", label = h4(HTML('<h4 style = "text-align:justify;color:#000000">Use Ladder Defaults')), value = TRUE, status = "primary"),

    conditionalPanel(
      condition = 'input.Ladder_switch == true',
      withSpinner(DT::dataTableOutput("ladder_table_example", width = "100%"))
    ),

    conditionalPanel(
      condition = 'input.Ladder_switch == false',

      fluidRow(
        column(3,
               textInput("LadderUploadName", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Input Custom Ladders Name')))
        ),
        column(5,
               textInput("LadderUpload", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Input Custom Ladders Size (each size must be separated by a comma, see examples in the table below)')))
        ),
        column(3,
               p(style="text-align: left;", actionBttn("Ladderbutton", "APPLY", size = "lg")))

        ),
        withSpinner(DT::dataTableOutput("ladder_table", width = "100%", height = "400"))
      )
    )
}

#####SERVER#####
upload_data_box_server <- function(input, output, session, continue_module) {

  reactive <- reactiveValues()

  #Load saved objects if applicable
  observe({
    reactive$laddertable <- continue_module$laddertable()
    reactive$fsa_list <- continue_module$fsa_list()
    reactive$metadata_table <- continue_module$metadata_table()
  })

  # #help files
  help_click("Data_Upload", helpfile = "data/upload/upload_data.html")
  help_click("Data_Channels", helpfile = "data/upload/fsa_channels.html")
  help_click("MetaData", helpfile = "data/upload/metadata.html")
  help_click("ladder", helpfile = "data/upload/ladder.html")

  observeEvent(input$LoadBoxSTART, {
    if (input$LoadBoxIntro$collapsed == FALSE) {
      js$collapse("LoadBoxIntro")
    }
    shinyjs::show("LoadBox2")
    shinyjs::hide("LoadBox5")
    shinyjs::hide("LoadBox3")
    shinyjs::hide("LoadBox4")
    shinyjs::hide("NextButtonLoad")

    updateRadioGroupButtons(session, "DataUpload", selected = "fsa")

    reactive$laddertable <- NULL
    reactive$fsa_list <- NULL
    reactive$metadata_table <- NULL
  })

  observe({
    if(input$DataUpload == "Use Example"){
      reactive$fsa_list <- trace::cell_line_fsa_list

      shinyjs::show("LoadBox2")
      shinyjs::show("LoadBox5")
      shinyjs::hide("LoadBox3")
      shinyjs::hide("LoadBox4")
      shinyjs::hide("NextButtonLoad")
    }
    else {
      updateMaterialSwitch(session, "DataUploadMeta", value = F)
      shinyjs::show("LoadBox2")
      shinyjs::hide("LoadBox5")
      shinyjs::hide("LoadBox3")
      shinyjs::hide("LoadBox4")
      shinyjs::hide("NextButtonLoad")
    }

    if(input$DataUploadMeta == T) {
      reactive$metadata_table <- trace::metadata
      shinyjs::show("LoadBox2")
      shinyjs::show("LoadBox5")
      shinyjs::show("LoadBox3")
      shinyjs::show("LoadBox4")
      shinyjs::show("NextButtonLoad")
    }
    else if(input$DataUploadMeta == F) {
      shinyjs::hide("LoadBox4")
      shinyjs::hide("NextButtonLoad")
      reactive$metadata_table <- NULL
    }

    if(input$DataUpload == "fsa"){
      shinyjs::hide("DataUploadMeta")
    }
    else {
      shinyjs::show("DataUploadMeta")
    }
  })


  observeEvent(input$SelectionButton, {
    updateMaterialSwitch(session, "DataUploadMeta", value = F)
    shinyjs::show("LoadBox2")
    shinyjs::show("LoadBox5")
    shinyjs::show("LoadBox3")
    shinyjs::show("LoadBox4")
    shinyjs::show("NextButtonLoad")
    shinyalert("SUCCESS!", "Channels Selected!", type = "success", confirmButtonCol = "#337ab7")
  })

  observeEvent(input$DataFSA, {
    tryCatch({
      withProgress(message = 'Loading fsa file(s)...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     filesdir = dirname(input$DataFSA[[1, 'datapath']])

                     for (i in 1:length(input$DataFSA$name)) {
                       file.rename(input$DataFSA[[i, 'datapath']], paste0(filesdir,'/',input$DataFSA$name[i]))
                     }

                     reactive$fsa_list <- read_fsa(list.files(full.names = TRUE, filesdir))

                     shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                     shinyjs::show("LoadBox2")
                     shinyjs::show("LoadBox5")
                     shinyjs::hide("LoadBox3")
                     shinyjs::hide("LoadBox4")
                     shinyjs::hide("NextButtonLoad")
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "File is not in correct format.", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  observe({
    if (!is.null(reactive$fsa_list)) {
      updatePickerInput(session, "sample_subset_upload", choices = names(reactive$fsa_list))
      updatePickerInput(session, "LadderChannel", choices = names(reactive$fsa_list[[1]]$fsa$Data)[grep("DATA.", names(reactive$fsa_list[[1]]$fsa$Data))], selected = "DATA.105")
      updatePickerInput(session, "SignalChannel", choices = names(reactive$fsa_list[[1]]$fsa$Data)[grep("DATA.", names(reactive$fsa_list[[1]]$fsa$Data))], selected = "DATA.1")
    }
  })

  output$plot_data_channels_UI <- renderUI({
    plotlyOutput("plot_data_channels", height = (300 + input$HeightDataChannels*20))
  })

  output$plot_data_channels <- renderPlotly({

    # Extract the names of the data channels that match "DATA."
    data_channels <- names(reactive$fsa_list[[input$sample_subset_upload]]$fsa$Data)[grep("DATA.", names(reactive$fsa_list[[input$sample_subset_upload]]$fsa$Data))]
    raw_data_list <- reactive$fsa_list[[input$sample_subset_upload]]$fsa$Data[data_channels]

    df <- as.data.frame(do.call(cbind.fill, raw_data_list))
    colnames(df) <- names(raw_data_list)
    df <- tibble::rowid_to_column(df, "ID")

    p <- plot_ly(height = (300 + input$HeightDataChannels*20)) %>%
      layout(title = input$sample_subset_upload,
             xaxis = list(title = "Scan"),
             yaxis = list(title = "Signal")
      )

    ## Add the traces one at a time
    for(i in 2:ncol(df)){
      p <- p %>%  add_trace(y = df[,i], x = df[,1], name = colnames(df)[i],
                            mode="lines")
    }

    p
  })


  observeEvent(input$MetadataUpload, {
    tryCatch({
      withProgress(message = 'Loading metadata file(s)...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     reactive$metadata_table <- read.csv(input$MetadataUpload$datapath)

                     if (any(grepl("unique_id", colnames(reactive$metadata_table))) &&
                         any(grepl("metrics_group_id", colnames(reactive$metadata_table))) &&
                         any(grepl("metrics_baseline_control", colnames(reactive$metadata_table))) &&
                         any(grepl("batch_run_id", colnames(reactive$metadata_table))) &&
                         any(grepl("batch_sample_id", colnames(reactive$metadata_table)))
                     )
                     {
                       if (all(reactive$metadata_table$unique_id %in% names(reactive$fsa_list))) {

                         if (any(grepl("TRUE", reactive$metadata_table$metrics_baseline_control))) {

                           if (all(unique(reactive$metadata_table[-which(reactive$metadata_table$metrics_baseline_control == TRUE), ]$metrics_group_id) %in%
                                   unique(reactive$metadata_table[which(reactive$metadata_table$metrics_baseline_control == TRUE), ]$metrics_group_id))) {

                             shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                             shinyjs::show("LoadBox2")
                             shinyjs::show("LoadBox5")
                             shinyjs::show("LoadBox3")
                             shinyjs::show("LoadBox4")
                             shinyjs::show("NextButtonLoad")

                             output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                              menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                              menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F,
                                                                                       badgeColor = "green", badgeLabel = "new")))

                           }
                           else {
                             shinyalert("ERROR!", "Not all samples have matching controls. Please check your column metrics_baseline_control.", type = "error", confirmButtonCol = "#337ab7")
                           }
                         }
                         else {
                           shinyalert("WARNING!", "Control samples not detected in uploaded dataframe, please check your column metrics_baseline_control. You may proceed but some functions may not be avaliable.", type = "warning", confirmButtonCol = "#337ab7")
                           shinyjs::show("LoadBox2")
                           shinyjs::show("LoadBox5")
                           shinyjs::show("LoadBox3")
                           shinyjs::show("LoadBox4")
                           shinyjs::show("NextButtonLoad")

                           output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                            menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                            menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F,
                                                                                     badgeColor = "green", badgeLabel = "new")))
                         }
                       }
                       else {
                         shinyalert("ERROR!", "fsa filenames does not match the unique ID names in the metadata (or is not complete). Please check your loaded fsa files and corresponding metadata names.", type = "error", confirmButtonCol = "#337ab7")
                         shinyjs::hide("NextButtonLoad")
                         }
                     }
                     else {
                       shinyalert("ERROR!", "File is not in correct format. Please check if your column names, make sure it is in the correct format.", type = "error", confirmButtonCol = "#337ab7")
                       shinyjs::hide("NextButtonLoad")
                     }
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "File is not in correct format. Please check if your file is in csv (comma separated values) format", type = "error", confirmButtonCol = "#337ab7")
      shinyjs::hide("NextButtonLoad")
    })
  })

  output$Metadata_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive$metadata_table), 'You must load your data first...'))
    reactive$metadata_table
  },  options = list(scrollX = TRUE))

  output$downloadExampleMetadata <- downloadHandler(
    filename = function() {
      paste("example_metadata.csv")
    },

    content = function(file) {

      write.csv(trace::metadata, file, row.names = F, col.names = T)
    }
  )

  observeEvent(input$Ladderbutton, {
    tryCatch({
      withProgress(message = 'Loading Ladder file...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     reactive$laddertable

                     ladderID <- read.csv("data/default_ladderids.csv")

                     if (grepl("(\\d+,){1,}", input$LadderUpload) && !grepl("[a-zA-Z]", input$LadderUpload)) {
                     entry <- as.data.frame(cbind(input$LadderUploadName, input$LadderUpload))

                     colnames(entry) <- colnames(ladderID)

                     if (is.null(reactive$laddertable)) {
                       reactive$laddertable <- rbind(ladderID, entry)
                     }
                     else {
                       reactive$laddertable <- rbind(reactive$laddertable, entry)
                     }

                       shinyjs::show("LoadBox2")
                       shinyjs::show("LoadBox5")
                       shinyjs::show("LoadBox3")
                       shinyjs::show("LoadBox4")
                       shinyjs::show("NextButtonLoad")

                       output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                        menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                        menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F,
                                                                                 badgeColor = "green", badgeLabel = "new")
                       ))
                     }
                     else {
                       shinyalert("ERROR!", "Ladder size input is not in correct format. Please follow the format shown in the table below.", type = "error", confirmButtonCol = "#337ab7")

                       output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                        menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T))
                       )
                     }
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "Something broke... Panic!", type = "error", confirmButtonCol = "#337ab7")
    })
  })

  output$ladder_table_example <- DT::renderDataTable({
    reactive$laddertable <- read.csv("data/default_ladderids.csv")

    reactive$laddertable
  },  options = list(scrollX = TRUE))

  output$ladder_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive$laddertable), 'You must load your data first...'))
    reactive$laddertable
  },  options = list(scrollX = TRUE))

  return(list(
    laddertable = reactive(reactive$laddertable),
    fsa_list = reactive(reactive$fsa_list),
    metadata_table = reactive(reactive$metadata_table),
    DataUpload = reactive(input$DataUpload),
    DataUploadMeta = reactive(input$DataUploadMeta),
    Ladder_switch = reactive(input$DataUpload)
  ))
}


