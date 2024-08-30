upload_data_box_ui1 <- function(id) {
  box(id = "LoadBoxIntro", title = strong("Load your data"), status = "warning", solidHeader = F,
      collapsible = T, collapsed = T, width = 12,

      h4(HTML('<h4 style="text-align:justify">This adjusts the app settings to be optimal for different types of samples. For example, “mouse” expects only one allele.
              “Custom” allows you to upload a .json file from previous app exports. This allows you to use the exact same parameters for the app as previously used.<br>')),
      br(), br(),

      fluidRow(column(3,
                      valueBox("NEW", actionBttn("LoadBoxSTART", "START",
                                                 style = "jelly",
                                                 color = "primary"), icon = icon("paper-plane"), width = 12, color = "aqua"))
      ))
}

upload_data_box_ui3 <- function(id) {
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
  ns = NS(id)
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
      fileInput("LadderUpload", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Upload Custom Ladders')), multiple = F),
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
  help_click("Data_Upload", helpfile = "data/help/upload_data.html")
  help_click("MetaData", helpfile = "data/help/upload_metadata.html")
  help_click("ladder", helpfile = "data/help/LadderHelp.html")

  observeEvent(input$LoadBoxSTART, {
    if (input$LoadBoxIntro$collapsed == FALSE) {
      js$collapse("LoadBoxIntro")
    }
    shinyjs::show("LoadBox2")
    shinyjs::hide("LoadBox3")
    shinyjs::hide("LoadBox4")
    shinyjs::hide("NextButtonLoad")
  })

  observe({
    if(input$DataUpload == "Use Example"){
      reactive$fsa_list <- instability::cell_line_fsa_list[1:72]

      shinyjs::show("LoadBox2")
      shinyjs::show("LoadBox3")
      shinyjs::hide("LoadBox4")
      shinyjs::hide("NextButtonLoad")
    }

    if(input$DataUploadMeta == T) {
      reactive$metadata_table <- instability::metadata
      shinyjs::show("LoadBox2")
      shinyjs::show("LoadBox3")
      shinyjs::show("LoadBox4")
      shinyjs::show("NextButtonLoad")
    }
    else {
      reactive$metadata_table <- NULL
      shinyjs::hide("NextButtonLoad")
    }
  })

  observeEvent(input$DataFSA, {
    tryCatch({
      withProgress(message = 'Loading fsa file(s)...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     reactive$fsa_list <- read_fsa(input$DataFSA$datapath)
                     names(reactive$fsa_list) <- input$DataFSA$name

                     shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                     shinyjs::show("LoadBox2")
                     shinyjs::show("LoadBox3")
                     shinyjs::hide("LoadBox4")
                     shinyjs::hide("NextButtonLoad")
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "File is not in correct format.", type = "error", confirmButtonCol = "#337ab7")
      shinyjs::hide("NextButtonLoad")
    })
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
                       if (all(names(reactive$fsa_list) == reactive$metadata_table$unique_id)) {

                         if (any(grepl("TRUE", reactive$metadata_table$metrics_baseline_control))) {

                           if (all(unique(reactive$metadata_table[-which(reactive$metadata_table$metrics_baseline_control == TRUE), ]$metrics_group_id) %in%
                                   unique(reactive$metadata_table[which(reactive$metadata_table$metrics_baseline_control == TRUE), ]$metrics_group_id))) {

                             shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                             shinyjs::show("LoadBox2")
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
                         shinyalert("ERROR!", "fsa filenames does not match the unique ID names in the metadata.", type = "error", confirmButtonCol = "#337ab7")
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
      Example <- read.csv("example_data/example_metadata.csv")
      write.csv(Example, file, row.names = F, col.names = T)
    }
  )

  observeEvent(input$LadderUpload, {
    tryCatch({
      withProgress(message = 'Loading Ladder file...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     reactive$laddertable <- read.csv(input$LadderUpload$datapath)

                     if (any(grepl("Ladder_ID", colnames(reactive$laddertable))) &&
                         any(grepl("Expected_ladder_peaks", colnames(reactive$laddertable))))
                     {

                       shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                       shinyjs::show("LoadBox2")
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
                       shinyalert("ERROR!", "File is not in correct format. Please check if your column names, make sure it is in the correct format.", type = "error", confirmButtonCol = "#337ab7")
                       shinyjs::hide("NextButtonLoad")

                       output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                        menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T))
                       )
                     }
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "File is not in correct format.", type = "error", confirmButtonCol = "#337ab7")
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


