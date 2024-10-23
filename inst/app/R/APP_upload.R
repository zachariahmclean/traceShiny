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
      fluidRow(
        column(12,
               tags$a(href="javascript:history.go(0)",
                      popify(tags$i(class="fa fa-refresh fa-3x"),
                             title = "",
                             content = "Click here to delete all data and restart the Shiny session",
                             placement = "right")))),
      fluidRow(
        column(12,
               radioGroupButtons(
                 inputId = "DataUpload",
                 label = h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Upload Method')),
                 choices = c("fsa",
                             "fastq",
                             "Use Example"),
                 individual = TRUE,
                 checkIcon = list(
                   yes = tags$i(class = "fa fa-circle",
                                style = "color: steelblue"),
                   no = tags$i(class = "fa fa-circle-o",
                               style = "color: steelblue")),
                 selected = "fsa"
               ))
      ),
      conditionalPanel(
        condition = 'input.DataUpload == "fsa"',
        fileInput("DataFSA", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Upload fsa File(s)')), multiple = T, accept = c(".fsa"))
      ),
      conditionalPanel(
        condition = 'input.DataUpload == "fastq"',
        fileInput("fastq", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Upload fastq Files(s)')), multiple = T, accept = c(".fastq", ".fq"))
      )
  )
}

upload_data_box_ui_fastq1 <- function(id) {
  box(id = "LoadBox_FASTQ1", title = p("Approximate Pattern Matching"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(6,
               textInput("flank_left", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Left Flanking Sequence')), placeholder = "CAAGTCCTTC")
        ),
        column(6,
               textInput("flank_right", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Right Flanking Sequence')), placeholder = "CAACAGCCGCCACCG")
        )
      ),

      fluidRow(
        column(6,
               textInput("repeat_pattern", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Repeat Pattern')), placeholder = "CAG")
        ),
        column(6,
               numericInput("no_repeats", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Find at least this many repeats')), value = 10,
                            min = 0, step = 1)
        )
      ),

      fluidRow(
        column(6,
               numericInput("Lev_distance", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Max Levenshtein Distance')), value = 0.1,
                            min = 0, max = 1, step = 0.01)
        ),
        column(6,
               numericInput("codon_start", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Select Starting Codon Position (For Short-Hand Sequence Notation)')), value = 2,
                            min = 0, step = 1)
        )
      ),
      p(style="text-align: center;", actionBttn("Fastq_Button", "APPLY", size = "lg"))
  )
}

upload_data_box_ui_fastq2 <- function(id) {
  box(id = "LoadBox_FASTQ2", title = p("Filtered Fastq"), status = "warning", solidHeader = F,
      collapsible = T, width = NULL,

      fluidRow(
        column(3,
               pickerInput("sample_subset_fastq", h4(HTML('<h4 style = "text-align:justify;color:#000000; margin-top:-50px;">Filter Samples')),
                           choices = "ALL"))
      ),

      withSpinner(DT::dataTableOutput("fastq_filter_table", width = "100%")),

      htmlOutput("short_form"),

      withSpinner(DT::dataTableOutput("short_form_table", width = "100%", height = 200))
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

      withSpinner(htmlOutput("plot_data_channels_UI")),

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
                multiple = F, accept = c(".txt", ".csv", ".xlsx", ".xls")),
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
    reactive$df_final <- continue_module$fastq()
    reactive$metadata_table_fastq <- continue_module$metadata_table_fastq()
    reactive$df <- continue_module$All()
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
    shinyjs::hide("NextButtonLoad2")
    shinyjs::hide("LoadBox_FASTQ1")
    shinyjs::hide("LoadBox_FASTQ2")

    updateRadioGroupButtons(session, "DataUpload", selected = "fsa")

    reactive$laddertable <- NULL
    reactive$fsa_list <- NULL
    reactive$metadata_table <- NULL
    reactive$df <- NULL
    reactive$metadata_table_fastq <- NULL
    reactive$df_final <- NULL
  })

  observeEvent(input$DataUpload, {
    if(input$DataUpload == "Use Example"){
        reactive$fsa_list <- trace::cell_line_fsa_list

        reactive$df <- NULL

        shinyjs::show("LoadBox2")
        shinyjs::show("LoadBox5")
        shinyjs::hide("NextButtonLoad")
        shinyjs::hide("NextButtonLoad2")
        shinyjs::hide("LoadBox_FASTQ1")
        shinyjs::hide("LoadBox_FASTQ2")
    }
    else {
      updateMaterialSwitch(session, "DataUploadMeta", value = F)
      shinyjs::show("LoadBox2")
      shinyjs::hide("NextButtonLoad")
      shinyjs::hide("NextButtonLoad2")
      shinyjs::hide("LoadBox_FASTQ1")
      shinyjs::hide("LoadBox_FASTQ2")
    }
  })

  observeEvent(input$DataUploadMeta, {
    if(input$DataUploadMeta == T) {
      reactive$metadata_table <- trace::metadata
      shinyjs::show("LoadBox2")
      shinyjs::show("LoadBox5")
      shinyjs::show("LoadBox3")
      shinyjs::show("LoadBox4")
      shinyjs::show("NextButtonLoad")
      shinyjs::hide("NextButtonLoad2")
      shinyjs::hide("LoadBox_FASTQ1")
      shinyjs::hide("LoadBox_FASTQ2")
    }
    else if(input$DataUploadMeta == F) {
      reactive$metadata_table <- NULL
    }
  })

  observe({
    if(input$DataUpload == "fsa" | input$DataUpload == "fastq"){
      shinyjs::hide("DataUploadMeta")
    }
    else {
      shinyjs::show("DataUploadMeta")
    }
  })

  observeEvent(input$DataUpload, {
      if (input$DataUpload == "fastq" | input$DataUpload == "fsa") {
        shinyjs::show("LoadBox2")
        shinyjs::hide("LoadBox5")
        shinyjs::hide("LoadBox3")
        shinyjs::hide("LoadBox4")
        shinyjs::hide("NextButtonLoad")
        shinyjs::hide("NextButtonLoad2")
        shinyjs::hide("LoadBox_FASTQ1")
        shinyjs::hide("LoadBox_FASTQ2")
      }
      else if (input$DataUpload == "Use Example") {
        shinyjs::show("LoadBox2")
        shinyjs::show("LoadBox5")
        shinyjs::hide("LoadBox3")
        shinyjs::hide("LoadBox4")
        shinyjs::hide("NextButtonLoad")
        shinyjs::hide("NextButtonLoad2")
        shinyjs::hide("LoadBox_FASTQ1")
        shinyjs::hide("LoadBox_FASTQ2")
      }
  })


  observeEvent(input$SelectionButton, {
    updateMaterialSwitch(session, "DataUploadMeta", value = F)
    shinyjs::show("LoadBox2")
    shinyjs::show("LoadBox5")
    shinyjs::show("LoadBox3")
    shinyjs::show("LoadBox4")
    shinyjs::show("NextButtonLoad")
    shinyjs::hide("NextButtonLoad2")
    shinyjs::hide("LoadBox_FASTQ1")
    shinyjs::hide("LoadBox_FASTQ2")
    shinyalert("SUCCESS!", "Channels Selected!", type = "success", confirmButtonCol = "#337ab7")

    output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                     menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T))
    )
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

                       reactive$df <- NULL

                       shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                       shinyjs::show("LoadBox2")
                       shinyjs::show("LoadBox5")
                       shinyjs::hide("LoadBox3")
                       shinyjs::hide("LoadBox4")
                       shinyjs::hide("NextButtonLoad")
                       shinyjs::hide("NextButtonLoad2")
                       shinyjs::hide("LoadBox_FASTQ1")
                       shinyjs::hide("LoadBox_FASTQ2")

                       output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                        menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T))
                       )
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
      reactive$fsa_list <- NULL
      reactive$df <- NULL
    })
  })

  observeEvent(input$fastq, {
    tryCatch({
      withProgress(message = 'Loading fastq file(s)...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                       filesdir = dirname(input$fastq[[1, 'datapath']])

                       for (i in 1:length(input$fastq$name)) {
                         file.rename(input$fastq[[i, 'datapath']], paste0(filesdir,'/',input$fastq$name[i]))
                         incProgress(0.1 + i/10)
                       }

                       CAG <- list()
                       CAG_reverse <- list()
                       joined <- list()

                       for (i in paste0(filesdir,'/',input$fastq$name)) {
                         CAG[[i]] <- microseq::readFastq(i)
                         CAG_reverse[[i]] <- microseq::readFastq(i)
                         CAG_reverse[[i]]$Sequence <- microseq::reverseComplement(CAG_reverse[[i]]$Sequence)
                         joined[[i]] <- Map(c, CAG[[i]], CAG_reverse[[i]])
                       }

                       reactive$df <- joined

                       reactive$fsa_list <- NULL

                       shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                       shinyjs::show("LoadBox2")
                       shinyjs::show("LoadBox_FASTQ1")
                       shinyjs::show("LoadBox_FASTQ2")
                       shinyjs::hide("LoadBox5")
                       shinyjs::hide("LoadBox3")
                       shinyjs::hide("LoadBox4")
                       shinyjs::hide("NextButtonLoad")
                       shinyjs::hide("NextButtonLoad2")

                       output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                        menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T))
                       )
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
      reactive$fsa_list <- NULL
      reactive$df <- NULL
    })
  })

  observeEvent(input$Fastq_Button, {
    tryCatch({
      withProgress(message = 'Loading fastq file(s)...', style = "old",
                   value = 0, {
                     incProgress(0.1)

                     ALL <- list()
                     df <- list()

                     for (i in names(reactive$df)) {

                       ALL[[i]] <- aregexec(paste0(input$flank_left, paste0("(", input$repeat_pattern, ")", "{", input$no_repeats, ",}"),  input$flank_right), reactive$df[[i]]$Sequence, max.distance = input$Lev_distance)

                       ## Extract matches
                       df[[i]] <- regmatches(reactive$df[[i]]$Sequence, ALL[[i]])
                       names(df[[i]]) <- reactive$df[[i]]$Header
                       df[[i]] <- df[[i]][lapply(df[[i]],length)>0]

                       df[[i]] <- as.data.frame(enframe(df[[i]]))
                       df[[i]] <- unnest(df[[i]])
                       df[[i]] <- df[[i]][!duplicated(df[[i]]$name), ]
                       df[[i]] <- mutate(df[[i]], Repeat_length = paste0(round((nchar(df[[i]]$value)- nchar(paste0(input$flank_left, input$flank_right)))/nchar(input$repeat_pattern)))) %>% mutate(df[[i]], SampleID = gsub(".+/", "", i))
                       df[[i]]$Repeat_length <- as.numeric(df[[i]]$Repeat_length)
                     }

                     df <- df[lapply(df,nrow)>4]

                     reactive$df_final <- rbindlist(df)
                     reactive$df_final <- reactive$df_final[,c(1,3,2,4)]
                     colnames(reactive$df_final) <- c("Read ID", "Repeat Length", "Sequence", "SampleID")

                     value <- str_sub(reactive$df_final$Sequence, input$codon_start)

                     for (i in 1:length(value)) {
                       reactive$df_final$`Sequence Short`[i] <- paste(paste0(rle(unlist(strsplit(value[i], "(?<=.{3})", perl = TRUE)))$values, rle(unlist(strsplit(value[i], "(?<=.{3})", perl = TRUE)))$lengths), collapse = " ")
                     }

                     reactive$df_final <-reactive$df_final[,c(1, 2, 5, 3, 4)]

                     updatePickerInput(session, "sample_subset_metrics2", choices = unique(reactive$df_final$SampleID))

                     shinyjs::show("LoadBox2")
                     shinyjs::show("LoadBox_FASTQ1")
                     shinyjs::show("LoadBox_FASTQ2")
                     shinyjs::hide("LoadBox5")
                     shinyjs::show("LoadBox3")
                     shinyjs::hide("LoadBox4")
                     shinyjs::hide("NextButtonLoad")
                     shinyjs::show("NextButtonLoad2")
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", "No sequence matching patterns found!", type = "error", confirmButtonCol = "#337ab7")
      reactive$fsa_list <- NULL
      reactive$df_final <- NULL
    })
  })

  output$short_form <- renderUI({
    h4(HTML('<b><h4 style = "text-align:justify">Sequence Summary</b>'))
  })

  observe({
    if (is.null(reactive$df_final)) {
      shinyjs::hide("short_form")
    }
    else {
      shinyjs::show("short_form")
    }
  })

  output$short_form_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive$df_final), ''))

    df <- reactive$df_final %>% group_by(`Sequence Short`, SampleID) %>% summarise(n())
    colnames(df)[3] <- "Number of Appearances"
    df <- arrange(df, desc(`Number of Appearances`))
    df <- df[,c(1,3,2)]

    if (input$sample_subset_fastq == "ALL") {

      datatable(df,
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               server = TRUE,
                               paging = TRUE,
                               pageLength = 15
                ),
                selection = 'single',
                rownames = FALSE)
    }
    else {
      datatable(df[which(df$SampleID == input$sample_subset_fastq),],
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               server = TRUE,
                               paging = TRUE,
                               pageLength = 15
                ),
                selection = 'single',
                rownames = FALSE)
    }

  },  options = list(scrollX = TRUE))

  output$fastq_filter_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive$df_final), 'You must select your parameters and press APPLY...'))

    if (input$sample_subset_fastq == "ALL") {

      datatable(reactive$df_final,
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               server = TRUE,
                               paging = TRUE,
                               pageLength = 15
                ),
                selection = 'single',
                rownames = FALSE)
    }
    else {
      datatable(reactive$df_final[which(reactive$df_final$SampleID == input$sample_subset_fastq),],
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               server = TRUE,
                               paging = TRUE,
                               pageLength = 15
                ),
                selection = 'single',
                rownames = FALSE)
    }

  },  options = list(scrollX = TRUE))

  observe({
    if (!is.null(reactive$df_final)) {
      updatePickerInput(session, "sample_subset_fastq", choices = c("ALL", unique(reactive$df_final$SampleID)))
    }
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

                     if (is.null(reactive$df_final)) {

                       if (grepl(".csv$", input$MetadataUpload$datapath)) {
                         reactive$metadata_table <- read.csv(input$MetadataUpload$datapath)
                       }
                       else if (grepl(".txt$", input$MetadataUpload$datapath)) {
                         reactive$metadata_table <- read.delim(input$MetadataUpload$datapath)
                       }
                       else if (grepl(".xlsx", input$MetadataUpload$datapath)) {
                         reactive$metadata_table <- as.data.frame(read_xlsx(input$MetadataUpload$datapath, na = "NA"))
                       }
                       else if (grepl(".xls", input$MetadataUpload$datapath)) {
                         reactive$metadata_table <- as.data.frame(read_xls(input$MetadataUpload$datapath, na = "NA"))
                       }

                       reactive$metadata_table[reactive$metadata_table==""] <- NA

                       if (any(grepl("unique_id", colnames(reactive$metadata_table))) &&
                           any(grepl("metrics_group_id", colnames(reactive$metadata_table))) &&
                           any(grepl("metrics_baseline_control", colnames(reactive$metadata_table))) &&
                           any(grepl("batch_run_id", colnames(reactive$metadata_table))) &&
                           any(grepl("batch_sample_id", colnames(reactive$metadata_table))) &&
                           any(grepl("batch_sample_modal_repeat", colnames(reactive$metadata_table)))
                       )
                       {
                         reactive$metadata_table <- reactive$metadata_table[match(names(reactive$fsa_list), reactive$metadata_table$unique_id),]

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
                               shinyjs::hide("NextButtonLoad2")
                               shinyjs::hide("LoadBox_FASTQ1")
                               shinyjs::hide("LoadBox_FASTQ2")

                               output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                                menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                                menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F,
                                                                                         badgeColor = "green", badgeLabel = "new")))

                             }
                             else {
                               shinyalert("WARNING!", "Not all samples have matching controls. Please check your column metrics_baseline_control.", type = "warning", confirmButtonCol = "#337ab7")

                               shinyjs::show("LoadBox2")
                               shinyjs::show("LoadBox5")
                               shinyjs::show("LoadBox3")
                               shinyjs::show("LoadBox4")
                               shinyjs::show("NextButtonLoad")
                               shinyjs::hide("NextButtonLoad2")
                               shinyjs::hide("LoadBox_FASTQ1")
                               shinyjs::hide("LoadBox_FASTQ2")

                               output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                                menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                                menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F,
                                                                                         badgeColor = "green", badgeLabel = "new")))
                             }
                           }
                           else {
                             shinyalert("WARNING!", "Control samples not detected in uploaded dataframe, please check your column metrics_baseline_control. You may proceed but some functions may not be avaliable.", type = "warning", confirmButtonCol = "#337ab7")
                             shinyjs::show("LoadBox2")
                             shinyjs::show("LoadBox5")
                             shinyjs::show("LoadBox3")
                             shinyjs::show("LoadBox4")
                             shinyjs::show("NextButtonLoad")
                             shinyjs::hide("NextButtonLoad2")
                             shinyjs::hide("LoadBox_FASTQ1")
                             shinyjs::hide("LoadBox_FASTQ2")

                             output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                              menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                              menuItem("Find Ladders", icon = icon("water-ladder"), tabName = "FindLadders", selected = F,
                                                                                       badgeColor = "green", badgeLabel = "new")))
                           }
                         }
                         else {
                           shinyalert("ERROR!", "fsa filenames does not match the unique ID names in the metadata (or is not complete). Please check your loaded fsa files and corresponding metadata names.", type = "error", confirmButtonCol = "#337ab7")
                           shinyjs::hide("NextButtonLoad")
                           shinyjs::hide("NextButtonLoad2")
                         }
                       }
                       else {
                         shinyalert("ERROR!", "File is not in correct format. Please check if your column names, make sure it is in the correct format.", type = "error", confirmButtonCol = "#337ab7")
                         shinyjs::hide("NextButtonLoad")
                         shinyjs::hide("NextButtonLoad2")
                       }
                     }
                     else if (!is.null(reactive$df_final)) {
                       if (grepl(".csv$", input$MetadataUpload$datapath)) {
                         reactive$metadata_table_fastq <- read.csv(input$MetadataUpload$datapath)
                       }
                       else if (grepl(".txt$", input$MetadataUpload$datapath)) {
                         reactive$metadata_table_fastq <- read.delim(input$MetadataUpload$datapath)
                       }
                       else if (grepl(".xlsx", input$MetadataUpload$datapath)) {
                         reactive$metadata_table_fastq <- as.data.frame(read_xlsx(input$MetadataUpload$datapath, na = "NA"))
                       }
                       else if (grepl(".xls", input$MetadataUpload$datapath)) {
                         reactive$metadata_table_fastq <- as.data.frame(read_xls(input$MetadataUpload$datapath, na = "NA"))
                       }

                       reactive$metadata_table_fastq[reactive$metadata_table_fastq==""] <- NA

                       if (any(grepl("unique_id", colnames(reactive$metadata_table_fastq))) &&
                           any(grepl("metrics_group_id", colnames(reactive$metadata_table_fastq))) &&
                           any(grepl("metrics_baseline_control", colnames(reactive$metadata_table_fastq))) &&
                           any(grepl("batch_run_id", colnames(reactive$metadata_table_fastq))) &&
                           any(grepl("batch_sample_id", colnames(reactive$metadata_table_fastq))) &&
                           any(grepl("batch_sample_modal_repeat", colnames(reactive$metadata_table_fastq)))
                       )
                       {
                         reactive$metadata_table_fastq <- reactive$metadata_table_fastq[match(unique(reactive$df_final$SampleID), reactive$metadata_table_fastq$unique_id),]

                         if (all(reactive$metadata_table_fastq$unique_id %in% unique(reactive$df_final$SampleID))) {

                           if (any(grepl("TRUE", reactive$metadata_table_fastq$metrics_baseline_control))) {

                             if (all(unique(reactive$metadata_table_fastq[-which(reactive$metadata_table_fastq$metrics_baseline_control == TRUE), ]$metrics_group_id) %in%
                                     unique(reactive$metadata_table_fastq[which(reactive$metadata_table_fastq$metrics_baseline_control == TRUE), ]$metrics_group_id))) {

                               shinyalert("SUCCESS!", "File uploaded successfully.", type = "success", confirmButtonCol = "#337ab7")

                               shinyjs::show("LoadBox2")
                               shinyjs::hide("LoadBox5")
                               shinyjs::show("LoadBox3")
                               shinyjs::hide("LoadBox4")
                               shinyjs::hide("NextButtonLoad")
                               shinyjs::show("NextButtonLoad2")
                               shinyjs::show("LoadBox_FASTQ1")
                               shinyjs::show("LoadBox_FASTQ2")

                               output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                                menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                                menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = F,
                                                                                         badgeColor = "green", badgeLabel = "new")))

                             }
                             else {
                               shinyalert("WARNING!", "Not all samples have matching controls. Please check your column metrics_baseline_control.", type = "warning", confirmButtonCol = "#337ab7")

                               shinyjs::show("LoadBox2")
                               shinyjs::hide("LoadBox5")
                               shinyjs::show("LoadBox3")
                               shinyjs::hide("LoadBox4")
                               shinyjs::hide("NextButtonLoad")
                               shinyjs::show("NextButtonLoad2")
                               shinyjs::show("LoadBox_FASTQ1")
                               shinyjs::show("LoadBox_FASTQ2")

                               output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                                menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                                menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = F,
                                                                                         badgeColor = "green", badgeLabel = "new")))
                             }
                           }
                           else {
                             shinyalert("WARNING!", "Control samples not detected in uploaded dataframe, please check your column metrics_baseline_control. You may proceed but some functions may not be avaliable.", type = "warning", confirmButtonCol = "#337ab7")
                             shinyjs::show("LoadBox2")
                             shinyjs::hide("LoadBox5")
                             shinyjs::show("LoadBox3")
                             shinyjs::hide("LoadBox4")
                             shinyjs::hide("NextButtonLoad")
                             shinyjs::show("NextButtonLoad2")
                             shinyjs::show("LoadBox_FASTQ1")
                             shinyjs::show("LoadBox_FASTQ2")

                             output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                              menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T),
                                                                              menuItem("Instability Metrics", icon = icon("table"), tabName = "InstabilityMetrics2", selected = F,
                                                                                       badgeColor = "green", badgeLabel = "new")))
                           }
                         }
                         else {
                           shinyalert("ERROR!", "fsa filenames does not match the unique ID names in the metadata (or is not complete). Please check your loaded fsa files and corresponding metadata names.", type = "error", confirmButtonCol = "#337ab7")
                           shinyjs::hide("NextButtonLoad")
                           shinyjs::hide("NextButtonLoad2")
                         }
                       }
                       else {
                         shinyalert("ERROR!", "File is not in correct format. Please check if your column names, make sure it is in the correct format.", type = "error", confirmButtonCol = "#337ab7")
                         shinyjs::hide("NextButtonLoad")
                         shinyjs::hide("NextButtonLoad2")
                       }
                     }
                   })
    },
    error = function(e) {
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
      reactive$metadata_table <- NULL
      reactive$metadata_table_fastq <- NULL
      shinyjs::hide("NextButtonLoad")
      shinyjs::hide("NextButtonLoad2")
    })
  })

  output$Metadata_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive$metadata_table) | !is.null(reactive$metadata_table_fastq), 'No metadata loaded. If you would like to skip this step please scroll to the bottom of the page and press the next step button.'))

    if (is.null(reactive$df_final)) {
      datatable(reactive$metadata_table,
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               server = TRUE,
                               paging = TRUE,
                               pageLength = 15
                ),
                selection = 'single',
                rownames = FALSE)
    }
    else {
      datatable(reactive$metadata_table_fastq,
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               server = TRUE,
                               paging = TRUE,
                               pageLength = 15
                ),
                selection = 'single',
                rownames = FALSE)
    }

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
                       shinyjs::hide("LoadBox_FASTQ1")
                       shinyjs::hide("LoadBox_FASTQ2")

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
      shinyalert("ERROR!", e$message, type = "error", confirmButtonCol = "#337ab7")
    })
  })

  output$ladder_table_example <- DT::renderDataTable({
    reactive$laddertable <- read.csv("data/default_ladderids.csv")

    datatable(reactive$laddertable,
              options = list(scrollX = TRUE,
                             scrollY = TRUE,
                             server = TRUE,
                             paging = TRUE,
                             pageLength = 15
              ),
              selection = 'single',
              rownames = FALSE)
  },  options = list(scrollX = TRUE))

  output$ladder_table <- DT::renderDataTable({
    validate(
      need(!is.null(reactive$laddertable), 'You must load your data first...'))
    reactive$laddertable
  },  options = list(scrollX = TRUE))

  return(list(
    All = reactive(reactive$df),
    fastq = reactive(reactive$df_final),
    laddertable = reactive(reactive$laddertable),
    fsa_list = reactive(reactive$fsa_list),
    metadata_table = reactive(reactive$metadata_table),
    metadata_table_fastq = reactive(reactive$metadata_table_fastq),
    DataUpload = reactive(input$DataUpload),
    DataUploadMeta = reactive(input$DataUploadMeta),
    Ladder_switch = reactive(input$Ladder_switch)
  ))
}


