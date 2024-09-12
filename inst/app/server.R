server <- function(input, output, session) {

  # Intro module ------------------------------------------------------------
  Intro(input = input, output = output, session = session)

  # Metrics module ---------------------------------------------------------------------
  continue_module <- continue_server(input = input, output = output, session = session)

  # upload data -------------------------------------------------------------
  upload_data <- upload_data_box_server(input = input, output = output, session = session, continue_module = continue_module)

  # ladder module ---------------------------------------------------------------------

  ladder_module <- ladder_server(input = input, output = output, session = session, continue_module = continue_module, upload_data = upload_data)

  # peaks module ---------------------------------------------------------------------

  peaks_module <- peaks_server(input = input, output = output, session = session, continue_module = continue_module, upload_data = upload_data, ladder_module = ladder_module)

  # Metrics module ---------------------------------------------------------------------

  metrics_server(input = input, output = output, session = session, continue_module = continue_module, upload_data = upload_data, ladder_module = ladder_module, peaks_module = peaks_module)

  # Help Menu ---------------------------------------------------------------
  output$sessionInfo <- renderPrint({
    print("Loaded Packages")
    sessionInfo()
  })

}
