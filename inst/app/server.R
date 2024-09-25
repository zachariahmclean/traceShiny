###Form
basicInfoForm <- list(
  id = "basicinfo",
  questions = list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE,
         hint = "Please leave your name"),
    list(id = "institution", type = "text", title = "Affliated Institution/Company", mandatory = TRUE,
         hint = "Please leave your affliated institution"),
    list(id = "email", type = "text", title = "Email", mandatory = TRUE,
         hint = "Please leave your email so we can contact you"),
    list(id = "comments", type = "special_text", title = "Query/Comment",
         hint = "Please leave a message"),
    list(id = "terms", type = "checkbox", title = "All information you submit will be confidential and will only be visible to the maintainers of the app. I agree to these terms")
  ),
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses"
  ),
  name = "",
  password = "ditto",
  reset = TRUE,
  validations = list(
    list(condition = "nchar(input$name) >= 3",
         message = "Name must be at least 3 characters"),
    list(condition = "nchar(input$institution) >= 3",
         message = "Institution must be at least 3 characters"),
    list(condition = "grepl('@', input$email)",
         message = "Please enter a valid email address"),
    list(condition = "input$terms == TRUE",
         message = "You must agree to the terms")
  )
)

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

  metrics_module <- metrics_server(input = input, output = output, session = session, continue_module = continue_module, upload_data = upload_data, ladder_module = ladder_module, peaks_module = peaks_module)

  # Analysis module ---------------------------------------------------------------------

  analysis_server(input = input, output = output, session = session, continue_module = continue_module, upload_data = upload_data, ladder_module = ladder_module, peaks_module = peaks_module, metrics_module = metrics_module)

  # Help Menu ---------------------------------------------------------------
  output$sessionInfo <- renderPrint({
    print("Loaded Packages")
    sessionInfo()
  })

  # Google Form -------------------------------------------------------------
  formServer(basicInfoForm)

}
