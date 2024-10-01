library(shiny)
library(sendmailR)

# Test whether a given object is a valid non-empty list
# @param listname a potential list to verify
# returns TRUE if the given object is a non-empty list, FALSE otherwise
testList <- function(listname){
  return(!is.null(listname) &&
           length(listname) != 0 &&
           "list" %in% class(listname))
}


# A list of all the available storage types for shinyforms.
#' @export
STORAGE_TYPES <- list(
  FLATFILE = "flatfile"
)



# Adds a mandatory star to a labelled question.
# @param label A string representing the mandatory question.
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# shinyform app defined CSS format
appCSS <- "
.shinyforms-ui .mandatory_star { color: #db4437; font-size: 20px; line-height: 0; }
.shinyforms-ui .sf-questions { margin-bottom: -30px; }
.shinyforms-ui .sf-question { margin-top: 10px; font-size: 16px; }
.shinyforms-ui .question-hint { font-size: 14px; color: #737373; font-weight: normal; }
.shinyforms-ui .action-button.btn { font-size: 16px; margin-top: 20px; }
.shinyforms-ui .thankyou_msg { margin-top: 10px; }
.shinyforms-ui .showhide { margin-top: 10px; display: inline-block; }
.shinyforms-ui .sf_submit_msg { font-weight: bold; }
.shinyforms-ui .sf_error { margin-top: 15px; color: red; }
.shinyforms-ui .answers { margin-top: 25px; }
.shinyforms-ui .pw-box { margin-top: -20px; }
.shinyforms-ui .created-by { font-size: 12px; font-style: italic; color: #777; margin: 25px auto 10px;}
"

formUI <- function(formInfo) {
  if (!testList(formInfo)) {
    stop("`formInfo` is not a valid list")
  }

  ns <- NS(formInfo$id)

  questions <- formInfo$questions

  fieldsMandatory <- Filter(function(x) { !is.null(x$mandatory) && x$mandatory }, questions)
  fieldsMandatory <- unlist(lapply(fieldsMandatory, function(x) { x$id }))

  titleElement <- NULL
  if (!is.null(formInfo$name)) {
    titleElement <- h2(formInfo$name)
  }

  responseText <- "Thank you, your response was submitted successfully."
  if (!is.null(formInfo$responseText)) {
    responseText <- formInfo$responseText
  }

  div(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    class = "shinyforms-ui",
    div(
      id = ns("form"),
      titleElement,
      div(
        class = "sf-questions",
        lapply(
          questions,
          function(question) {
            label <- question$title
            if (question$id %in% fieldsMandatory) {
              label <- labelMandatory(label)
            }

            if (question$type == "text") {
              input <- textInput(ns(question$id), NULL, "")
            }
            else if (question$type == "special_text") {
              input <- textAreaInput(ns(question$id), NULL, "", rows = 5, resize = "both",
                                     placeholder = "Dear Zach and Andrew, we love the app!")
            }
            else if (question$type == "numeric") {
              input <- numericInput(ns(question$id), NULL, 0)
            } else if (question$type == "checkbox") {
              input <- checkboxInput(ns(question$id), label, FALSE)
            }

            div(
              class = "sf-question",
              if (question$type != "checkbox") {
                tags$label(
                  `for` = ns(question$id),
                  class = "sf-input-label",
                  label,
                  if (!is.null(question$hint)) {
                    div(class = "question-hint", question$hint)
                  }
                )
              },
              input
            )
          }
        )
      ),
      br(),
      actionBttn(ns("submit"), "Submit"),
      if (!is.null(formInfo$reset) && formInfo$reset) {
        actionBttn(ns("reset"), "Reset")
      },
      shinyjs::hidden(
        span(id = ns("submit_msg"),
             class = "sf_submit_msg",
             "Submitting..."),
        div(class = "sf_error", id = ns("error"),
            div(tags$b(icon("exclamation-circle"), "Error: "),
                span(id = ns("error_msg")))
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("thankyou_msg"),
        class = "thankyou_msg",
        strong(responseText), br(),
        actionLink(ns("submit_another"), "Submit another response")
      )
    ),
    shinyjs::hidden(
      actionLink(ns("showhide"),
                 class = "showhide",
                 "Show responses")
    )
  )
}

formServer <- function(formInfo) {
  if (!testList(formInfo)) {
    stop("`formInfo` is not a valid list")
  }
  callModule(formServerHelper, formInfo$id, formInfo)
}




# Helper function for formServer component
formServerHelper <- function(input, output, session, formInfo) {
  if (grepl("\\s", formInfo$id)) {
    stop("Form id cannot have any spaces", call. = FALSE)
  }

  if (formInfo$storage$type == STORAGE_TYPES$FLATFILE) {
    if (!dir.exists(formInfo$storage$path)) {
      dir.create(formInfo$storage$path, showWarnings = FALSE)
    }
  }

  questions <- formInfo$questions

  fieldsMandatory <- Filter(function(x) {!is.null(x$mandatory) && x$mandatory }, questions)
  fieldsMandatory <- unlist(lapply(fieldsMandatory, function(x) { x$id }))
  fieldsAll <- unlist(lapply(questions, function(x) { x$id }))

  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  observeEvent(input$reset, {
    shinyjs::reset("form")
    shinyjs::hide("error")
  })

  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {

    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    on.exit({
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })

    if (!is.null(formInfo$validations)) {
      errors <- unlist(lapply(
        formInfo$validations, function(validation) {
          if (!eval(parse(text = validation$condition))) {
            return(validation$message)
          } else {
            return()
          }
        }
      ))
      if (length(errors) > 0) {
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        if (length(errors) == 1) {
          shinyjs::html("error_msg", errors[1])
        } else {
          errors <- c("", errors)
          shinyjs::html("error_msg", paste(errors, collapse = "<br>&bull; "))
        }
        return()
      }
    }

    # Save the data (show an error message in case of error)
    tryCatch({
      # With authentication and SSL
      sendmail(from="traceshiny@gmail.com",
               to=c("traceshiny@gmail.com"),
               subject="traceShiny Query",
               msg=paste0(formData()),
               engine = "curl",
               engineopts = list(username = "traceshiny", password = "qoeu wmjg nmqu ltax"),
               control=list(smtpServer="smtp://smtp.gmail.com:587", verbose = TRUE)
      )

      sendmail(from="traceshiny@gmail.com",
               to=paste0(formData()[3]),
               subject="traceShiny Inquiry Recieved",
               msg="This is an automated email confirming that your query/comment about traceShiny app has been recieved.",
               engine = "curl",
               engineopts = list(username = "traceshiny", password = "qoeu wmjg nmqu ltax"),
               control=list(smtpServer="smtp://smtp.gmail.com:587", verbose = TRUE)
      )

      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::logjs(err)
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    })
  })

  if (!is.null(formInfo$multiple) && !formInfo$multiple) {
    submitMultiple <- FALSE
    shinyjs::hide("submit_another")
  } else {
    submitMultiple <- TRUE
  }
  observeEvent(input$submit_another, {
    if (!submitMultiple) {
      return()
    }
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })

  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = as.integer(Sys.time()))
    data <- t(data)
    data
  })
}
