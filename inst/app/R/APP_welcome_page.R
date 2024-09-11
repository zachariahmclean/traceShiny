###UI
welcome_message_ui <- function(id) {
  fluidRow(
  box(id = "intro", status = "warning", solidHeader = F, collapsible = T, width = 12,
      title = h1(HTML('<h2 style = "text-align:justify; margin-top:-50px; margin-bottom:-10px; ">Welcome to the TRACE')),

      h4(includeHTML("data/welcome_page/welcome_page.html")),
      br(),
      fluidRow(column(3,
                      valueBox("NEW", actionBttn("startbuttonintro", "START",
                                                 style = "jelly",
                                                 color = "primary"),
                               icon = icon("paper-plane"), width = 12, color = "aqua")),
               column(3,
                      valueBox("CONTINUE",
                               actionBttn("continue", "CONTINUE",
                                          style = "jelly",
                                          color = "primary"),
                               icon = icon("upload"), width = 12, color = "aqua"))
      ))
  )
}


Intro <- function(input, output, session) {
observeEvent(input$startbuttonintro, {
  withProgress(message = "Getting everything ready, won't take long...", style = "old",
               value = 0, {
                 incProgress(0.1)

                 incProgress(0.2, detail = "Loading Packages")

                 library(trace)

                 incProgress(0.8)

                 shinyjs::hide("NextButtonLoad", asis = T)

                 if(input$LoadBoxIntro$collapsed == TRUE) {
                   js$collapse("LoadBoxIntro")
                 }
                 shinyjs::show("startbuttonLoad")
                 shinyjs::hide(id = "LoadBox2")
                 shinyjs::hide(id = "LoadBox5")
                 shinyjs::hide(id = "LoadBox3")
                 shinyjs::hide(id = "LoadBox4")
                 shinyjs::hide(id = "NextButtonLoad")
                 updateRadioGroupButtons(session, "DataUpload", selected = "fsa")

                 output$dynamic_content <- renderMenu(sidebarMenu(id = "tabs",
                                                                  menuItem("Upload", icon = icon("spinner"), tabName = "Upload", selected = T,
                                                                           badgeColor = "green", badgeLabel = "new")
                 ))
               })
})
}
