library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(dashboardthemes)
library(shinybusy)
library(shinycssloaders)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(shinyWidgets)
library(trace)

####GLOBAL ENVIROMENT####
# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=3)

#Java code
options(java.parameters = "-Xss2560k")

#Shinyjs box collapse code
jsboxcollapsecode <- "shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}"

#Inactivity timer
inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", 10800*1000, 10800, 10800*1000)

#Upload size restriction
options(shiny.maxRequestSize = 100000*1024^2)

sidebar <- dashboardSidebar(
  width = 250,

  #Use latest font-awesome icons
  tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),

  tags$style(HTML(
    ".pretty > div > label {font-size: 14px !important; line-height: 1px !important;}"
  )),

  tags$head(
    tags$style(HTML("

  .prevent_click{
  position:fixed;
  z-index:999;
  width:200%;
  height:200%;
  background-color: transparent;
  }"))
  ),

  conditionalPanel(
    condition = "$(\'html\').hasClass(\'shiny-busy\')",
    tags$div(class = "prevent_click")
  ),

  sidebarMenu(id = "orig_tabs",
              menuItem("Introduction", icon = icon("info"), tabName = "intro"),
              sidebarMenuOutput("dynamic_content"),
              menuItem("Help", icon = icon("question-circle"), tabName = "Help")
  )
)

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = jsboxcollapsecode, functions = c("collapse")),
  add_busy_spinner(spin = "fading-circle", position = "bottom-left", color = "#337ab7", height = "100px", width = "100px", margins = c(10,60)),
  tags$head(tags$style(HTML(".small-box {background-color: #e8ebe9 !important; color: #000000 !important; }"))),

  tags$style(
    HTML(
      ".main-footer {
            background-color: #37394a;
            color: white;
          }
          .main-footer a {
            color: white;
          }")
  ),

  tags$head(tags$style(HTML('
  .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:550px;
  }
  '))),

  tags$head(tags$style(
    "html {height: 100%; overflow-y: scroll;}"
  )),

  tags$head(
    tags$style(HTML("

  .prevent_click{
  position:fixed;
  z-index:999;
  width:200%;
  height:200%;
  background-color: transparent;
  }"))
  ),

  tags$script(inactivity),

  # display load spinner when shiny is busy
  conditionalPanel(
    condition = "$(\'html\').hasClass(\'shiny-busy\')",
    tags$div(class = "prevent_click")
  ),

  tags$head(
    tags$style(
      HTML("
      .shiny-input-container input[type=file] {
    overflow: hidden;
    max-width: 100%;
}

.shiny-progress-container {
    position: fixed;
    top: 0px;
    width: 100%;
    z-index: 2000;
}

.shiny-progress .progress {
    position: absolute;
    width: 100%;
    top: 0px;
    height: 17px;
    margin: 0px;
}

.shiny-progress .bar {
    background-color: #FF0000;
    .opacity = 0.8;
    transition-duration: 250ms;
}

.shiny-progress .progress-text {
    position: absolute;
    right: 30px;
    height: 40px;
    width: 1000px;
    background-color: #D9534F;
    padding-top: 2px;
    padding-right: 3px;
    padding-bottom: 2px;
    padding-left: 3px;
    opacity: 0.95;
    border-radius: 10px;
    -webkit-border-radius: 10px;
    -moz-border-radius: 10px;
    margin: 5px;
}

.progress-text {
    top: 15px !important;
    color: #FFFFFF !important;
    text-align: center;
}

/* Bold Initial part of message */
.shiny-progress .progress-text .progress-message {
    padding-top: 3px;
    padding-right: 3px;
    padding-bottom: 3px;
    padding-left: 10px;
    font-weight: bold;
    font-size: 23px;
}

/* Message detail */
.shiny-progress .progress-text .progress-detail {
    padding-top: 3px;
    padding-right: 3px;
    padding-bottom: 3px;
    padding-left: 3px;
    font-size: 22px;
}
            }
           "
      )
    )
  ),
  tabItems(

    ####TAB 0####
    tabItem(tabName = "intro",
            fluidRow(
              column(12,
              welcome_message_ui("fe")))
    ),
    tabItem(tabName = "Upload",
            fluidRow(upload_data_box_ui1("fe")),
            fluidRow(
              column(12,
                     upload_data_box_ui2("fe"),
                     upload_data_box_ui3("fe"),
                     upload_data_box_ui4("fe"),
                     upload_data_box_ui5("fe"),
                     p(style="text-align: center;", actionBttn("NextButtonLoad", "CONTINUE TO NEXT STEP...",
                                                               style = "jelly",
                                                               color = "primary",
                                                               icon = icon("play"), block = T))
              )
            )
    ),
    tabItem(tabName = "FindLadders",
            fluidRow(ladder_box_ui1("fe")),
            fluidRow(
              column(3,
                     ladder_box_ui2("fe"),
                     p(style="text-align: center;", actionBttn("NextButtonLadder", "CONTINUE TO NEXT STEP...",
                                                               style = "jelly",
                                                               color = "primary",
                                                               icon = icon("play"), block = T))),
              column(9,
                     ladder_box_ui3("fe")
              )
            )
    ),
    tabItem(tabName = "FindPeaks",
            fluidRow(peaks_box_ui1("fe")),
            fluidRow(
              column(3,
                     peaks_box_ui2("fe"),
                     p(style="text-align: center;", actionBttn("NextButtonPeaks", "CONTINUE TO NEXT STEP...",
                                                               style = "jelly",
                                                               color = "primary",
                                                               icon = icon("play"), block = T))),
              column(9,
                     peaks_box_ui3("fe")
              )
            ),
            fluidRow(
              column(12,
                     peaks_box_ui4("fe")))
    ),
    tabItem(tabName = "InstabilityMetrics",
            fluidRow(metrics_box_ui1("fe")),
            fluidRow(
              column(3,
                     metrics_box_ui2("fe"),
                     metrics_box_ui5("fe")),
              column(9,
                     metrics_box_ui4("fe"),
                     metrics_box_ui3("fe")
              )
            )
    ),

    ####HELP TAB####
    tabItem(tabName = "Help",

            box(id = "HelpBox", title = strong("Help"), status = "warning", solidHeader = F,
                collapsible = T, width = 12,

                includeHTML("data/help_tab/help.html")
            ),

            box(id = "LoadedPackagesBox", title = strong("Loaded Packages"), status = "warning", solidHeader = F,
                collapsible = T, width = 12,
                verbatimTextOutput("sessionInfo")
            )
    )

    )
  )

    ui <- function () {dashboardPage(
      dashboardHeader(
        title = img(id="Ditto",src="R.gif",width="30%"),
        tags$li(class = "dropdown", p(img(id="CGM", src="cfgm-logo-animated.gif", width="85%"))),
        titleWidth = 250),
      sidebar,
      body,
      md = TRUE,
      skin = "black",
      title = "Instability",
      footer = dashboardFooter(right = h6("Created by: Andrew Jiang & Zachariah Mclean", br(), "Github: zachariahmclean/instability", br(), "MGH Centre of Genomic Research", br(), "MGH & Harvard Medical School"),
                               left = p(img(id="CGM_logo", src="CGM-Logo.webp", width="8%", style = "margin-right: 30px"), img(id="MGH", src="MGHRI_2C_RGB_nobgrd.webp", width="10%", style = "margin-right: 30px"),
                                        img(id="Harvard", src="hms_logo_final_rgb_0-3.webp", width="10%")))
    )
    }
