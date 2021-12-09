 header <- dashboardHeader(title = div(img(src = 'A848_logo', height = "55px"),
                           style = "position: relative; margin: -3px 0px 0px -25px; display:left-align;"),
                           titleWidth=300,
leftUi = tagList(
  appButton(inputId = "hide", label = NULL, icon = icon("eye-slash")),
  bsTooltip(id='hide', 'Click to hide the header', placement = "bottom", trigger = "hover", options = NULL),
   bsTooltip(id='excel_fuzzy', 'Add current analysis to excel tab', placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip(id='xlsx', 'download xlsx', placement = "bottom", trigger = "hover", options = NULL),

                appButton(inputId = "excel_fuzzy", label = NULL, icon = icon("save")),
                downloadButton("xlsx", NULL,block = F, style = "simple", size="lg")),

 dropdownMenuOutput("taskMenu"),
 tags$li(a(href = 'http://www.swissmedic.ch', icon("home"), title = "Swissmedic Home"), class = "dropdown"),userOutput("user")
)
