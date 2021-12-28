rm(list = ls())
packagesToLoad <- c('RPostgreSQL', 'getPass','shiny', 'shinythemes' ,'shinyWidgets', 'DT', 'tidyverse', 'shinydashboard', 'shinydashboardPlus',
                    'data.table', 'fresh','shinyjs', 'shinyBS', 'openxlsx')

# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )

addResourcePath('A848_logo', 'www/A848_logo.jpg')
addResourcePath('ProfilFoto', 'www/ProfilFoto.jpg')
addResourcePath('app.css', 'www/app.css')

source('radioTooltips.R')


function(request) {
  dashboardPage(skin='red-light',
  dashboardHeader(title = div(img(src = 'A848_logo', height = "55px"),
                           style = "position: relative; margin: -3px 0px 0px -25px; display:left-align;"),
                           titleWidth=300,
leftUi = tagList(
  appButton(inputId = "hide", label = NULL, icon = icon("eye-slash")),
  bsTooltip(id='hide', 'Click to hide the header', placement = "bottom", trigger = "hover", options = NULL),
   bsTooltip(id='excel', 'Add current analysis to excel tab', placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip(id='xlsx', 'download xlsx', placement = "bottom", trigger = "hover", options = NULL),

                appButton(inputId = "excel", label = NULL, icon = icon("save")),
                downloadButton("xlsx", NULL,block = F, style = "simple", size="lg")),

 dropdownMenuOutput("taskMenu"),
 tags$li(a(href = 'http:/www.swissmedic.ch', icon("home"), title = "Swissmedic Home"), class = "dropdown"),userOutput("user")
),
  dashboardSidebar(width = 300,
  sidebarMenu(id='tabs',
                      conditionalPanel('input.hide > input.show',
                                        appButton(inputId = "show", label = NULL, icon = icon("eye"))),
    bsTooltip(id='show', 'Click to show the header', placement = "bottom", trigger = "hover",options = NULL),
    menuItem("Data", tabName = "data", icon = icon("th")),
    menuItem("Unique", tabName = "unique",icon = icon('line-chart'),
              badgeLabel = "analysis", badgeColor = "green"),
    menuItem("Duplicates", icon = icon('line-chart'), tabName = 'duplicates',
             badgeLabel = "analysis", badgeColor = "green"),
  shinyWidgets::awesomeRadio('data_source', 'Choose data source', choices = c("historic"),
                             selected = "historic", status = "danger"),
                selectizeInput("columns",label = "Choose columns to be shown",
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname", "Strasse", "PLZ"),
                 multiple =T, options = NULL),
       fileInput('file_input', 'Choose file with data to be loaded', accept = c('.csv','.xlsx')),
             conditionalPanel("input.tabs == 'duplicates' | input.tabs == 'unique'",
                selectizeInput("grouping_vars",label = 'Select vars for duplicates detection',
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname"),
                 multiple =T, options = NULL)),
conditionalPanel("input.tabs == 'duplicates'",
shinyWidgets::awesomeRadio('calc_mode', 'Choose calulation mode', choices = c("exact", 'fuzzy'),
                           selected = "exact", status = "danger"),
conditionalPanel('input.calc_mode == `fuzzy`',
            sliderInput('fuzzy', 'Define max allowed Levenshtein distance', min=0, max=0.3, value=0.05, step=0.01),
              appButton(inputId = "calc", label = "Calculate", icon = icon("box")))),
conditionalPanel("input.data_source != 'historic'",
              appButton(inputId = "transfer", label = "Transfer", icon = icon("save"))),
    bsTooltip(id='transfer', 'Transfer new Data to database', placement = "bottom", trigger = "hover",options = NULL)
))
,

#### Body
  dashboardBody(
  # initialize shinyjs
  shinyjs::useShinyjs(),
  # add custom JS code
  extendShinyjs(text = "shinyjs.hidehead = function(parm){
                            $('header').css('display', parm);
                        }", functions = c('hidehead')),

### style the shiny notification according to the stylesheet
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),

tabItems(
#### Tab data
tabItem(tabName = "data",
  DTOutput("data", width = '100%' )),

### Tab unique
tabItem(tabName = "unique",
  DTOutput("unique", width = '100%' )),

### Tab duplicates
tabItem(tabName = "duplicates",
  DTOutput("dupl", width = '100%' )))),
  dashboardControlbar(skin = "light", collapsed = TRUE, width = 250,
      controlbarMenu(
       id = "menu",
       controlbarItem(
        NULL,
          chooseSliderSkin("Flat", color = "#e00007"),
)))

  )
}