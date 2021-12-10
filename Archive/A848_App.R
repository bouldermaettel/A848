rm(list = ls())
packagesToLoad <- c('shiny', 'shinythemes' ,'shinyWidgets', 'DT', 'tidyverse', 'shinydashboard', 'shinydashboardPlus',
                    'data.table', 'fresh','shinyjs', 'shinyBS', 'openxlsx')

# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )

addResourcePath('A848_logo', '../www/A848_logo.jpg')
addResourcePath('ProfilFoto', '../www/ProfilFoto.jpg')
addResourcePath('app.css', '../www/app.css')
          chooseSliderSkin("Flat", color = "#e00007")

source('../app_helper_files/radioTooltips.R')

ui <- function(request) {

  ############# Header
dashboardPage(skin='red-light',
dashboardHeader(title = div(img(src = 'A848_logo', height = "55px"),
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
),
### sidebar
dashboardSidebar(width = 300,
sidebarMenu(id='tabs',
conditionalPanel('input.hide > input.show',
appButton(inputId = "show", label = NULL, icon = icon("eye"))),
bsTooltip(id='show', 'Click to show the header', placement = "bottom", trigger = "hover",options = NULL),

menuItem("Historic", tabName = "hist", icon = icon("th")),
menuItem("New", icon = icon('line-chart'), tabName = "new",
badgeLabel = "new", badgeColor = "green"),
menuItem("Total", icon = icon('pushpin', lib = "glyphicon"), tabName = 'total',
badgeLabel = "total", badgeColor = "green"),
menuItem("Duplicates", icon = icon('pushpin', lib = "glyphicon"), tabName = 'dupl_names',
badgeLabel = "total", badgeColor = "green"),
menuItem("Fuzzy duplicates", icon = icon('pushpin', lib = "glyphicon"), tabName = 'fuzzy_dups',
badgeLabel = "total", badgeColor = "green"),
fileInput('file_input', 'Choose file with data to be loaded', accept = c('.csv','.xlsx')),
conditionalPanel("input.tabs != `hist`",
appButton(inputId = "transfer", label = "Transfer", icon = icon("save"))),
bsTooltip(id='transfer', 'Transfer new Data to database', placement = "bottom", trigger = "hover",options = NULL)
)),

#### body
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
  #### Tab hist
tabItem(tabName = "hist",
        box(title = h3("Choose columns to be shown"), width = 4,
  selectizeInput("columns_hist",label = NULL,
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname", "Strasse", "PLZ"),
                 multiple =T, options = NULL)),
  DTOutput("hist_data", width = '100%' )),

### Tab New
tabItem(tabName = "new",
        box(title = h3("Choose columns to be shown"), width = 4,
  selectizeInput("columns_new",label = NULL,
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname", "Strasse", "PLZ"),
                 multiple =T, options = NULL)),
  DTOutput("new_data", width = '100%' )),

  ### Tab total
tabItem(tabName = "total",
                               box(title = h3("Choose columns to be shown"), width = 4,
  selectizeInput("columns_tot",label = NULL,
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname", "Strasse", "PLZ"),
                 multiple =T, options = NULL)),
DTOutput("total_data", width = '100%' )),

  ### Tab duplicates names
tabItem(tabName = "dupl_names",
                       box(title = h3('Choose grouping vars for duplicate detection'), width = 4,
  selectizeInput("grouping_vars",label = NULL,
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname"),
                 multiple =T, options = NULL)),
           box(title = h3('Choose columns to be shown'), width = 4,
selectizeInput("columns_dupl",label = NULL,
               choices= c("Name", "Vorname", "Strasse", "PLZ"),
               selected = c("Name", "Vorname", "Strasse", "PLZ"),
               multiple =T, options = NULL)),
DTOutput("dupl_data", width = '100%' )),

    ### Tab fuzzy dups
tabItem(tabName = "fuzzy_dups",
                       box(title = h3('Choose grouping vars for fuzzy duplicate detection'), width = 4,
  selectizeInput("grouping_vars_fuzzy",label = NULL,
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname"),
                 multiple =T, options = NULL),
sliderInput('fuzzy', 'Define max allowed Levenshtein distance', min=0, max=0.3, value=0.05, step=0.01),
          appButton(inputId = "calc", label = "Calculate", icon = icon("box"))),
                               box(title = h3('Choose columns to be shown'), width = 4,
selectizeInput("columns_fuzzy",label = "Choose columns to be shown",
               choices= c("Name", "Vorname", "Strasse", "PLZ"),
               selected = c("Name", "Vorname", "Strasse", "PLZ"),
               multiple =T, options = NULL)),
DTOutput("dupl_fuzzy", width = '100%' )))),

### controlbar (empty at the moment)
dashboardControlbar(skin = "light", collapsed = TRUE, width = 250,
controlbarMenu(
id = "menu",
controlbarItem(
NULL,
chooseSliderSkin("Flat", color = "#e00007")
))))
}

server <- function(input, output, session){
variable <- reactiveValues()
data <- reactiveValues()
wb <- reactiveValues()
data$path <- '../data/Vereinfachtes_Verfahren_ab_2019.xlsx'

source('../src/data/data_wrangler.R')


observeEvent(input$hide, {
js$hidehead('none')
variable$head <- 'hidden'
})

observeEvent(input$show, {
js$hidehead('')
    variable$head <- 'not hidden'
})

# adapt the max size of file-upload (the def
options(shiny.maxRequestSize=50000*1024^2)

# load data from database
observe({
  data$hist <- tibble(get_data(path = data$path, sheet = 'Sendungen'))
  data$hist_show <- data$hist[,input$columns_hist]
})

    output$hist_data <- renderDT({
    data$hist_show %>%
       datatable( options = list(searching = T,pageLength=20, c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
                 filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
    })

# import new file
observe({
  req(input$file_input)
  inFile <- input$file_input
  ext <- substrRight(inFile$datapath, 4)
  if (ext == 'xlsx') {
  data$new <-  tibble(get_data(inFile$datapath,sheet='Sendungen' ))
  } else {
      data$new <-  tibble(get_data(inFile$datapath))
  }
  data$tot <- bind_rows(data$new, data$hist)
  data$new_show <- data$new[,input$columns_new]
})

  observe({
    req(data$tot)
    data$tot_show <- data$tot[,input$columns_tot]
  })

output$new_data <- renderDT({
  data$new_show %>%
  datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
           filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

output$total_data <- renderDT({
data$tot_show %>%
  datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
           filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

observe({
updateSelectizeInput(session, "columns_hist",
           choices= colnames(data$hist),
           selected = colnames(data$hist))

updateSelectizeInput(session, "columns_new",
           choices= colnames(data$new),
           selected = colnames(data$new))

updateSelectizeInput(session, "columns_tot",
           choices= colnames(data$tot),
           selected = colnames(data$tot))

updateSelectizeInput(session, "columns_dupl",
           choices= colnames(data$tot),
           selected = colnames(data$tot))

updateSelectizeInput(session, "columns_fuzzy",
         choices= colnames(data$tot),
         selected = colnames(data$tot))

updateSelectizeInput(session, "grouping_vars",
           choices= colnames(data$tot),
           selected = c('Name', 'Vorname'))

updateSelectizeInput(session, "grouping_vars_fuzzy",
           choices= colnames(data$tot),
           selected = c('Name', 'Vorname'))
})

# get exact duplicates
  observe({
    req(data$new, data$hist)
    data$dupl_names <- tibble(get_duplicate_records(first_df = data$new, second_df = data$hist, group_vars = input$grouping_vars))
    data$dupl_names_show <- data$dupl_names [,input$columns_dupl]
  })

  output$dupl_data <- renderDT({
data$dupl_names_show %>%
  datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
           filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

# get fuzzy duplicate records
observeEvent(input$calc, {
  req(data$new, data$hist)
  data$dupl_fuzzy <- tibble(get_fuzzy_duplicated_records(first_df = data$new, second_df = data$hist,
                                                         group_vars = input$grouping_vars_fuzzy, max_distance = input$fuzzy))
  data$dupl_fuzzy_show <- data$dupl_fuzzy [,input$columns_fuzzy]
})

  output$dupl_fuzzy <- renderDT({
data$dupl_fuzzy_show %>%
  datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
           filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

observeEvent(input$excel_fuzzy, {
    if (input$excel_fuzzy==1) {
wb[['fuzzy']] <- openxlsx::createWorkbook()
wb[['exact']] <- openxlsx::createWorkbook()
    }
  if (input$tabs == "fuzzy_dups") {
  groups <- paste0(input$grouping_vars_fuzzy, collapse='_')
  sheet_name <- paste0(input$excel_fuzzy,'fuzzy', groups, input$fuzzy, collapse='_')
  openxlsx::addWorksheet(wb[['fuzzy']], sheetName = sheet_name)
  openxlsx::writeData(wb[['fuzzy']], sheet = sheet_name, x = data$dupl_fuzzy_show, startCol = 1, startRow = 1)
  } else {
  groups <- paste0(input$grouping_vars, collapse='_')
  sheet_name <- paste0(input$excel_fuzzy,'exact', groups, collapse='_')
  openxlsx::addWorksheet(wb[['exact']], sheetName = sheet_name)
  openxlsx::writeData(wb[['exact']], sheet = sheet_name, x = data$dupl_names_show, startCol = 1, startRow = 1)
    }
})

  output$xlsx <- downloadHandler(
  filename = function() {
  if (input$tabs == "fuzzy_dups") {
    paste0('Fuzzy_analysis', ".xlsx")
  } else {
        paste0('Exact_analysis', ".xlsx")
    }
  },
  content = function(file) {
      if (input$tabs == "fuzzy_dups") {
    openxlsx::saveWorkbook(wb[["fuzzy"]], file = file, overwrite = TRUE)
    } else {
    openxlsx::saveWorkbook(wb[["exact"]], file = file, overwrite = TRUE)
      }
    }
)

  # save total data to excel (restricted to one click)
  observeEvent(input$transfer, {
    if (input$transfer == 1) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheetName = 'Sendungen')
    openxlsx::writeData(wb, sheet = 'Sendungen', x = data$tot, startCol = 1, startRow = 1)
    openxlsx::saveWorkbook(wb, file = data$path, overwrite = TRUE)
      }
  })

  observeEvent(input$file_input,{
                updateTabItems(session, 'tabs', selected = 'new')
  })

    output$user <- renderUser({
    dashboardUser(
    name = "Matthias Mueller",
    image = 'ProfilFoto',
    title = "Swissmedic 4.0",
    subtitle = "Data Scientist",
    footer = p('App creator', class = "text-center"),
    fluidRow(
    dashboardUserItem(
    width = 6,
    socialButton(
    href = "https://dropbox.com",
    icon = icon("dropbox")
      )),
    dashboardUserItem(
    width = 6,
    socialButton(
    href = "https://github.com",
    icon = icon("github")
          )
        )
      )
    )
  })

}

shinyApp(ui = ui, server = server)
