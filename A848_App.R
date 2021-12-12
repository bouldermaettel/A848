rm(list = ls())
packagesToLoad <- c('shiny', 'shinythemes' ,'shinyWidgets', 'DT', 'tidyverse', 'shinydashboard', 'shinydashboardPlus',
                    'data.table', 'fresh','shinyjs', 'shinyBS', 'openxlsx')

# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )

addResourcePath('A848_logo', 'www/A848_logo.jpg')
addResourcePath('ProfilFoto', './www/ProfilFoto.jpg')
addResourcePath('app.css', './www/app.css')

source('./app_helper_files/radioTooltips.R')


ui <- function(request) {
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
 tags$li(a(href = 'http://www.swissmedic.ch', icon("home"), title = "Swissmedic Home"), class = "dropdown"),userOutput("user")
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

server <- function(input, output, session){
variable <- reactiveValues()
data <- reactiveValues()
wb <- reactiveValues()

source('./src/data/data_wrangler.R')

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
data$path <- './data/Vereinfachtes_Verfahren_ab_2019.xlsx'
observe({
  # data$hist <- tibble::tibble(get_data(path = data$path, sheet = 'Sendungen'))
    data$hist <- tibble::tibble(readRDS(file = "./data/historic_data.rds"))
})

# import new file
observe({
  req(input$file_input)
  inFile <- input$file_input
  ext <- substrRight(inFile$datapath, 4)
  if (ext == 'xlsx') {
    data$new <-  tibble(get_data(inFile$datapath, sheet='Sendungen' ))
  } else {
    data$new <-  tibble(get_data(inFile$datapath))
  }
    data$all <- bind_rows(data$new, data$hist)
})

    # choose output data for data tab
  observe({
    if (input$data_source == 'historic'){
      data$show <- data$hist[,input$columns]
    } else if (input$data_source == 'new') {
      data$show <- data$new[,input$columns]
    } else if (input$data_source == 'all' | input$data_source == 'historic & new') {
      data$show <- data$all[,input$columns]
    }
  })

observe({
updateSelectizeInput(session, "columns",
           choices= colnames(data$hist),
           selected = c('Name', 'Vorname'))

updateSelectizeInput(session, "grouping_vars",
           choices= colnames(data$hist),
           selected = c('Name', 'Vorname'))
})

  # get unique records
observe({
  req(data$hist)
  if (input$tabs == 'unique') {
  if (input$data_source == 'historic') {
      data$unique <- tibble(get_uniques(first_df = data$hist, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'new')  {
      data$unique <- tibble(get_uniques(first_df = data$new, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'all')  {
      data$unique <- tibble(get_uniques(first_df = data$all, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'historic & new')  {
  data$unique <- tibble(get_uniques(first_df = data$new, second_df = data$hist, as.list(input$grouping_vars)))
    }
  }
})

# get exact duplicates
observe({
  req(data$hist)
  if (input$calc_mode == 'exact') {
  if (input$data_source == 'historic') {
      data$dupl <- tibble(get_duplicate_records(first_df = data$hist, group_vars = input$grouping_vars))
  } else if (input$data_source == 'new')  {
      data$dupl <- tibble(get_duplicate_records(first_df = data$new, group_vars = input$grouping_vars))
  } else if (input$data_source == 'all')  {
      data$dupl <- tibble(get_duplicate_records(first_df = data$all,group_vars = input$grouping_vars))
  } else if (input$data_source == 'historic & new')  {
  data$dupl <- tibble(get_duplicate_records(first_df = data$new, second_df = data$hist, group_vars = input$grouping_vars))
    }
  }
})

# get fuzzy duplicate records
observeEvent(input$calc, {
  req(data$hist)
  if (input$data_source == 'historic') {
      data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$hist, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'new')  {
      data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$new, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'all')  {
      data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$all, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'historic & new')  {
  data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$new, second_df = data$hist,
                                               group_vars = input$grouping_vars, max_distance = input$fuzzy))
      }
})

# choose output data for dupl tab
observe({
  if (input$calc_mode == 'exact'){
    data$show_dupl <- data$dupl[,input$columns]
  } else {
    data$show_dupl <- data$dupl_fuzzy[,input$columns]
  }
})

observe({
  req(data$unique)
      data$show_unique <- data$unique[,input$columns]
})

observe({
  req(data$hist)
  if (input$data_source == 'historic'){
    data$show <- data$hist[,input$columns]
  } else if (input$data_source == 'new') {
    data$show <- data$new[,input$columns]
  } else if (input$data_source == 'all' | input$data_source == 'historic & new') {
    data$show <- data$all[,input$columns]
  }
})

output$data <- renderDT({
  data$show %>%
     datatable( options = list(searching = T,pageLength=20, c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
               filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

output$dupl <- renderDT({
  data$show_dupl %>%
    datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
             filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

  output$unique <- renderDT({
  data$show_unique %>%
    datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
             filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})


observeEvent(input$excel, {
    if (input$excel==1) {
wb[['duplicates']] <- openxlsx::createWorkbook()
    }
  if (input$tabs == 'duplicates') {
  if (input$calc_mode == "fuzzy") {
  groups <- paste0(input$grouping_vars, collapse='_')
  sheet_name <- substr(paste(input$excel,'fuzzy', groups, input$fuzzy, sep='_'),1,31)

  openxlsx::addWorksheet(wb[['duplicates']], sheetName = sheet_name)
  openxlsx::writeData(wb[['duplicates']], sheet = sheet_name, x = data$show_dupl, startCol = 1, startRow = 1)
  } else {
  groups <- paste0(input$grouping_vars, collapse='_')
      sheet_name <- substr(paste(input$excel,'exact', groups, sep='_'),1,31)
  openxlsx::addWorksheet(wb[['duplicates']], sheetName = sheet_name)
  openxlsx::writeData(wb[['duplicates']], sheet = sheet_name, x = data$show_dupl, startCol = 1, startRow = 1)
    }
  } else if (input$tabs == 'unique') {
  groups <- paste0(input$grouping_vars, collapse='_')
  sheet_name <- substr(paste(input$excel,'unique', groups, sep ='_'),1,31)
  openxlsx::addWorksheet(wb[['duplicates']], sheetName = sheet_name)
  openxlsx::writeData(wb[['duplicates']], sheet = sheet_name, x = data$show_unique, startCol = 1, startRow = 1)
  }
})

  output$xlsx <- downloadHandler(
  filename = function() {
    paste0('Duplicate_analysis', ".xlsx")
  },
  content = function(file) {
    openxlsx::saveWorkbook(wb[["duplicates"]], file = file, overwrite = TRUE)
  }
)

  # save total data to excel (restricted to one click)
  observeEvent(input$transfer, {
    if (input$transfer == 1) {
    # wb <- openxlsx::createWorkbook()
    # openxlsx::addWorksheet(wb, sheetName = 'Sendungen')
    # openxlsx::writeData(wb, sheet = 'Sendungen', x = data$all, startCol = 1, startRow = 1)
    # openxlsx::saveWorkbook(wb, file = data$path, overwrite = TRUE)
      saveRDS(data$all, file = "./data/historic_data.rds")
      }
  })

  observeEvent(input$file_input,{
shinyWidgets::updateAwesomeRadio(session, 'data_source', choices = c("historic", "new", 'all', "historic & new"),
                             selected = "new")
  })

    output$dirs <- renderText({
    paste(list.dirs('.', recursive=FALSE), collapse = ' | ')
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
