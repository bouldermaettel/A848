
rm(list = ls())
packagesToLoad <- c('shiny', 'shinythemes' ,'shinyWidgets', 'shinyFiles',  'png', 'DT','xlsx',
                    'tidyverse', 'keys', 'parallel', 'shinydashboard', 'shinydashboardPlus', 'data.table', 'fresh',
                    'shinyjs', 'shinyBS', 'openxlsx')

# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )

addResourcePath('A848_logo', 'www/A848_logo.jpg')
addResourcePath('ProfilFoto', './www/ProfilFoto.jpg')
addResourcePath('app.css', './www/app.css')



source('./app_helper_files/sidebar.R')
source('./app_helper_files/body.R')
source('./app_helper_files/header.R')
source('./app_helper_files/controlbar.R')
source('./app_helper_files/radioTooltips.R')


ui <- function(request) {
  dashboardPage(skin='red-light',
  header,
  sidebar,
  body,
  controlbar
  )
}

server <- function(input, output, session){
    variable <- reactiveValues()
    data <- reactiveValues()
    wb <- reactiveValues()
    data$path <- './data/Vereinfachtes_Verfahren_ab_2019.xlsx'

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
})

observe({
  updateSelectizeInput(session, "columns_new",
             choices= colnames(data$new),
             selected = colnames(data$new))
})

observe({
updateSelectizeInput(session, "columns_tot",
           choices= colnames(data$tot),
           selected = colnames(data$tot))
})

  observe({
updateSelectizeInput(session, "columns_dupl",
           choices= colnames(data$tot),
           selected = colnames(data$tot))
})

  observe({
  updateSelectizeInput(session, "columns_fuzzy",
           choices= colnames(data$tot),
           selected = colnames(data$tot))
})

observe({
updateSelectizeInput(session, "grouping_vars",
           choices= colnames(data$tot),
           selected = c('Name', 'Vorname'))
})

  observe({
updateSelectizeInput(session, "grouping_vars_fuzzy",
           choices= colnames(data$tot),
           selected = c('Name', 'Vorname'))
})




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



  observe({
print(input$tabs)
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

  observeEvent(input$transfer, {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheetName = 'Sendungen')
    openxlsx::writeData(wb, sheet = 'Sendungen', x = data$tot, startCol = 1, startRow = 1)
    openxlsx::saveWorkbook(wb, file = data$path, overwrite = TRUE)
  })


    output$user <- renderUser({
    dashboardUser(
    name = "Matthias Mueller",
    image = 'ProfilFoto',
    title = "Swissmedic 4.0",
    subtitle = "Data Scientist",
    footer = p('Logged in', class = "text-center"),
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
