function(input, output, session){
variable <- reactiveValues()
data <- reactiveValues()
wb <- reactiveValues()

source('data_wrangler.R')

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
pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                    dbname="postgres",
                    host="db", port=5432,
                    user = 'root',
                    password = 'root')
#TODO: user input for filters (i.e. date filter)
res <- dbSendQuery(con, "SELECT * FROM historic_data")
data$hist <- tibble::tibble(dbFetch(res))
  DBI::dbDisconnect(con)
})

# import new file
observe({
    print(input$table_rows_selected)
  req(input$file_input)
  inFile <- input$file_input
  ext <- substrRight(inFile$datapath, 4)
  if (ext == 'xlsx') {
    data$new <-  tibble::tibble(get_data(inFile$datapath, sheet='Sendungen' ))
  } else {
    data$new <-  tibble::tibble(get_data(inFile$datapath))
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
           selected = colnames(data$hist))

updateSelectizeInput(session, "grouping_vars",
           choices= colnames(data$hist),
           selected = c('Name', 'Vorname'))
})

  # get unique records
observe({
  req(data$hist)
  if (input$tabs == 'unique') {
  if (input$data_source == 'historic') {
      data$unique <- tibble::tibble(get_uniques(first_df = data$hist, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'new')  {
      data$unique <- tibble::tibble(get_uniques(first_df = data$new, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'all')  {
      data$unique <- tibble::tibble(get_uniques(first_df = data$all, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'historic & new')  {
  data$unique <- tibble::tibble(get_uniques(first_df = data$new, second_df = data$hist, as.list(input$grouping_vars)))
    }
  }
})

# get exact duplicates
observe({
  req(data$hist)
  if (input$calc_mode == 'exact') {
  if (input$data_source == 'historic') {
      data$dupl <- tibble::tibble(get_duplicate_records(first_df = data$hist, group_vars = input$grouping_vars))
  } else if (input$data_source == 'new')  {
      data$dupl <- tibble::tibble(get_duplicate_records(first_df = data$new, group_vars = input$grouping_vars))
  } else if (input$data_source == 'all')  {
      data$dupl <- tibble::tibble(get_duplicate_records(first_df = data$all,group_vars = input$grouping_vars))
  } else if (input$data_source == 'historic & new')  {
  data$dupl <- tibble::tibble(get_duplicate_records(first_df = data$new, second_df = data$hist, group_vars = input$grouping_vars))
    }
  }
})

# get fuzzy duplicate records
observeEvent(input$calc, {
  req(data$hist)
  if (input$data_source == 'historic') {
      data$dupl_fuzzy <- tibble::tibble(get_fuzzy_duplicate_records(first_df = data$hist, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'new')  {
      data$dupl_fuzzy <- tibble::tibble(get_fuzzy_duplicate_records(first_df = data$new, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'all')  {
      data$dupl_fuzzy <- tibble::tibble(get_fuzzy_duplicate_records(first_df = data$all, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'historic & new')  {
  data$dupl_fuzzy <- tibble::tibble(get_fuzzy_duplicate_records(first_df = data$new, second_df = data$hist,
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
     DT::datatable( options = list(searching = T,pageLength=20, c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
               filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

output$dupl <- renderDT({
  data$show_dupl %>%
    DT::datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
             filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
})

  output$unique <- renderDT({
  data$show_unique %>%
    DT::datatable( options = list(searching = T,pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200), autoWidth = TRUE, scrollx=TRUE),
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
  sheet_name <- substr(paste(input$excel,input$tabs, groups, sep ='_'),1,31)
  openxlsx::addWorksheet(wb[['duplicates']], sheetName = sheet_name)
  openxlsx::writeData(wb[['duplicates']], sheet = sheet_name, x = data$show_unique, startCol = 1, startRow = 1)
  } else if (input$tabs == 'data') {
  sheet_name <- substr(paste(input$excel,input$tabs, input$data_source, sep="_"),1,31)
  openxlsx::addWorksheet(wb[['duplicates']], sheetName = sheet_name)
  openxlsx::writeData(wb[['duplicates']], sheet = sheet_name, x = data$show, startCol = 1, startRow = 1)
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
      pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                    dbname="postgres",
                    host="db", port=5432,
                    user = 'root',
                    password = 'root')

      DBI::dbWriteTable(con, 'historic_data', data$new,append=TRUE, row.names=FALSE)
      DBI::dbDisconnect(con)
    #   saveRDS(data$all, file = "./data/historic_data.rds")
      }
  })

  observeEvent(input$file_input,{
shinyWidgets::updateAwesomeRadio(session, 'data_source', choices = c("historic", "new", 'all', "historic & new"),
                             selected = "new")
  })

  output$dirs <- renderText({
    paste(list.dirs('.', recursive=FALSE), collapse = ' | ')
  })
print(paste(list.dirs('.', recursive=FALSE), collapse = ' | '))
    output$user <- renderUser({
    dashboardUser(
    name = "Matthias Mueller",
    image = 'ProfilFoto.jpg',
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
    href = "http//github.com",
    icon = icon("github")
          )
        )
      )
    )
  })

}

# anonymize data
# df <- readRDS('./Docker_container/app/data/historic_data.rds')
# df2[['Name']] <- randomNames::randomNames(100, which.names='first')
# df2[['Name']] <- randomNames::randomNames(100, which.names='last')
# df2[['Strasse']] <- paste(randomNames::randomNames(100, which.names='last'), sample.int(1:20, replace = T))

# library(magrittr)
#
# for (i in 1:1000) {
#   if (i == 1){
#     df_new <- df
#   } else {
#     df_new <-   dplyr::bind_rows(df, df_new)
#   }
# }
# head(df_new)
# df_new %>% dim()
#
# df_new <- readRDS('./data/historic_data.rds')
# df_new[['Vorname']] <- randomNames::randomNames(100000, which.names='first')
# df_new[['Name']] <- randomNames::randomNames(100000, which.names='last')
# df_new[['Strasse']] <- paste(randomNames::randomNames(100000, which.names='last'), sample.int(1:50, replace = T))
#
# saveRDS(df_new, './data/historic_data.rds')
#
# mock <- readxl::read_excel('./data/mock_data.xlsx')
# mock[['Vorname']] <- randomNames::randomNames(20, which.names='first')
# mock[['Name']] <- randomNames::randomNames(20, which.names='last')
# mock[['Strasse']] <- paste(randomNames::randomNames(20, which.names='last'), sample.int(1:50, replace = T))
#
# wb <- openxlsx::createWorkbook()
# openxlsx::addWorksheet(wb, sheetName = 'Sendungen')
# openxlsx::writeData(wb, sheet = 'Sendungen', x = mock, startCol = 1, startRow = 1)
# openxlsx::saveWorkbook(wb,'./data/mock_data_2.xlsx')