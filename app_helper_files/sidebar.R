source('./app_helper_files/radioTooltips.R')

sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(id='tabs',
                      conditionalPanel('input.hide > input.show',
                                        appButton(inputId = "show", label = NULL, icon = icon("eye"))),
    bsTooltip(id='show', 'Click to show the header', placement = "bottom", trigger = "hover",options = NULL),
    menuItem("Data", tabName = "data", icon = icon("th")),
    menuItem("Unique", tabName = "unique",icon = icon('line-chart'),
              badgeLabel = "analysis", badgeColor = "green"),
    menuItem("Duplicates", icon = icon('pushpin', lib = "glyphicon"), tabName = 'duplicates',
             badgeLabel = "analysis", badgeColor = "green"),
  shinyWidgets::awesomeRadio('data_source', 'Choose data source', choices = c("historic"),
                             selected = "historic", status = "danger"),
                selectizeInput("columns",label = "Choose columns to be shown",
                 choices= c("Name", "Vorname", "Strasse", "PLZ"),
                 selected = c("Name", "Vorname", "Strasse", "PLZ"),
                 multiple =T, options = NULL),
       fileInput('file_input', 'Choose file with data to be loaded', accept = c('.csv','.xlsx')),
              conditionalPanel("input.tabs != `hist`",
              appButton(inputId = "transfer", label = "Transfer", icon = icon("save"))),
    bsTooltip(id='transfer', 'Transfer new Data to database', placement = "bottom", trigger = "hover",options = NULL)
))



