source('./app_helper_files/radioTooltips.R')

sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(id='tabs',

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

))



