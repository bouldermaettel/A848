body <- dashboardBody(
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
tabItem(tabName = "../../data",
  DTOutput("../../data", width = '100%' )),

### Tab unique
tabItem(tabName = "unique",
  DTOutput("unique", width = '100%' )),

### Tab duplicates
tabItem(tabName = "duplicates",
  DTOutput("dupl", width = '100%' ))))
