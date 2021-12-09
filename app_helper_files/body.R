body <- dashboardBody(
          # initialize shinyjs
          shinyjs::useShinyjs(),
          # add custom JS code
          extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }", functions = c('hidehead')),

  ### style the shiny notification according to the stylesheet
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),

  use_theme('bouldermaettel'),

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
 # appButton(inputId = "excel_fuzzy", label = "Save in Excel Tab", icon = icon("box"))),

DTOutput("dupl_fuzzy", width = '100%' )),


  # #### Tab NLP Playground masked word
  #         tabItem(tabName = "nlp_masked",
  #              box(title = h3('Masked word prediction'), width = 5,
  #   textAreaInput("nlp_masked_word", NULL, "Matthias Müller is a [MASK].",
  #             width = "1000px", height = '120px'),
  #  numericInput('kwords',h4("How many words (top k)"),value=10, min= 1, max=30, step=1, width='150px'),
  #   appButton(inputId = "predict_masked_word", label = h5("predict masked word"), icon = icon("line-chart")), # icon = icon("arrow-right", lib = "glyphicon")
  #             br(),
  #             br(),
  #                  column(width=4,
  # DTOutput('masked_word')))),

#### Tab NLP Playground masked word
          tabItem(tabName = "nlp_masked",
               box(title = h3('Masked word prediction'), width = 5,
    textAreaInput("nlp_masked_word", NULL, "Matthias Muller is a [MASK].",
              width = "1000px", height = '120px'),
   numericInput('kwords',h4("How many words (top k)"),value=10, min= 1, max=25, step=1, width='150px'),
    appButton(inputId = "predict_masked_word", label = h5("predict masked word"), icon = icon("line-chart"))), # icon = icon("arrow-right", lib = "glyphicon")
    box(title = NULL, width = 5,
                   # column(width=4,
  DTOutput('masked_word')))))




