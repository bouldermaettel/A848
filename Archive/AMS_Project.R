library(shiny)

ui <- fluidPage(

  titlePanel("Upload File"),

  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      comment("Max file size to upload is 5MB"),
      radioButtons("quote", "Analysis",
                   choices = c(All = "",
                               "Unique" = '"',
                               "Duplicates" = "'",
                               "Similiar Name Vorname" = "'"),
                   selected = '"'),
      downloadButton("download", "Download last Query"),
      tags$hr()
    ),
    
    mainPanel(
      
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  
  output$contents <- renderTable({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   quote = input$quote)

  })
  
}

shinyApp(ui, server)

