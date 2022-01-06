library(shiny)
library(rhandsontable)

ui <- fluidPage(
    tags$style(HTML(".datepicker {z-index:99999 !important;}")),

    helpText("Click this first. Date picker should work fine."),
    actionButton("show", "Show modal"),
    hr(),
    helpText("Now click below to render the handsontable. When showing the modal again, the date picker shows up behind the modal."),
    checkboxInput("showHot", "Show handsontable"),
    uiOutput("hot_rendered")
    )

server <- function(input, output, session) {
    # Modal:
    observeEvent(input$show, {
        showModal(
            modalDialog(
                title = "My Modal",
                dateInput("date", "Choose date")
            )
        )
    })
    # Handsontable:
    output$hot_rendered = renderUI({
        req(input$showHot==T)
        rHandsontableOutput("hot")
    })
    output$hot = renderRHandsontable({
        req(input$showHot==T)
        rhandsontable(head(iris))
    })
}

shinyApp(ui, server)